package com.lightning.walletapp.ln

import scala.util.{Success, Try}
import fr.acinq.bitcoin.{MilliSatoshi, Transaction}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}

import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.crypto.Sphinx._
import com.lightning.walletapp.ln.RoutingInfoTag._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.lnutils.JsonHttpUtils.to
import scodec.bits.ByteVector
import scodec.Attempt


object PaymentInfo {
  final val WAITING = 1
  final val SUCCESS = 2
  final val FAILURE = 3

  final val NOT_SENDABLE = 0
  final val SENDABLE_AIR = 1
  final val SENDABLE_MULTIPART = 2

  final val NOIMAGE = ByteVector.fromValidHex("3030303030303030")
  final val NOCHANID = ByteVector.fromValidHex("3131313131313131")
  final val REBALANCING = "Rebalancing"

  type FullOrEmptyRD = Either[RoutingData, RoutingData]
  type FailureTry = Try[DecryptedFailurePacket]
  type FailureTryVec = Vector[FailureTry]

  // Stores a history of error responses from peers per each outgoing payment request
  var errors = Map.empty[ByteVector, FailureTryVec] withDefaultValue Vector.empty
  private[this] var replacedChans = Set.empty[Long]

  def emptyRD(pr: PaymentRequest, firstMsat: Long, useCache: Boolean, airLeft: Int) =
    RoutingData(pr, routes = Vector.empty, usedRoute = Vector.empty, PacketAndSecrets(emptyOnionPacket, Vector.empty),
      firstMsat = firstMsat, lastMsat = 0L, lastExpiry = 0L, callsLeft = 4, useCache = useCache, airLeft = airLeft)

  def buildOnion(keys: PublicKeyVec, payloads: Vector[PerHopPayload], assoc: ByteVector): PacketAndSecrets = {
    require(keys.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    val encodedPayloads = for (rawPayload <- payloads) yield serialize(OnionCodecs.perHopPayloadCodec encode rawPayload)
    PaymentPacket.create(Tools.randomPrivKey, keys, encodedPayloads, assoc)
  }

  def useFirstRoute(rest: PaymentRouteVec, rd: RoutingData) = rest match {
    case firstRoute +: restOfRoutes => useRoute(firstRoute, restOfRoutes, rd)
    case _ => Left(rd)
  }

  def useRoute(route: PaymentRoute, rest: PaymentRouteVec, rd: RoutingData): FullOrEmptyRD = {
    // 9 + 1 block in case if block just appeared and there is a 1-block discrepancy between peers
    val firstExpiry = LNParams.broadcaster.currentHeight + rd.pr.adjustedMinFinalCltvExpiry
    val firstPayloadVector = PerHopPayload(0L, rd.firstMsat, firstExpiry) +: Vector.empty
    val start = (firstPayloadVector, Vector.empty[PublicKey], rd.firstMsat, firstExpiry)

    val (allPayloads, nodeIds, lastMsat, lastExpiry) = route.reverse.foldLeft(start) {
      case (loads, nodes, msat, expiry) \ Hop(nodeId, shortChannelId, delta, _, base, prop) =>
        // Walk in reverse direction from receiver to sender and accumulate cltv deltas with fees

        val nextFee = LNParams.hopFee(msat, base, prop)
        val nextPayload = PerHopPayload(shortChannelId, msat, expiry)
        (nextPayload +: loads, nodeId +: nodes, msat + nextFee, expiry + delta)
    }

    val isCltvBreach = lastExpiry - LNParams.broadcaster.currentHeight > LNParams.maxCltvDelta
    if (LNParams.isFeeBreach(route, rd.firstMsat) || isCltvBreach) useFirstRoute(rest, rd) else {
      val onion = buildOnion(keys = nodeIds :+ rd.pr.nodeId, payloads = allPayloads, assoc = rd.pr.paymentHash)
      val rd1 = rd.copy(routes = rest, usedRoute = route, onion = onion, lastMsat = lastMsat, lastExpiry = lastExpiry)
      Right(rd1)
    }
  }

  def without(routes: PaymentRouteVec, fun: Hop => Boolean) = routes.filterNot(routeOfHops => routeOfHops exists fun)
  def failIncorrectDetails(pkt: DecryptedPacket, msat: MilliSatoshi, add: UpdateAddHtlc) = failHtlc(pkt, IncorrectOrUnknownPaymentDetails(msat.amount), add)
  def failHtlc(pkt: DecryptedPacket, msg: FailureMessage, add: UpdateAddHtlc) = CMDFailHtlc(reason = FailurePacket.create(pkt.sharedSecret, msg), id = add.id)

  def withoutChan(shortId: Long, rd: RoutingData, span: Long, msat: Long) = {
    val routesWithoutBadChannels = without(rd.routes, _.shortChannelId == shortId)
    val blackListedChan = Tuple3(shortId.toString, span, msat)
    val rd1 = rd.copy(routes = routesWithoutBadChannels)
    Some(rd1) -> Vector(blackListedChan)
  }

  def withoutNodes(badNodes: PublicKeyVec, rd: RoutingData, span: Long) = {
    val routesWithoutBadNodes = without(rd.routes, badNodes contains _.nodeId)
    val blackListedNodes = for (node <- badNodes) yield (node.toString, span, 0L)
    val rd1 = rd.copy(routes = routesWithoutBadNodes)
    Some(rd1) -> blackListedNodes
  }

  def replaceRoute(rd: RoutingData, upd: ChannelUpdate) = {
    // In some cases we can just replace a faulty hop with a supplied one
    // but only do this once per each channel to avoid infinite loops
    val rd1 = rd.copy(routes = updateHop(rd, upd) +: rd.routes)
    // Prevent endless loop by marking this channel
    replacedChans += upd.shortChannelId
    Some(rd1) -> Vector.empty
  }

  def updateHop(rd: RoutingData, upd: ChannelUpdate) = rd.usedRoute map {
    case keepHop if keepHop.shortChannelId != upd.shortChannelId => keepHop
    case oldHop => upd.toHop(oldHop.nodeId)
  }

  def parseFailureCutRoutes(fail: UpdateFailHtlc)(rd: RoutingData) = {
    val parsed = FailurePacket.decrypt(fail.reason, rd.onion.sharedSecrets)
    errors = errors.updated(rd.pr.paymentHash, errors(rd.pr.paymentHash) :+ parsed)

    parsed map {
      case DecryptedFailurePacket(nodeKey, _: Perm) if nodeKey == rd.pr.nodeId => None -> Vector.empty
      case DecryptedFailurePacket(nodeKey, ExpiryTooFar) if nodeKey == rd.pr.nodeId => None -> Vector.empty
      case DecryptedFailurePacket(_, u: ExpiryTooSoon) if !replacedChans.contains(u.update.shortChannelId) => replaceRoute(rd, u.update)
      case DecryptedFailurePacket(_, u: FeeInsufficient) if !replacedChans.contains(u.update.shortChannelId) => replaceRoute(rd, u.update)
      case DecryptedFailurePacket(_, u: IncorrectCltvExpiry) if !replacedChans.contains(u.update.shortChannelId) => replaceRoute(rd, u.update)

      case DecryptedFailurePacket(nodeKey, u: Update) =>
        val isHonest = Announcements.checkSig(u.update, nodeKey)
        if (!isHonest) withoutNodes(Vector(nodeKey), rd, 86400 * 7 * 1000)
        else rd.usedRoute.collectFirst { case payHop if payHop.nodeId == nodeKey =>
          // A node along a payment route may choose a different channel than the one we have requested
          // if that happens it means our requested channel has not been used so we put it back here and retry it once again
          if (payHop.shortChannelId == u.update.shortChannelId) withoutChan(payHop.shortChannelId, rd, 180 * 1000, rd.firstMsat)
          else withoutChan(u.update.shortChannelId, rd.copy(routes = rd.usedRoute +: rd.routes), 180 * 1000, rd.firstMsat)
        } getOrElse withoutNodes(Vector(nodeKey), rd, 180 * 1000)

      case DecryptedFailurePacket(nodeKey, PermanentNodeFailure) => withoutNodes(Vector(nodeKey), rd, 86400 * 7 * 1000)
      case DecryptedFailurePacket(nodeKey, RequiredNodeFeatureMissing) => withoutNodes(Vector(nodeKey), rd, 86400 * 1000)
      case DecryptedFailurePacket(nodeKey, _: BadOnion) => withoutNodes(Vector(nodeKey), rd, 180 * 1000)

      case DecryptedFailurePacket(nodeKey, UnknownNextPeer | PermanentChannelFailure) =>
        rd.usedRoute.collectFirst { case payHop if payHop.nodeId == nodeKey =>
          withoutChan(payHop.shortChannelId, rd, 86400 * 7 * 1000, 0L)
        } getOrElse withoutNodes(Vector(nodeKey), rd, 180 * 1000)

      case DecryptedFailurePacket(nodeKey, _) =>
        rd.usedRoute.collectFirst { case payHop if payHop.nodeId == nodeKey =>
          withoutChan(payHop.shortChannelId, rd, 180 * 1000, rd.firstMsat)
        } getOrElse withoutNodes(Vector(nodeKey), rd, 180 * 1000)

    } getOrElse {
      val cut = rd.usedRoute drop 1 dropRight 1
      withoutNodes(cut.map(_.nodeId), rd, 60 * 1000)
    }
  }

  // Once mutually signed HTLCs are present we need to parse and fail/fulfill them
  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentInfoBag, loop: Boolean) =
    PaymentPacket.peel(nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket) match {
      case Left(badOnion) => CMDFailMalformedHtlc(add.id, badOnion.onionHash, FailureMessageCodecs failureCode badOnion)
      case Right(packet) => if (packet.isLastPacket) doResolve(packet, add, bag, loop) else failHtlc(packet, UnknownNextPeer, add)
    }

  def doResolve(pkt: DecryptedPacket, add: UpdateAddHtlc, bag: PaymentInfoBag, loop: Boolean) =
    Tuple2(OnionCodecs.perHopPayloadCodec decode pkt.payload.bits, bag getPaymentInfo add.paymentHash) match {
      case attempt \ Success(info) if attempt.isSuccessful && info.pr.msatOrMin > add.amount => failIncorrectDetails(pkt, info.pr.msatOrMin, add)
      case attempt \ Success(info) if attempt.isSuccessful && info.pr.msatOrMin * 2 < add.amount => failIncorrectDetails(pkt, info.pr.msatOrMin, add)
      case Attempt.Successful(payload) \ _ if payload.value.outgoingCltvValue != add.expiry => failHtlc(pkt, FinalIncorrectCltvExpiry(add.expiry), add)
      case attempt \ Success(info) if attempt.isSuccessful && !loop && info.incoming == 1 && info.status != SUCCESS => CMDFulfillHtlc(add, info.preimage)
      case attempt \ _ if attempt.isSuccessful => failIncorrectDetails(pkt, add.amount, add)
      case _ => InvalidOnionPayload(PaymentPacket hash add.onionRoutingPacket)
    }
}

case class RoutingData(pr: PaymentRequest, routes: PaymentRouteVec, usedRoute: PaymentRoute,
                       onion: PacketAndSecrets, firstMsat: Long /* amount without off-chain fee */,
                       lastMsat: Long /* amount with off-chain fee */, lastExpiry: Long, callsLeft: Int,
                       useCache: Boolean, airLeft: Int) {

  // Empty used route means we're sending to peer and its nodeId should be our targetId
  def nextNodeId(route: PaymentRoute) = route.headOption.map(_.nodeId) getOrElse pr.nodeId
  lazy val withMaxOffChainFeeAdded = firstMsat + LNParams.maxAcceptableFee(firstMsat, hops = 3)
  lazy val queryText = s"${pr.description} ${pr.nodeId.toString} ${pr.paymentHash.toHex}"
  lazy val isReflexive = pr.nodeId == LNParams.nodePublicKey
}

case class PaymentInfo(rawPr: String, preimage: ByteVector, incoming: Int, status: Int, stamp: Long,
                       description: String, firstMsat: Long, lastMsat: Long, lastExpiry: Long) {

  val firstSum = MilliSatoshi(firstMsat)
  // Incoming lastExpiry is 0, updated if reflexive
  val isLooper = incoming == 1 && lastExpiry != 0
  // Keep serialized for performance reasons
  lazy val pr = to[PaymentRequest](rawPr)
}

trait PaymentInfoBag { me =>
  def extractPreimage(tx: Transaction)
  def getPaymentInfo(hash: ByteVector): Try[PaymentInfo]
  def updStatus(paymentStatus: Int, hash: ByteVector)
  def updOkOutgoing(m: UpdateFulfillHtlc)
  def updOkIncoming(m: UpdateAddHtlc)
}