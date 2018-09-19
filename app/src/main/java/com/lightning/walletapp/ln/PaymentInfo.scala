package com.lightning.walletapp.ln

import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.crypto._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.ln.crypto.Sphinx._
import com.lightning.walletapp.ln.RoutingInfoTag._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.wire.FailureMessageCodecs._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.lnutils.JsonHttpUtils.to
import com.lightning.walletapp.ln.Tools.random
import scodec.bits.BitVector
import scodec.Attempt

import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Transaction}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import scala.util.{Success, Try}


object PaymentInfo {
  final val HIDDEN = 0
  final val WAITING = 1
  final val SUCCESS = 2
  final val FAILURE = 3
  final val FROZEN = 4

  final val NOIMAGE = BinaryData("3030303030303030")
  final val NOCHANID = BinaryData("3131313131313131")

  type FailureTry = Try[ErrorPacket]
  type FailureTryVec = Vector[FailureTry]
  type FullOrEmptyRD = Either[RoutingData, RoutingData]

  // Stores a history of error responses from peers per each outgoing payment request
  var errors = Map.empty[BinaryData, FailureTryVec] withDefaultValue Vector.empty
  private[this] var replacedChans = Set.empty[Long]

  def emptyRD(pr: PaymentRequest, firstMsat: Long, throughPeers: Set[PublicKey], useCache: Boolean) = {
    val emptyPacket = Packet(Array(Version), random getBytes 33, random getBytes DataLength, random getBytes MacLength)
    RoutingData(pr, routes = Vector.empty, usedRoute = Vector.empty, SecretsAndPacket(Vector.empty, emptyPacket), firstMsat,
      lastMsat = 0L, lastExpiry = 0L, callsLeft = 4, throughPeers, useCache)
  }

  def buildOnion(keys: PublicKeyVec, payloads: Vector[PerHopPayload], assoc: BinaryData): SecretsAndPacket = {
    require(keys.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    makePacket(PrivateKey(random getBytes 32), keys, payloads.map(php => serialize(perHopPayloadCodec encode php).toArray), assoc)
  }

  def useRoutesLeft(rd: RoutingData) = useFirstRoute(rd.routes, rd)
  def useFirstRoute(rest: PaymentRouteVec, rd: RoutingData) = rest match {
    case firstRoute +: restOfRoutes => useRoute(firstRoute, restOfRoutes, rd)
    case noRoutesLeft => Left(rd)
  }

  def useRoute(route: PaymentRoute, rest: PaymentRouteVec, rd: RoutingData): FullOrEmptyRD = {
    // 9 + 1 block in case if block just appeared and there is a 1-block discrepancy between peers
    val firstExpiry = LNParams.broadcaster.currentHeight + rd.pr.adjustedMinFinalCltvExpiry
    val firstPayloadVector = PerHopPayload(0L, rd.firstMsat, firstExpiry) +: Vector.empty
    val start = (firstPayloadVector, Vector.empty[PublicKey], rd.firstMsat, firstExpiry)

    val (allPayloads, nodeIds, lastMsat, lastExpiry) = route.reverse.foldLeft(start) {
      case (loads, nodes, msat, expiry) \ Hop(nodeId, shortChannelId, delta, _, base, prop) =>
        // Walk in reverse direction from receiver to sender and accumulate cltv deltas with fees

        val nextFee = msat + base + (prop * msat) / 1000000L
        val nextPayload = PerHopPayload(shortChannelId, msat, expiry)
        (nextPayload +: loads, nodeId +: nodes, nextFee, expiry + delta)
    }

    val cltvDeltaFail = lastExpiry - LNParams.broadcaster.currentHeight > LNParams.maxCltvDelta
    val lnFeeFail = LNParams.isFeeNotOk(lastMsat, lastMsat - rd.firstMsat, route.size)

    if (cltvDeltaFail || lnFeeFail) useFirstRoute(rest, rd) else {
      val onion = buildOnion(keys = nodeIds :+ rd.pr.nodeId, payloads = allPayloads, assoc = rd.pr.paymentHash)
      val rd1 = rd.copy(routes = rest, usedRoute = route, onion = onion, lastMsat = lastMsat, lastExpiry = lastExpiry)
      Right(rd1)
    }
  }

  def without(rs: PaymentRouteVec, fun: Hop => Boolean) = rs.filterNot(_ exists fun)
  def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) =
    CMDFailHtlc(reason = createErrorPacket(sharedSecret, failure), id = add.id)

  def withoutChan(chan: Long, rd: RoutingData, span: Long, msat: Long) = {
    val routesWithoutBadChannels = without(rd.routes, _.shortChannelId == chan)
    val blackListedChan = Tuple3(chan.toString, span, msat)
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

    val withReplacedHop = rd.usedRoute map { hop =>
      // Replace a single hop and return it otherwise unchanged
      val shouldReplace = hop.shortChannelId == upd.shortChannelId
      if (shouldReplace) upd toHop hop.nodeId else hop
    }

    // Put reconstructed route back, nothing to blacklist
    val rd1 = rd.copy(routes = withReplacedHop +: rd.routes)
    replacedChans += upd.shortChannelId
    Some(rd1) -> Vector.empty
  }

  def parseFailureCutRoutes(fail: UpdateFailHtlc)(rd: RoutingData) = {
    // Try to reduce remaining routes and also remember bad nodes and channels
    val parsed = Try apply parseErrorPacket(rd.onion.sharedSecrets, fail.reason)
    errors = errors.updated(rd.pr.paymentHash, errors(rd.pr.paymentHash) :+ parsed)

    parsed map {
      case ErrorPacket(nodeKey, _: Perm) if nodeKey == rd.pr.nodeId => None -> Vector.empty
      case ErrorPacket(nodeKey, ExpiryTooFar) if nodeKey == rd.pr.nodeId => None -> Vector.empty
      case ErrorPacket(nodeKey, u: ExpiryTooSoon) if !replacedChans.contains(u.update.shortChannelId) => replaceRoute(rd, u.update)
      case ErrorPacket(nodeKey, u: FeeInsufficient) if !replacedChans.contains(u.update.shortChannelId) => replaceRoute(rd, u.update)
      case ErrorPacket(nodeKey, u: IncorrectCltvExpiry) if !replacedChans.contains(u.update.shortChannelId) => replaceRoute(rd, u.update)

      case ErrorPacket(nodeKey, u: Update) =>
        val isHonest = Announcements.checkSig(u.update, nodeKey)
        if (!isHonest) withoutNodes(Vector(nodeKey), rd, 86400 * 7 * 1000)
        else rd.usedRoute.collectFirst { case payHop if payHop.nodeId == nodeKey =>
          // A node along a payment route may choose a different channel than the one we have requested
          // if that happens it means our requested channel has not been used so we put it back here and retry it once again
          if (payHop.shortChannelId == u.update.shortChannelId) withoutChan(u.update.shortChannelId, rd, 180 * 1000, rd.firstMsat)
          else withoutChan(u.update.shortChannelId, rd.copy(routes = rd.usedRoute +: rd.routes), 180 * 1000, rd.firstMsat)
        } getOrElse withoutNodes(Vector(nodeKey), rd, 180 * 1000)

      case ErrorPacket(nodeKey, PermanentNodeFailure) => withoutNodes(Vector(nodeKey), rd, 86400 * 7 * 1000)
      case ErrorPacket(nodeKey, RequiredNodeFeatureMissing) => withoutNodes(Vector(nodeKey), rd, 86400 * 1000)
      case ErrorPacket(nodeKey, _: BadOnion) => withoutNodes(Vector(nodeKey), rd, 180 * 1000)

      case ErrorPacket(nodeKey, UnknownNextPeer | PermanentChannelFailure) =>
        rd.usedRoute.collectFirst { case payHop if payHop.nodeId == nodeKey =>
          withoutChan(payHop.shortChannelId, rd, 86400 * 7 * 1000, 0L)
        } getOrElse withoutNodes(Vector(nodeKey), rd, 180 * 1000)

      case ErrorPacket(nodeKey, _) =>
        rd.usedRoute.collectFirst { case payHop if payHop.nodeId == nodeKey =>
          withoutChan(payHop.shortChannelId, rd, 180 * 1000, rd.firstMsat)
        } getOrElse withoutNodes(Vector(nodeKey), rd, 180 * 1000)

    } getOrElse {
      val cut = rd.usedRoute drop 1 dropRight 1
      withoutNodes(cut.map(_.nodeId), rd, 60 * 1000)
    }
  }

  // After mutually signed HTLCs are present we need to parse and fail/fulfill them
  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentInfoBag) = Try {
    val packet = parsePacket(privateKey = nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket)
    Tuple3(perHopPayloadCodec decode BitVector(packet.payload), packet.nextPacket, packet.sharedSecret)
  } map {
    // We are the final HTLC recipient, sanity checks first
    case (Attempt.Successful(decoded), nextPacket, sharedSecret)
      if nextPacket.isLast && decoded.value.outgoingCltv != add.expiry =>
      failHtlc(sharedSecret, FinalIncorrectCltvExpiry(add.expiry), add)

    case (Attempt.Successful(_), nextPacket, ss) if nextPacket.isLast => bag getPaymentInfo add.paymentHash match {
      // Payment request may not have a zero final sum which means it's a donation and should not be checked for overflow
      case Success(info) if info.pr.amount.exists(add.amountMsat > _.amount * 2) => failHtlc(ss, IncorrectPaymentAmount, add)
      case Success(info) if info.pr.amount.exists(add.amountMsat < _.amount) => failHtlc(ss, IncorrectPaymentAmount, add)
      case Success(info) if info.incoming == 1 && info.actualStatus != SUCCESS => CMDFulfillHtlc(add.id, info.preimage)
      case _ => failHtlc(ss, UnknownPaymentHash, add)
    }

    case (Attempt.Successful(_), _, sharedSecret) =>
      // We don't route so can't find the next node
      failHtlc(sharedSecret, UnknownNextPeer, add)

    case (Attempt.Failure(_), _, sharedSecret) =>
      // Payload could not be parsed at all so fail it
      failHtlc(sharedSecret, PermanentNodeFailure, add)

  } getOrElse {
    val hash = Crypto sha256 add.onionRoutingPacket
    CMDFailMalformedHtlc(add.id, hash, BADONION)
  }
}

case class PerHopPayload(shortChannelId: Long, amtToForward: Long, outgoingCltv: Long)
case class RoutingData(pr: PaymentRequest, routes: PaymentRouteVec, usedRoute: PaymentRoute,
                       onion: SecretsAndPacket, firstMsat: Long, lastMsat: Long, lastExpiry: Long,
                       callsLeft: Int, throughPeers: Set[PublicKey], useCache: Boolean) {

  // Allow users to search by payment description OR recipient nodeId OR payment hash
  lazy val queryText = s"${pr.description} ${pr.nodeId.toString} ${pr.paymentHash.toString}"
}

case class PaymentInfo(rawPr: String, preimage: BinaryData, incoming: Int, status: Int,
                       stamp: Long, description: String, hash: String, firstMsat: Long,
                       lastMsat: Long, lastExpiry: Long) {

  def actualStatus = incoming match {
    // Once we have a preimage it is a SUCCESS
    // but only if this is an outgoing payment
    case 0 if preimage != NOIMAGE => SUCCESS
    // Incoming payment always has preimage
    // so we should always look at status
    case _ => status
  }

  // Keep serialized for performance
  lazy val firstSum = MilliSatoshi(firstMsat)
  lazy val pr = to[PaymentRequest](rawPr)
}

trait PaymentInfoBag { me =>
  // Manage Revoked HTLC parameters
  type RevokedHashExpiry = (BinaryData, Long)
  def getAllRevoked(number: Long): Vector[RevokedHashExpiry]
  def saveRevoked(hash: BinaryData, expiry: Long, number: Long)

  // Manage payments list
  def extractPreimage(tx: Transaction)
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]
  def updStatus(paymentStatus: Int, hash: BinaryData)
  def updOkOutgoing(m: UpdateFulfillHtlc)
  def updOkIncoming(m: UpdateAddHtlc)
}