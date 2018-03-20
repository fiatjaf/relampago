package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.RoutingInfoTag._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.ln.wire.FailureMessageCodecs._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import com.lightning.wallet.lnutils.JsonHttpUtils.to
import com.lightning.wallet.ln.Tools.random
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

  final val TARGET_ALL = "TYPE_ALL"
  final val TYPE_NODE = "TYPE_NODE"
  final val TYPE_CHAN = "TYPE_CHAN"

  final val NOIMAGE = BinaryData("00000000" getBytes "UTF-8")
  type FullOrEmptyRD = Either[RoutingData, RoutingData]

  def emptyRD(pr: PaymentRequest, firstMsat: Long) = {
    val emptyPacket = Packet(Array(Version), random getBytes 33, random getBytes DataLength, random getBytes MacLength)
    RoutingData(pr, Vector.empty, Vector.empty, SecretsAndPacket(Vector.empty, emptyPacket), firstMsat, 0L, 0L, 4, ok = true)
  }

  def emptyRDFromPR(pr: PaymentRequest) = emptyRD(pr, pr.unsafeMsat) // Automatic requests like storage tokens
  def emptyRDFromInfo(info: PaymentInfo) = emptyRD(info.pr, info.firstMsat) // Requests with option for custom amount

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
    val firstExpiry = LNParams.broadcaster.currentHeight + rd.pr.minFinalCltvExpiry.getOrElse(default = 9L)
    val firstPayloadVector = PerHopPayload(shortChannelId = 0L, rd.firstMsat, firstExpiry) +: Vector.empty
    val start = (firstPayloadVector, Vector.empty[PublicKey], rd.firstMsat, firstExpiry)

    val (allPayloads, nodeIds, lastMsat, lastExpiry) = route.reverse.foldLeft(start) {
      case (loads, nodes, msat, expiry) \ Hop(nodeId, shortChannelId, delta, _, base, prop) =>
        // Walk in reverse direction from receiver to sender and accumulate cltv deltas with fees

        val nextFee = msat + base + (prop * msat) / 1000000L
        val nextPayload = PerHopPayload(shortChannelId, msat, expiry)
        (nextPayload +: loads, nodeId +: nodes, nextFee, expiry + delta)
    }

    val cltvDeltaFail = lastExpiry - firstExpiry > LNParams.maxCltvDelta
    val lnFeeFail = LNParams.isFeeNotOk(lastMsat, lastMsat - rd.firstMsat, route.size)

    if (cltvDeltaFail | lnFeeFail) useFirstRoute(rest, rd) else {
      val onion = buildOnion(keys = nodeIds :+ rd.pr.nodeId, allPayloads, assoc = rd.pr.paymentHash)
      val rd1 = rd.copy(routes = rest, usedRoute = route, onion = onion, lastMsat = lastMsat, lastExpiry = lastExpiry)
      Right(rd1)
    }
  }

  def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) = {
    // Will send an error onion which contains a detailed description back to payment sender
    val reason = createErrorPacket(sharedSecret, failure)
    CMDFailHtlc(add.id, reason)
  }

  def without(rs: PaymentRouteVec, fn: Hop => Boolean) = rs.filterNot(_ exists fn)
  def withoutChans(ids: Vector[Long], rd: RoutingData, targetId: String, span: Long) = {
    val toBan = for (shortChanId <- ids) yield Tuple3(Right apply shortChanId, targetId, span)
    val withoutBadChans = without(rd.routes, hop => ids contains hop.shortChannelId)
    rd.copy(routes = withoutBadChans) -> toBan
  }

  def withoutNode(badNode: PublicKey, rd: RoutingData, span: Long) = {
    val withoutBadNodes = without(rd.routes, hop => badNode == hop.nodeId)
    val toBan = Tuple3(Left apply badNode, TARGET_ALL, span)
    rd.copy(routes = withoutBadNodes) -> Vector(toBan)
  }

  def parseFailureCutRoutes(fail: UpdateFailHtlc)(rd: RoutingData) = {
    // Try to reduce remaining routes and also remember bad nodes and channels
    val parsed = Try apply parseErrorPacket(rd.onion.sharedSecrets, fail.reason)

    parsed map {
      case ErrorPacket(nodeKey, cd: ChannelDisabled) =>
        val isNodeHonest = Announcements.checkSig(cd.update, nodeKey)
        if (!isNodeHonest) withoutNode(badNode = nodeKey, rd, span = 86400 * 2 * 1000)
        else withoutChans(Vector(cd.update.shortChannelId), rd, TARGET_ALL, 600 * 1000)

      case ErrorPacket(nodeKey, tf: TemporaryChannelFailure) =>
        val isNodeHonest = Announcements.checkSig(tf.update, nodeKey)
        if (!isNodeHonest) withoutNode(badNode = nodeKey, rd, span = 86400 * 2 * 1000)
        else withoutChans(Vector(tf.update.shortChannelId), rd, rd.pr.nodeId.toString, 600 * 1000)

      case ErrorPacket(nodeKey, TemporaryNodeFailure) => withoutNode(nodeKey, rd, 600 * 1000)
      case ErrorPacket(nodeKey, PermanentNodeFailure) => withoutNode(nodeKey, rd, 86400 * 2 * 1000)
      case ErrorPacket(nodeKey, RequiredNodeFeatureMissing) => withoutNode(nodeKey, rd, 86400 * 1000)

      case ErrorPacket(nodeKey, UnknownNextPeer) =>
        rd.usedRoute.collectFirst { case hop if hop.nodeId == nodeKey =>
          // Node can not find a specified peer so we remove a failed channel
          withoutChans(Vector(hop.shortChannelId), rd, TARGET_ALL, 86400 * 2 * 1000)
        } getOrElse withoutNode(nodeKey, rd, 180 * 1000)

      case ErrorPacket(nKey, _) =>
        // Don't blacklist, halt a payment
        rd.copy(ok = false) -> Vector.empty

    } getOrElse {
      val shortChanIds = rd.usedRoute.map(_.shortChannelId)
      withoutChans(shortChanIds drop 1 dropRight 1, rd,
        rd.pr.nodeId.toString, 180 * 1000)
    }
  }

  // After mutually signed HTLCs are present we need to parse and fail/fulfill them
  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentInfoBag, minExpiry: Long) = Try {
    val packet = parsePacket(privateKey = nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket)
    Tuple3(perHopPayloadCodec decode BitVector(packet.payload), packet.nextPacket, packet.sharedSecret)
  } map {
    // We are the final HTLC recipient, sanity checks first
    case (Attempt.Successful(decoded), nextPacket, sharedSecret)
      if nextPacket.isLast && decoded.value.outgoingCltv != add.expiry =>
      failHtlc(sharedSecret, FinalIncorrectCltvExpiry(add.expiry), add)

    case (Attempt.Successful(_), nextPacket, sharedSecret)
      // We may not have enough time to enforce this on-chain
      if nextPacket.isLast && add.expiry < minExpiry =>
      failHtlc(sharedSecret, FinalExpiryTooSoon, add)

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
                       onion: SecretsAndPacket, firstMsat: Long, lastMsat: Long,
                       lastExpiry: Long, callsLeft: Int, ok: Boolean) {

  lazy val qryText = s"${pr.description} $paymentHashString"
  lazy val paymentHashString = pr.paymentHash.toString
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
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]
  def updateStatus(paymentStatus: Int, hash: BinaryData)
  def updOkOutgoing(fulfill: UpdateFulfillHtlc)
  def updOkIncoming(add: UpdateAddHtlc)
  def extractPreimg(tx: Transaction)
}