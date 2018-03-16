package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.RoutingInfoTag._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.ln.wire.FailureMessageCodecs._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._

import scala.util.{Success, Try}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Transaction}
import com.lightning.wallet.lnutils.JsonHttpUtils.to
import com.lightning.wallet.ln.Tools.random
import fr.acinq.bitcoin.Crypto
import scodec.bits.BitVector
import scodec.Attempt


object PaymentInfo {
  final val HIDDEN = 0
  final val WAITING = 1
  final val SUCCESS = 2
  final val FAILURE = 3
  final val FROZEN = 4

  final val TARGET_ALL = "ALL"
  final val TYPE_NODE = "NODE"
  final val TYPE_CHAN = "CHAN"

  final val NOIMAGE = BinaryData("00000000" getBytes "UTF-8")
  type FullOrEmptyRPI = Either[RuntimePaymentInfo, RuntimePaymentInfo]

  def emptyRD = {
    val emptyPacket = Packet(Array(Version), random getBytes 33, random getBytes DataLength, random getBytes MacLength)
    RoutingData(Vector.empty, Vector.empty, Set.empty, Set.empty, SecretsAndPacket(Vector.empty, emptyPacket), 0L, 0L)
  }

  def buildOnion(keys: PublicKeyVec, payloads: Vector[PerHopPayload], assoc: BinaryData): SecretsAndPacket = {
    require(keys.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    makePacket(PrivateKey(random getBytes 32), keys, payloads.map(php => serialize(perHopPayloadCodec encode php).toArray), assoc)
  }

  def useRoutesLeft(rpi: RuntimePaymentInfo) = useFirstRoute(rpi.rd.routes, rpi)
  def useFirstRoute(rest: PaymentRouteVec, rpi: RuntimePaymentInfo) = rest match {
    case firstCandidate +: restOfRoutes => useRoute(firstCandidate, restOfRoutes, rpi)
    case noRoutesLeft => Left(rpi)
  }

  def useRoute(route: PaymentRoute, rest: PaymentRouteVec, rpi: RuntimePaymentInfo): FullOrEmptyRPI = {
    val firstExpiry = LNParams.broadcaster.currentHeight + rpi.pr.minFinalCltvExpiry.getOrElse(default = 9L)
    val firstPayloadVector = PerHopPayload(shortChannelId = 0L, rpi.firstMsat, firstExpiry) +: Vector.empty
    val start = (firstPayloadVector, Vector.empty[PublicKey], rpi.firstMsat, firstExpiry)

    val (allPayloads, nodeIds, lastMsat, lastExpiry) = route.reverse.foldLeft(start) {
      case (loads, nodes, msat, expiry) \ Hop(nodeId, shortChannelId, delta, _, base, prop) =>
        // Walk in reverse direction from receiver to sender and accumulate cltv deltas with fees

        val nextFee = msat + base + (prop * msat) / 1000000L
        val nextPayload = PerHopPayload(shortChannelId, msat, expiry)
        (nextPayload +: loads, nodeId +: nodes, nextFee, expiry + delta)
    }

    //if (LNParams isFeeNotOk lastMsat - rpi.firstMsat) useFirstRoute(rest, rpi) else {
      val onion = buildOnion(keys = nodeIds :+ rpi.pr.nodeId, allPayloads, rpi.pr.paymentHash)
      val rd1 = RoutingData(rest, route, rpi.rd.badNodes, rpi.rd.badChans, onion, lastMsat, lastExpiry)
      val rpi1 = rpi.copy(rd = rd1)
      Right(rpi1)
    //}
  }

  def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) = {
    // Will send an error onion which contains a detailed description back to payment sender
    val reason = createErrorPacket(sharedSecret, failure)
    CMDFailHtlc(add.id, reason)
  }

  def without(rs: PaymentRouteVec, fn: Hop => Boolean) = rs.filterNot(_ exists fn)
  def withoutChans(bad: Vector[Long], rd: RoutingData, targetNodeId: String, span: Long) = {
    val toBan = for (shortChannelId <- bad) yield (shortChannelId, TYPE_CHAN, targetNodeId, span)
    val routesWithoutBadChans = without(rd.routes, bad contains _.shortChannelId)
    rd.copy(routes = routesWithoutBadChans) -> toBan
  }

  def withoutNodes(bad: PublicKeyVec, rd: RoutingData, span: Long) = {
    val toBan = for (nodeId <- bad) yield (nodeId, TYPE_NODE, TARGET_ALL, span)
    val routesWithoutBadNodes = without(rd.routes, bad contains _.nodeId)
    rd.copy(routes = routesWithoutBadNodes) -> toBan
  }

  def withoutChanOrNode(nodeKey: PublicKey, rd: RoutingData, chanSpan: Long, nodeSpan: Long) = rd.usedRoute.collectFirst {
    case hopData if hopData.nodeId == nodeKey => withoutChans(bad = Vector(hopData.shortChannelId), rd, TARGET_ALL, chanSpan)
  } getOrElse withoutNodes(bad = Vector(nodeKey), rd, nodeSpan)

  def parseFailureCutRoutes(fail: UpdateFailHtlc)(rd: RoutingData) = {
    // Try to reduce remaining routes and also remember bad nodes and channels
    val parsed = Try apply parseErrorPacket(rd.onion.sharedSecrets, fail.reason)

    parsed map {
      case ErrorPacket(nodeKey, cd: ChannelDisabled) =>
        val isNodeHonest = Announcements.checkSig(cd.update, nodeKey)
        if (!isNodeHonest) withoutNodes(bad = Vector(nodeKey), rd, 7200000)
        else withoutChans(Vector(cd.update.shortChannelId), rd, TARGET_ALL, 600000)

      case ErrorPacket(nodeKey, tf: TemporaryChannelFailure) =>
        val isNodeHonest = Announcements.checkSig(tf.update, nodeKey)
        if (!isNodeHonest) withoutNodes(bad = Vector(nodeKey), rd, 7200000)
        else withoutChans(Vector(tf.update.shortChannelId), rd,
          rd.usedRoute.last.nodeId.toString, 600000)

      case ErrorPacket(nodeKey, TemporaryNodeFailure) => withoutNodes(Vector(nodeKey), rd, 600000)
      case ErrorPacket(nodeKey, PermanentNodeFailure) => withoutNodes(Vector(nodeKey), rd, 7200000)
      case ErrorPacket(nodeKey, RequiredNodeFeatureMissing) => withoutNodes(Vector(nodeKey), rd, 7200000)
      case ErrorPacket(nKey, UnknownNextPeer) => withoutChanOrNode(nKey, rd, 7200000, 300000)
      case ErrorPacket(nKey, _) => withoutChanOrNode(nKey, rd, 300000, 300000)

    } getOrElse {
      val shortChanIds = rd.usedRoute.map(_.shortChannelId)
      withoutChans(shortChanIds drop 1 dropRight 1, rd,
        rd.usedRoute.last.nodeId.toString, 300000)
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
case class RoutingData(routes: PaymentRouteVec, usedRoute: PaymentRoute, badNodes: Set[PublicKey],
                       badChans: Set[Long], onion: SecretsAndPacket, lastMsat: Long, lastExpiry: Long)

case class RuntimePaymentInfo(rd: RoutingData, pr: PaymentRequest, firstMsat: Long) {
  // firstMsat is an amount I'm actually getting or an amount I'm paying without routing fees
  // incoming firstMsat is updated on fulfilling, outgoing firstMsat is updated on pay attempt
  def searchText = s"${pr.description} ${pr.nodeId} $paymentHashString"
  lazy val paymentHashString = pr.paymentHash.toString
}

// PaymentInfo is constructed directly from database
case class PaymentInfo(rawRd: String, rawPr: String, preimage: BinaryData,
                       incoming: Int, firstMsat: Long, status: Int, stamp: Long,
                       description: String, hash: String) {

  def actualStatus = incoming match {
    // Once we have a preimage it is a SUCCESS
    // but only if this is an outgoing payment
    case 0 if preimage != NOIMAGE => SUCCESS
    // Incoming payment always has preimage
    // so we should always look at status
    case _ => status
  }

  lazy val firstSum = MilliSatoshi(firstMsat)
  // Keep these serialized for performance
  lazy val pr = to[PaymentRequest](rawPr)
  lazy val rd = to[RoutingData](rawRd)
}

trait PaymentInfoBag { me =>
  def updateRouting(rpi: RuntimePaymentInfo): Unit
  def updateStatus(status: Int, hash: BinaryData): Unit
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]
  def updOkOutgoing(fulfill: UpdateFulfillHtlc): Unit
  def updOkIncoming(add: UpdateAddHtlc): Unit

  def extractPreimage(tx: Transaction) = tx.txIn.map(_.witness.stack) collect {
    case Seq(_, pre, _) if pre.size == 32 => me updOkOutgoing UpdateFulfillHtlc(null, 0L, pre)
    case Seq(_, _, _, pre, _) if pre.size == 32 => me updOkOutgoing UpdateFulfillHtlc(null, 0L, pre)
  }
}