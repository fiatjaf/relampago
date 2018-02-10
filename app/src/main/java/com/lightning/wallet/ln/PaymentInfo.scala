package com.lightning.wallet.ln

import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.ln.wire.FailureMessageCodecs._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._

import scala.util.{Success, Try}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Transaction}
import com.lightning.wallet.ln.RoutingInfoTag.PaymentRoute
import com.lightning.wallet.lnutils.JsonHttpUtils.to
import com.lightning.wallet.ln.Tools.random
import scodec.bits.BitVector
import scodec.Attempt


object PaymentInfo {
  final val HIDDEN = 0
  final val WAITING = 1
  final val SUCCESS = 2
  final val FAILURE = 3

  // Placeholder for unresolved outgoing payments
  val NOIMAGE = BinaryData("00000000" getBytes "UTF-8")
  def buildOnion(keys: PublicKeyVec, payloads: Vector[PerHopPayload], assoc: BinaryData): SecretsAndPacket = {
    require(keys.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    makePacket(PrivateKey(random getBytes 32), keys, payloads.map(php => serialize(perHopPayloadCodec encode php).toArray), assoc)
  }

  def completeRPI(rpi: RuntimePaymentInfo): Option[RuntimePaymentInfo] =
    // Complete a first available payment route and check if it's fee is ok
    // blacklist outlier channels and try again if fee is not ok

    rpi.rd.routes match {
      case route +: rest =>
        val firstExpiry = LNParams.broadcaster.currentHeight + rpi.pr.minFinalCltvExpiry.getOrElse(9L)
        val firstPayload = PerHopPayload(shortChannelId = 0L, amtToForward = rpi.firstMsat, firstExpiry)
        val start = (Vector(firstPayload), Vector.empty[PublicKey], rpi.firstMsat, firstExpiry)

        val (allPayloads, nodeIds, lastMsat, lastExpiry) = route.reverse.foldLeft(start) {
          case (loads, nodes, msat, expiry) \ Hop(nodeId, shortChannelId, delta, _, base, prop) =>
            // Walk in reverse direction from receiver to sender and accumulate cltv deltas with fees

            val nextFee = msat + base + (prop * msat) / 1000000L
            val nextPayload = PerHopPayload(shortChannelId, msat, expiry)
            (nextPayload +: loads, nodeId +: nodes, nextFee, expiry + delta)
        }

        if (LNParams lnFeeNotOk lastMsat - rpi.firstMsat) completeRPI(rpi.modify(_.rd.routes) setTo rest) else {
          val onion = buildOnion(keys = nodeIds :+ rpi.pr.nodeId, payloads = allPayloads, assoc = rpi.pr.paymentHash)
          val rd1 = rpi.rd.copy(routes = rest, usedRoute = route, onion = onion, lastMsat = lastMsat, lastExpiry = lastExpiry)
          // We have a complete payment route at this point, can proceed with adding to a channel
          Some(rpi.modify(_.rd) setTo rd1)
        }

      // No more routes
      case _ => None
    }

  def emptyRD = {
    // Default RoutingData contains an empty route so it can be used to send funds to a peer right away
    val packet = Packet(Array(Version), random getBytes 33, random getBytes DataLength, random getBytes MacLength)
    RoutingData(Vector(Vector.empty), Vector.empty, Set.empty, Set.empty, SecretsAndPacket(Vector.empty, packet), 0L, 0L)
  }

  private def without(rs: Vector[PaymentRoute], fn: Hop => Boolean) = rs.filterNot(_ exists fn)
  private def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) = {
    // Will send an error onion packet which contains a detailed description back to payment sender
    val reason = createErrorPacket(sharedSecret, failure)
    CMDFailHtlc(add.id, reason)
  }

  def withoutNodes(bad: PublicKeyVec, rpi: RuntimePaymentInfo) = {
    val routesWithoutBadNodes = without(rpi.rd.routes, bad contains _.nodeId)
    val rd1 = rpi.rd.copy(routes = routesWithoutBadNodes, badNodes = rpi.rd.badNodes ++ bad)
    rpi.copy(rd = rd1)
  }

  def withoutChannels(shortChanIds: Vector[Long], rpi: RuntimePaymentInfo) = {
    val routesWithoutBadChans = without(rpi.rd.routes, shortChanIds contains _.shortChannelId)
    val rd1 = rpi.rd.copy(routes = routesWithoutBadChans, badChans = rpi.rd.badChans ++ shortChanIds)
    rpi.copy(rd = rd1)
  }

  def cutAffectedRoutes(fail: UpdateFailHtlc)(rpi: RuntimePaymentInfo) = {
    // Try to reduce remaining routes and also remember bad nodes and channels
    val parsed = Try apply parseErrorPacket(rpi.rd.onion.sharedSecrets, fail.reason)
    parsed.foreach(packet => Tools log packet.failureMessage.toString)

    parsed map {
      case ErrorPacket(nodeKey, _: Node) =>
        withoutNodes(Vector(nodeKey), rpi)

      case ErrorPacket(nodeKey, message: Update) =>
        val isHonest = Announcements.checkSig(message.update, nodeKey)
        if (isHonest) withoutChannels(Vector(message.update.shortChannelId), rpi)
        else withoutNodes(Vector(nodeKey), rpi)

      case ErrorPacket(nodeKey, _) =>
        rpi.rd.usedRoute.collectFirst {
          case hop if hop.nodeId == nodeKey =>
            // Try without this outgoing channel
            withoutChannels(Vector(hop.shortChannelId), rpi)
        } getOrElse withoutNodes(Vector(nodeKey), rpi)

    } getOrElse {
      // Except for our channel and peer's channel
      val shortChanIds = rpi.rd.usedRoute.map(_.shortChannelId)
      withoutChannels(shortChanIds drop 1 dropRight 1, rpi)
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
    val hash = sha256(add.onionRoutingPacket)
    CMDFailMalformedHtlc(add.id, hash, BADONION)
  }
}

case class PerHopPayload(shortChannelId: Long, amtToForward: Long, outgoingCltv: Long)
case class RoutingData(routes: Vector[PaymentRoute], usedRoute: PaymentRoute, badNodes: Set[PublicKey],
                       badChans: Set[Long], onion: SecretsAndPacket, lastMsat: Long, lastExpiry: Long)

// This class is constructed directly from database
// firstMsat is an amount I'm actually getting or an amount I'm paying without routing fees
// incoming firstMsat is updated on fulfilling, outgoing firstMsat is updated on pay attempt
case class PaymentInfo(rawRd: String, rawPr: String, preimage: BinaryData, incoming: Int,
                       firstMsat: Long, status: Int, stamp: Long, text: String) {

  def actualStatus = incoming match {
    // Once we have a preimage it is a SUCCESS
    // but only if this is an outgoing payment
    case 0 if preimage != NOIMAGE => SUCCESS
    // Incoming payment always has preimage
    // so we should always look at status
    case _ => status
  }

  // Keep these serialized for performance
  lazy val firstSum = MilliSatoshi(firstMsat)
  lazy val pr = to[PaymentRequest](rawPr)
  lazy val rd = to[RoutingData](rawRd)
}

case class RuntimePaymentInfo(rd: RoutingData, pr: PaymentRequest, firstMsat: Long) {
  lazy val text = pr.description match { case Right(info) => info case _ => new String }
  lazy val searchText = s"$text ${pr.paymentHash.toString}"
}

trait PaymentInfoBag { me =>
  def updateRouting(rpi: RuntimePaymentInfo)
  def updateStatus(status: Int, hash: BinaryData)
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]
  def updOkOutgoing(fulfill: UpdateFulfillHtlc)
  def updOkIncoming(add: UpdateAddHtlc)

  def extractPreimage(tx: Transaction) = tx.txIn.map(_.witness.stack) collect {
    case Seq(_, pre, _) if pre.size == 32 => me updOkOutgoing UpdateFulfillHtlc(null, 0L, pre)
    case Seq(_, _, _, pre, _) if pre.size == 32 => me updOkOutgoing UpdateFulfillHtlc(null, 0L, pre)
  }
}