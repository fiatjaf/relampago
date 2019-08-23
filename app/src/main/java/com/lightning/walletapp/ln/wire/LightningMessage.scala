package com.lightning.walletapp.ln.wire

import com.lightning.walletapp.ln._
import com.softwaremill.quicklens._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._

import fr.acinq.bitcoin.{Crypto, MilliSatoshi, Satoshi}
import fr.acinq.bitcoin.Crypto.{Point, PrivateKey, PublicKey, Scalar}
import java.net.{Inet4Address, Inet6Address, InetAddress, InetSocketAddress}
import com.lightning.walletapp.lnutils.olympus.OlympusWrap.StringVec
import com.lightning.walletapp.ln.crypto.Sphinx
import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector


trait LightningMessage
trait RoutingMessage extends LightningMessage
trait ChannelSetupMessage extends LightningMessage
trait ChannelMessage extends LightningMessage { val channelId: ByteVector }
case class Init(globalFeatures: ByteVector, localFeatures: ByteVector) extends LightningMessage
case class Ping(pongLength: Int, data: ByteVector) extends LightningMessage
case class Pong(data: ByteVector) extends LightningMessage

// CHANNEL SETUP MESSAGES: open channels never get these

case class ChannelFlags(flags: Byte) {
  def isPublic = Features.isBitSet(0, flags)
  def isZeroConfSpendablePush = Features.isBitSet(3, flags)
}

case class OpenChannel(chainHash: ByteVector, temporaryChannelId: ByteVector, fundingSatoshis: Long, pushMsat: Long,
                       dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: UInt64, channelReserveSatoshis: Long, htlcMinimumMsat: Long,
                       feeratePerKw: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int, fundingPubkey: PublicKey, revocationBasepoint: Point,
                       paymentBasepoint: Point, delayedPaymentBasepoint: Point, htlcBasepoint: Point, firstPerCommitmentPoint: Point,
                       channelFlags: ChannelFlags) extends ChannelSetupMessage

case class AcceptChannel(temporaryChannelId: ByteVector, dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: UInt64,
                         channelReserveSatoshis: Long, htlcMinimumMsat: Long, minimumDepth: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int,
                         fundingPubkey: PublicKey, revocationBasepoint: Point, paymentBasepoint: Point, delayedPaymentBasepoint: Point,
                         htlcBasepoint: Point, firstPerCommitmentPoint: Point) extends ChannelSetupMessage {

  lazy val dustLimitSat = Satoshi(dustLimitSatoshis)
}

case class FundingCreated(temporaryChannelId: ByteVector,
                          fundingTxid: ByteVector, fundingOutputIndex: Int,
                          signature: ByteVector) extends ChannelSetupMessage

case class FundingSigned(channelId: ByteVector, signature: ByteVector) extends ChannelSetupMessage

// CHANNEL MESSAGES

case class ClosingSigned(channelId: ByteVector, feeSatoshis: Long, signature: ByteVector) extends ChannelMessage
case class FundingLocked(channelId: ByteVector, nextPerCommitmentPoint: Point) extends ChannelMessage { me =>
  def some = Some(me)
}

case class Shutdown(channelId: ByteVector, scriptPubKey: ByteVector) extends ChannelMessage { me =>
  def some = Some(me)
}

case class UpdateAddHtlc(channelId: ByteVector,
                         id: Long, amountMsat: Long, paymentHash: ByteVector, expiry: Long,
                         onionRoutingPacket: OnionRoutingPacket) extends ChannelMessage {

  lazy val hash160: ByteVector = Crypto ripemd160 paymentHash
  lazy val amount: MilliSatoshi = MilliSatoshi(amountMsat)
}

case class UpdateFailHtlc(channelId: ByteVector, id: Long, reason: ByteVector) extends ChannelMessage
case class UpdateFailMalformedHtlc(channelId: ByteVector, id: Long, onionHash: ByteVector, failureCode: Int) extends ChannelMessage
case class UpdateFulfillHtlc(channelId: ByteVector, id: Long, paymentPreimage: ByteVector) extends ChannelMessage {

  val paymentHash = Crypto sha256 paymentPreimage
}

case class UpdateFee(channelId: ByteVector, feeratePerKw: Long) extends ChannelMessage
case class CommitSig(channelId: ByteVector, signature: ByteVector, htlcSignatures: List[ByteVector] = Nil) extends ChannelMessage
case class RevokeAndAck(channelId: ByteVector, perCommitmentSecret: Scalar, nextPerCommitmentPoint: Point) extends ChannelMessage

case class Error(channelId: ByteVector, data: ByteVector) extends ChannelMessage {
  def exception = new LightningException(text)
  lazy val text = bin2readable(data.toArray)
  lazy val tag = data.take(2)
}

case class ChannelReestablish(channelId: ByteVector, nextLocalCommitmentNumber: Long,
                              nextRemoteRevocationNumber: Long, yourLastPerCommitmentSecret: Option[Scalar],
                              myCurrentPerCommitmentPoint: Option[Point] = None) extends ChannelMessage

// ROUTING MESSAGES: open channels never get these except for ChannelUpdate

case class AnnouncementSignatures(channelId: ByteVector,
                                  shortChannelId: Long, nodeSignature: ByteVector,
                                  bitcoinSignature: ByteVector) extends RoutingMessage

case class ChannelAnnouncement(nodeSignature1: ByteVector, nodeSignature2: ByteVector, bitcoinSignature1: ByteVector,
                               bitcoinSignature2: ByteVector, features: ByteVector, chainHash: ByteVector, shortChannelId: Long,
                               nodeId1: PublicKey, nodeId2: PublicKey, bitcoinKey1: PublicKey, bitcoinKey2: PublicKey,
                               unknownFields: ByteVector = ByteVector.empty) extends RoutingMessage {

  val (blockHeight, txIndex, outputIndex) = fromShortId(shortChannelId)
  lazy val nodes = Set(nodeId1, nodeId2)
}

// PAYMENT ROUTE INFO

case class ChannelUpdate(signature: ByteVector, chainHash: ByteVector, shortChannelId: Long, timestamp: Long,
                         messageFlags: Byte, channelFlags: Byte, cltvExpiryDelta: Int, htlcMinimumMsat: Long,
                         feeBaseMsat: Long, feeProportionalMillionths: Long, htlcMaximumMsat: Option[Long],
                         unknownFields: ByteVector = ByteVector.empty) extends RoutingMessage {

  require(requirement = (messageFlags & 1) != 0 == htlcMaximumMsat.isDefined, "htlcMaximumMsat is not consistent with messageFlags")
  def toHop(nodeId: PublicKey) = Hop(nodeId, shortChannelId, cltvExpiryDelta, htlcMinimumMsat, feeBaseMsat, feeProportionalMillionths)
  lazy val isHosted = Tools.fromShortId(shortChannelId) match { case (blockHeight, _, _) => blockHeight <= LNParams.maxHostedBlockHeight }
  lazy val feeEstimate = feeBaseMsat + feeProportionalMillionths * 10
}

case class Hop(nodeId: PublicKey, shortChannelId: Long,
               cltvExpiryDelta: Int, htlcMinimumMsat: Long,
               feeBaseMsat: Long, feeProportionalMillionths: Long) {

  lazy val feeBreakdown = f"${feeProportionalMillionths / 10000D}%2f%% of payment sum + baseline $feeBaseMsat msat"
  lazy val humanDetails = s"Node ID: $nodeId, Channel ID: $shortChannelId, Expiry: $cltvExpiryDelta blocks, Routing fees: $feeBreakdown"
}

case class QueryChannelRange(chainHash: ByteVector, firstBlockNum: Long, numberOfBlocks: Long) extends RoutingMessage
case class GossipTimestampFilter(chainHash: ByteVector, firstTimestamp: Long, timestampRange: Long) extends RoutingMessage

// NODE ADDRESS HANDLING

case class NodeAnnouncement(signature: ByteVector, features: ByteVector, timestamp: Long,
                            nodeId: PublicKey, rgbColor: RGB, alias: String, addresses: NodeAddressList,
                            unknownFields: ByteVector = ByteVector.empty) extends RoutingMessage {

  val pretty = addresses collectFirst {
    case _: IPv4 | _: IPv6 => nodeId.toString take 15 grouped 3 mkString "\u0020"
    case _: Tor2 => s"<strong>Tor</strong>\u0020${nodeId.toString take 12 grouped 3 mkString "\u0020"}"
    case _: Tor3 => s"<strong>Tor</strong>\u0020${nodeId.toString take 12 grouped 3 mkString "\u0020"}"
  } getOrElse "No IP address"

  val identifier = (alias + nodeId.toString).toLowerCase
  val asString = s"<strong>${alias take 17}</strong><br><small>$pretty</small>"
  lazy val hostedChanId = Tools.hostedChanId(LNParams.nodePublicKey.toBin, nodeId.toBin)
}

sealed trait NodeAddress { def canBeUpdatedIfOffline: Boolean }
case object Padding extends NodeAddress { def canBeUpdatedIfOffline = false }

case class IPv4(ipv4: Inet4Address, port: Int) extends NodeAddress {
  override def toString: String = s"${ipv4.toString.tail}:$port"
  def canBeUpdatedIfOffline = true
}

case class IPv6(ipv6: Inet6Address, port: Int) extends NodeAddress {
  override def toString: String = s"${ipv6.toString.tail}:$port"
  def canBeUpdatedIfOffline = true
}

case class Tor2(tor2: String, port: Int) extends NodeAddress {
  override def toString: String = s"$tor2${NodeAddress.onionSuffix}:$port"
  def canBeUpdatedIfOffline = false
}

case class Tor3(tor3: String, port: Int) extends NodeAddress {
  override def toString: String = s"$tor3${NodeAddress.onionSuffix}:$port"
  def canBeUpdatedIfOffline = false
}

case class Domain(domain: String, port: Int) extends NodeAddress {
  override def toString: String = s"$domain:$port"
  def canBeUpdatedIfOffline = false
}

case object NodeAddress {
  val onionSuffix = ".onion"
  val V2Len = 16
  val V3Len = 56

  def toInetSocketAddress: PartialFunction[NodeAddress, InetSocketAddress] = {
    case Tor2(onionHost, port) => new InetSocketAddress(s"$onionHost$onionSuffix", port)
    case Tor3(onionHost, port) => new InetSocketAddress(s"$onionHost$onionSuffix", port)
    case IPv4(sockAddress, port) => new InetSocketAddress(sockAddress, port)
    case IPv6(sockAddress, port) => new InetSocketAddress(sockAddress, port)
    case Domain(site, port) => new InetSocketAddress(site, port)
  }

  def fromParts(host: String, port: Int, orElse: (String, Int) => NodeAddress = resolveIp) =
    if (host.endsWith(onionSuffix) && host.length == V2Len + onionSuffix.length) Tor2(host dropRight onionSuffix.length, port)
    else if (host.endsWith(onionSuffix) && host.length == V3Len + onionSuffix.length) Tor3(host dropRight onionSuffix.length, port)
    else orElse(host, port)

  def resolveIp(host: String, port: Int) = InetAddress getByName host match {
    case inetVersion4Address: Inet4Address => IPv4(ipv4 = inetVersion4Address, port)
    case inetVersion6Address: Inet6Address => IPv6(ipv6 = inetVersion6Address, port)
  }
}

// Hosted channel messages

trait HostedChannelMessage extends LightningMessage

case class InvokeHostedChannel(chainHash: ByteVector, scriptPubKey: ByteVector) extends HostedChannelMessage

case class InitHostedChannel(maxHtlcValueInFlightMsat: UInt64,
                             htlcMinimumMsat: Long, maxAcceptedHtlcs: Int, channelCapacityMsat: Long,
                             liabilityDeadlineBlockdays: Int, minimalOnchainRefundAmountSatoshis: Long,
                             initialClientBalanceMsat: Long) extends HostedChannelMessage

case class LastCrossSignedState(lastRefundScriptPubKey: ByteVector,
                                initHostedChannel: InitHostedChannel, lastLocalStateUpdate: StateUpdate,
                                lastRemoteStateUpdate: StateUpdate) extends HostedChannelMessage {

  def reverse =
    copy(lastLocalStateUpdate = lastRemoteStateUpdate,
      lastRemoteStateUpdate = lastLocalStateUpdate)

  def isBehind(remoteLCSS: LastCrossSignedState) =
    lastLocalStateUpdate.stateOverride.localUpdatesSoFar < remoteLCSS.lastRemoteStateUpdate.stateOverride.localUpdatesSoFar ||
      lastRemoteStateUpdate.stateOverride.localUpdatesSoFar < remoteLCSS.lastLocalStateUpdate.stateOverride.localUpdatesSoFar
}

case class StateOverride(updatedLocalBalanceMsat: Long,
                         blockDay: Long, localUpdatesSoFar: Long = 0L, remoteUpdatesSoFar: Long = 0L,
                         nodeSignature: ByteVector = ByteVector.empty) extends HostedChannelMessage

case class InFlightHtlc(incoming: Boolean, id: Long, amountMsat: Long, paymentHash: ByteVector, expiry: Long) { me =>
  def toAdd(hostedChanId: ByteVector) = UpdateAddHtlc(hostedChanId, id, amountMsat, paymentHash, expiry, Sphinx.emptyOnionPacket)
  def reverse = InFlightHtlc(incoming = !incoming, id, amountMsat, paymentHash, expiry)
  def toHtlc(chanId: ByteVector) = Htlc(incoming, me toAdd chanId)
}

case class StateUpdate(stateOverride: StateOverride, inFlightHtlcs: List[InFlightHtlc] = Nil) extends HostedChannelMessage { me =>
  def signed(channelId: ByteVector, refundScriptPubKey: ByteVector, init: InitHostedChannel, priv: PrivateKey): StateUpdate =
    me.modify(_.stateOverride.nodeSignature) setTo sign(hostedSigHash(channelId, refundScriptPubKey, me, init), priv)

  def verify(channelId: ByteVector, refundScriptPubKey: ByteVector, init: InitHostedChannel, pub: PublicKey): Boolean =
    Crypto.verifySignature(hostedSigHash(channelId, refundScriptPubKey, me, init), stateOverride.nodeSignature, pub)
}

// Not in a spec
case class OutRequest(sat: Long, badNodes: Set[String], badChans: Set[Long], from: Set[String], to: String)
case class WalletZygote(v: Int, db: ByteVector, wallet: ByteVector, chain: ByteVector)
case class CerberusPayload(payloads: Vector[AESZygote], halfTxIds: StringVec)
case class AESZygote(v: Int, iv: ByteVector, ciphertext: ByteVector)
case class GDriveBackup(chans: Vector[HasNormalCommits], v: Int)