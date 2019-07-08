package com.lightning.walletapp.ln.wire

import java.net._
import scodec.codecs._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.ln.{LightningException, PerHopPayload, RevocationInfo}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey, Scalar}
import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, Err}

import com.lightning.walletapp.ln.wire.Tlv.TlvSeq
import com.lightning.walletapp.ln.crypto.Sphinx
import org.apache.commons.codec.binary.Base32
import fr.acinq.bitcoin.Crypto
import fr.acinq.eclair.UInt64
import java.math.BigInteger
import scala.util.Try


object LightningMessageCodecs { me =>
  type NodeAddressList = List[NodeAddress]
  type BitVectorAttempt = Attempt[BitVector]
  type LNMessageVector = Vector[LightningMessage]
  type RedeemScriptAndSig = (ByteVector, ByteVector)
  type RGB = (Byte, Byte, Byte)

  def serialize(attempt: BitVectorAttempt): ByteVector = attempt match {
    case Attempt.Successful(binary) => ByteVector.view(binary.toByteArray)
    case Attempt.Failure(err) => throw new LightningException(err.message)
  }

  def deserialize(raw: ByteVector): LightningMessage =
    lightningMessageCodec decode BitVector(raw) match {
      case Attempt.Successful(decodedResult) => decodedResult.value
      case Attempt.Failure(err) => throw new LightningException(err.message)
    }

  def discriminatorWithDefault[A](discriminator: Codec[A], fallback: Codec[A]) = new Codec[A] {
    def encode(element: A) = discriminator encode element recoverWith { case _ => fallback encode element }
    def sizeBound = discriminator.sizeBound | fallback.sizeBound

    def decode(bv: BitVector) = discriminator decode bv recoverWith {
      case _: KnownDiscriminatorType[_]#UnknownDiscriminator => fallback decode bv
    }
  }

  // RGB <-> ByteVector
  private val bv2Rgb: PartialFunction[ByteVector, RGB] = {
    case ByteVector(red, green, blue, _*) => (red, green, blue)
  }

  private val rgb2Bv: PartialFunction[RGB, ByteVector] = {
    case (red, green, blue) => ByteVector(red, green, blue)
  }

  def der2wire(signature: ByteVector): ByteVector =
    Crypto decodeSignature signature match { case (r, s) =>
      val fixedR = Crypto fixSize ByteVector.view(r.toByteArray dropWhile 0.==)
      val fixedS = Crypto fixSize ByteVector.view(s.toByteArray dropWhile 0.==)
      fixedR ++ fixedS
    }

  def wire2der(sig: ByteVector): ByteVector = {
    val r = new BigInteger(1, sig.take(32).toArray)
    val s = new BigInteger(1, sig.takeRight(32).toArray)
    Crypto.encodeSignature(r, s) :+ 1.toByte
  }

  // Generic codecs

  val signature = Codec[ByteVector] (
    encoder = (der: ByteVector) => bytes(64).encode(me der2wire der),
    decoder = (wire: BitVector) => bytes(64).decode(wire).map(_ map wire2der)
  )

  val scalar = Codec[Scalar] (
    encoder = (scalar: Scalar) => bytes(32).encode(scalar.toBin),
    decoder = (wire: BitVector) => bytes(32).decode(wire).map(_ map Scalar.apply)
  )

  val point = Codec[Point] (
    encoder = (point: Point) => bytes(33).encode(point toBin true),
    decoder = (wire: BitVector) => bytes(33).decode(wire).map(_ map Point.apply)
  )

  val publicKey = Codec[PublicKey] (
    encoder = (publicKey: PublicKey) => bytes(33).encode(publicKey.value toBin true),
    decoder = (wire: BitVector) => bytes(33).decode(wire).map(_ map PublicKey.apply)
  )

  val ipv6address: Codec[Inet6Address] = bytes(16).exmap(
    bv => Attempt fromTry Try(Inet6Address.getByAddress(null, bv.toArray, null)),
    inet6Address => Attempt fromTry Try(ByteVector view inet6Address.getAddress)
  )

  private val ipv4address: Codec[Inet4Address] = bytes(4).xmap(
    bv => InetAddress.getByAddress(bv.toArray).asInstanceOf[Inet4Address],
    inet4Address => ByteVector.view(inet4Address.getAddress)
  )

  def base32(size: Int): Codec[String] = bytes(size).xmap(
    bv => (new Base32 encodeAsString bv.toArray).toLowerCase,
    address => ByteVector.view(new Base32 decode address.toUpperCase)
  )

  def uint64min(codec: Codec[UInt64], min: Long): Codec[UInt64] = codec.exmap(f = {
    case value if value < UInt64(min) => Attempt failure Err("Not minimally encoded")
    case value => Attempt successful value
  }, Attempt.successful)

  val uint64Overflow: Codec[Long] = int64.narrow(f = {
    case value if value < 0 => Attempt failure Err(s"Overflow $value")
    case value => Attempt successful value
  }, identity)

  val uint64L: Codec[UInt64] = bytes(8).xmap(bv => UInt64(bv.reverse), _.toByteVector.padLeft(8).reverse)
  val uint64: Codec[UInt64] = bytes(8).xmap(UInt64.apply, _.toByteVector padLeft 8)

  val varint: Codec[UInt64] = {
    val largeCodec = uint64min(uint64L, 0x100000000L)
    val middleCodec = uint64min(uint32L.xmap(long => UInt64(long), _.toBigInt.toLong), 0x10000)
    val smallCodec = uint64min(uint16L.xmap(int => UInt64(int), _.toBigInt.toInt), 0xFD)

    discriminatorWithDefault(
      discriminated[UInt64].by(uint8L)
        .\(0xFF) { case i if i >= UInt64(0x100000000L) => i } (largeCodec)
        .\(0xFE) { case i if i >= UInt64(0x10000) => i } (middleCodec)
        .\(0xFD) { case i if i >= UInt64(0xFD) => i } (smallCodec),
      uint8L.xmap(int => UInt64(int), _.toBigInt.toInt)
    )
  }

  val varintoverflow: Codec[Long] = varint.narrow(f = {
    case value if value > UInt64(Long.MaxValue) => Attempt failure Err(s"Overflow $value")
    case value => Attempt successful value.toBigInt.toLong
  }, UInt64.apply)

  val bytes32: Codec[ByteVector] = limitedSizeBytes(codec = bytesStrict(32).xmap(identity, identity), limit = 32)
  val zeropaddedstring: Codec[String] = fixedSizeBytes(32, utf8).xmap(_.takeWhile(_ != '\u0000'), identity)
  val varsizebinarydataLong: Codec[ByteVector] = variableSizeBytesLong(uint32, bytes)
  val varsizebinarydata: Codec[ByteVector] = variableSizeBytes(uint16, bytes)
  val rgb: Codec[RGB] = bytes(3).xmap(bv2Rgb, rgb2Bv)
  val txvec = vectorOfN(uint16, varsizebinarydata)

  def nodeaddress: Codec[NodeAddress] =
    discriminated[NodeAddress].by(uint8)
      .typecase(cr = (ipv4address :: uint16).as[IPv4], tag = 1)
      .typecase(cr = (ipv6address :: uint16).as[IPv6], tag = 2)
      .typecase(cr = (base32(10) :: uint16).as[Tor2], tag = 3)
      .typecase(cr = (base32(35) :: uint16).as[Tor3], tag = 4)
      .typecase(cr = (zeropaddedstring :: uint16).as[Domain], tag = 5)

  // Data formats

  private val init = (varsizebinarydata withContext "globalFeatures") :: (varsizebinarydata withContext "localFeatures")
  private val error = (bytes32 withContext "channelId") :: (varsizebinarydata withContext "data")
  private val ping = (uint16 withContext "pongLength") :: (varsizebinarydata withContext "data")
  private val pong = varsizebinarydata withContext "data"

  val channelReestablish =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "nextLocalCommitmentNumber") ::
      (uint64Overflow withContext "nextRemoteRevocationNumber") ::
      (optional(bitsRemaining, scalar) withContext "yourLastPerCommitmentSecret") ::
      (optional(bitsRemaining, point) withContext "myCurrentPerCommitmentPoint")

  val channelFlagsCodec =
    (byte withContext "flags").as[ChannelFlags]

  private val openChannel =
    (bytes32 withContext "chainHash") ::
      (bytes32 withContext "temporaryChannelId") ::
      (uint64Overflow withContext "fundingSatoshis") ::
      (uint64Overflow withContext "pushMsat") ::
      (uint64Overflow withContext "dustLimitSatoshis") ::
      (uint64 withContext "maxHtlcValueInFlightMsat") ::
      (uint64Overflow withContext "channelReserveSatoshis") ::
      (uint64Overflow withContext "htlcMinimumMsat") ::
      (uint32 withContext "feeratePerKw") ::
      (uint16 withContext "toSelfDelay") ::
      (uint16 withContext "maxAcceptedHtlcs") ::
      (publicKey withContext "fundingPubkey") ::
      (point withContext "revocationBasepoint") ::
      (point withContext "paymentBasepoint") ::
      (point withContext "delayedPaymentBasepoint") ::
      (point withContext "htlcBasepoint") ::
      (point withContext "firstPerCommitmentPoint") ::
      (channelFlagsCodec withContext "channelFlags")

  private val acceptChannel =
    (bytes32 withContext "temporaryChannelId") ::
      (uint64Overflow withContext "dustLimitSatoshis") ::
      (uint64 withContext "maxHtlcValueInFlightMsat") ::
      (uint64Overflow withContext "channelReserveSatoshis") ::
      (uint64Overflow withContext "htlcMinimumMsat") ::
      (uint32 withContext "minimumDepth") ::
      (uint16 withContext "toSelfDelay") ::
      (uint16 withContext "maxAcceptedHtlcs") ::
      (publicKey withContext "fundingPubkey") ::
      (point withContext "revocationBasepoint") ::
      (point withContext "paymentBasepoint") ::
      (point withContext "delayedPaymentBasepoint") ::
      (point withContext "htlcBasepoint") ::
      (point withContext "firstPerCommitmentPoint")

  val acceptChannelCodec =
    acceptChannel.as[AcceptChannel]

  private val fundingCreated =
    (bytes32 withContext "temporaryChannelId") ::
      (bytes32 withContext "txid") ::
      (uint16 withContext "fundingOutputIndex") ::
      (signature withContext "signature")

  private val fundingSigned =
    (bytes32 withContext "channelId") ::
      (signature withContext "signature")

  private val fundingLocked =
    (bytes32 withContext "channelId" ) ::
      (point withContext "nextPerCommitmentPoint")

  val fundingLockedCodec =
    fundingLocked.as[FundingLocked]

  private val shutdown =
    (bytes32 withContext "channelId") ::
      (varsizebinarydata withContext "scriptPubKey")

  val shutdownCodec =
    shutdown.as[Shutdown]

  private val closingSigned =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "feeSatoshis") ::
      (signature withContext "signature")

  val closingSignedCodec =
    closingSigned.as[ClosingSigned]

  private val updateAddHtlc =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (uint64Overflow withContext "amountMsat") ::
      (bytes32 withContext "paymentHash") ::
      (uint32 withContext "expiry") ::
      (bytes(Sphinx.PacketLength) withContext "onionRoutingPacket")

  val updateAddHtlcCodec =
    updateAddHtlc.as[UpdateAddHtlc]

  private val updateFulfillHtlc =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (bytes32 withContext "paymentPreimage")

  val updateFulfillHtlcCodec =
    updateFulfillHtlc.as[UpdateFulfillHtlc]

  private val updateFailHtlc =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (varsizebinarydata withContext "reason")

  val updateFailHtlcCodec =
    updateFailHtlc.as[UpdateFailHtlc]

  private val updateFailMalformedHtlc =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (bytes32 withContext "onionHash") ::
      (uint16 withContext "failureCode")

  private val commitSig =
    (bytes32 withContext "channelId") ::
      (signature withContext "signature") ::
      (listOfN(uint16, signature) withContext "htlcSignatures")

  val commitSigCodec =
    commitSig.as[CommitSig]

  private val revokeAndAck =
    (bytes32 withContext "channelId") ::
      (scalar withContext "perCommitmentSecret") ::
      (point withContext "nextPerCommitmentPoint")

  private val updateFee =
    (bytes32 withContext "channelId") ::
      (uint32 withContext "feeratePerKw")

  private val announcementSignatures =
    (bytes32 withContext "channelId") ::
      (int64 withContext "shortChannelId") ::
      (signature withContext "nodeSignature") ::
      (signature withContext "bitcoinSignature")

  val channelAnnouncementWitness =
    (varsizebinarydata withContext "features") ::
      (bytes32 withContext "chainHash") ::
      (int64 withContext "shortChannelId") ::
      (publicKey withContext "nodeId1") ::
      (publicKey withContext "nodeId2") ::
      (publicKey withContext "bitcoinKey1") ::
      (publicKey withContext "bitcoinKey2")

  private val channelAnnouncement =
    (signature withContext "nodeSignature1") ::
      (signature withContext "nodeSignature2") ::
      (signature withContext "bitcoinSignature1") ::
      (signature withContext "bitcoinSignature2") ::
      channelAnnouncementWitness

  val nodeAnnouncementWitness =
    (varsizebinarydata withContext "features") ::
      (uint32 withContext "timestamp") ::
      (publicKey withContext "nodeId") ::
      (rgb withContext "rgbColor") ::
      (zeropaddedstring withContext "alias") ::
      (variableSizeBytes(value = list(nodeaddress), size = uint16) withContext "addresses")

  val channelUpdateWitness =
    (bytes32 withContext "chainHash") ::
      (int64 withContext "shortChannelId") ::
      (uint32 withContext "timestamp") ::
      (byte withContext "messageFlags").flatPrepend { messageFlags =>
        (byte withContext "channelFlags" ) ::
          (uint16 withContext "cltvExpiryDelta") ::
          (uint64Overflow withContext "htlcMinimumMsat") ::
          (uint32 withContext "feeBaseMsat") ::
          (uint32 withContext "feeProportionalMillionths" ) ::
          (conditional(included = (messageFlags & 1) != 0, uint64Overflow) withContext "htlcMaximumMsat")
      }

  val nodeAnnouncementCodec = (signature.withContext("signature") :: nodeAnnouncementWitness).as[NodeAnnouncement]
  val channelUpdateCodec = (signature.withContext("signature") :: channelUpdateWitness).as[ChannelUpdate]

  val lightningMessageCodec =
    discriminated[LightningMessage].by(uint16)
      .typecase(cr = init.as[Init], tag = 16)
      .typecase(cr = error.as[Error], tag = 17)
      .typecase(cr = ping.as[Ping], tag = 18)
      .typecase(cr = pong.as[Pong], tag = 19)
      .typecase(cr = openChannel.as[OpenChannel], tag = 32)
      .typecase(cr = acceptChannelCodec, tag = 33)
      .typecase(cr = fundingCreated.as[FundingCreated], tag = 34)
      .typecase(cr = fundingSigned.as[FundingSigned], tag = 35)
      .typecase(cr = fundingLockedCodec, tag = 36)
      .typecase(cr = shutdownCodec, tag = 38)
      .typecase(cr = closingSignedCodec, tag = 39)
      .typecase(cr = updateAddHtlcCodec, tag = 128)
      .typecase(cr = updateFulfillHtlcCodec, tag = 130)
      .typecase(cr = updateFailHtlcCodec, tag = 131)
      .typecase(cr = commitSigCodec, tag = 132)
      .typecase(cr = revokeAndAck.as[RevokeAndAck], tag = 133)
      .typecase(cr = updateFee.as[UpdateFee], tag = 134)
      .typecase(cr = updateFailMalformedHtlc.as[UpdateFailMalformedHtlc], tag = 135)
      .typecase(cr = channelReestablish.as[ChannelReestablish], tag = 136)
      .typecase(cr = channelAnnouncement.as[ChannelAnnouncement], tag = 256)
      .typecase(cr = nodeAnnouncementCodec, tag = 257)
      .typecase(cr = channelUpdateCodec, tag = 258)
      .typecase(cr = announcementSignatures.as[AnnouncementSignatures], tag = 259)

  // Not in a spec

  val hopCodec = {
    (publicKey withContext "nodeId") ::
      (int64 withContext "shortChannelId") ::
      (uint16 withContext "cltvExpiryDelta") ::
      (uint64Overflow withContext "htlcMinimumMsat") ::
      (uint32 withContext "feeBaseMsat") ::
      (uint32 withContext "feeProportionalMillionths")
  }.as[Hop]

  val perHopPayloadCodec = {
    (constant(ByteVector fromByte 0) withContext "realm") ::
      (uint64Overflow withContext "shortChannelId") ::
      (uint64Overflow withContext "amtToForward") ::
      (uint32 withContext "outgoingCltv") ::
      (ignore(8 * 12) withContext "unusedWithV0VersionOnHeader")
  }.as[PerHopPayload]

  val revocationInfoCodec = {
    (listOfN(uint16, varsizebinarydata ~ signature) withContext "redeemScriptsToSigs") ::
      (optional(bool, signature) withContext "claimMainTxSig") ::
      (optional(bool, signature) withContext "claimPenaltyTxSig") ::
      (uint64Overflow withContext "feeRate") ::
      (uint64Overflow withContext "dustLimit") ::
      (varsizebinarydata withContext "finalScriptPubKey") ::
      (uint16 withContext "toSelfDelay") ::
      (publicKey withContext "localPubKey") ::
      (publicKey withContext "remoteRevocationPubkey") ::
      (publicKey withContext "remoteDelayedPaymentKey")
  }.as[RevocationInfo]

  val walletZygoteCodec = {
    (uint16 withContext "v") ::
      (varsizebinarydataLong withContext "db") ::
      (varsizebinarydataLong withContext "wallet") ::
      (varsizebinarydataLong withContext "chain")
  }.as[WalletZygote]

  val aesZygoteCodec = {
    (uint16 withContext "v") ::
      (varsizebinarydataLong withContext "iv") ::
      (varsizebinarydataLong withContext "ciphertext")
  }.as[AESZygote]

  val cerberusPayloadCodec = {
    (vectorOfN(uint16, aesZygoteCodec) withContext "payloads") ::
      (vectorOfN(uint16, zeropaddedstring) withContext "halfTxIds")
  }.as[CerberusPayload]
}

trait Tlv { val tag: UInt64 }
sealed trait OnionTlv extends Tlv
// Generic Tlv type we fallback to if we don't understand the tag
case class GenericTlv(tag: UInt64, value: ByteVector) extends Tlv

object Tlv { me =>
  type TlvSeq = Seq[Tlv]
  type TlvCodec = Codec[Tlv]
  type EitherTlv = Either[GenericTlv, Tlv]

  def tlvStream(codec: TlvCodec) =
    list(me tlvFallback codec).exmap(
      parsed => Attempt fromTry Try(TlvStream apply parsed),
      rawStream => Attempt.successful(rawStream.records.toList)
    ): Codec[TlvStream]

  def lengthPrefixedTlvStream(codec: TlvCodec): Codec[TlvStream] =
    variableSizeBytesLong(value = tlvStream(codec), size = varintoverflow)

  private def unpack(data: EitherTlv) = data match {
    case Left(genericTlvRecord) => genericTlvRecord
    case Right(knownTlvRecord) => knownTlvRecord
  }

  private def pack(data: Tlv) = data match {
    case genericTlv: GenericTlv => Left(genericTlv)
    case knownTlvRecord => Right(knownTlvRecord)
  }

  private def tlvFallback(codec: TlvCodec) = discriminatorFallback(genericTlvCodec, codec).xmap(unpack, pack)
  private val genericTlv = (varint withContext "tag") :: variableSizeBytesLong(varintoverflow, bytes)
  private val genericTlvCodec: Codec[GenericTlv] = genericTlv.as[GenericTlv]
}

case class TlvStream(records: TlvSeq) {
  private def checkUnknownEven(record: Tlv) = {
    val knownOrOdd = !record.isInstanceOf[GenericTlv] || record.tag.toBigInt % 2 != 0
    require(knownOrOdd, "Tlv streams must not contain unknown even tlv types")
    Some(record)
  }

  (Option.empty[Tlv] /: records) {
    case (Some(previousRecord), record) =>
      require(record.tag > previousRecord.tag, "Tlv records must be ordered by monotonically-increasing types")
      require(record.tag != previousRecord.tag, "Tlv streams must not contain duplicate records")
      checkUnknownEven(record)

    case (None, record) =>
      checkUnknownEven(record)
  }
} 