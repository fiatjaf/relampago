package com.lightning.walletapp.ln.wire

import java.net._
import scodec.codecs._
import fr.acinq.eclair.UInt64.Conversions._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.ln.{LightningException, RevocationInfo}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Err}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey, Scalar}
import com.lightning.walletapp.ln.crypto.{Mac32, Sphinx}
import scodec.bits.{BitVector, ByteVector}

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

  def minimalValue(codec: Codec[UInt64], min: UInt64): Codec[UInt64] = codec.exmap(f = {
    case value if value < min => Attempt failure Err("Value is not minimally encoded")
    case value => Attempt successful value
  }, Attempt.successful)

  val uint64Overflow: Codec[Long] = int64.narrow(f = {
    case value if value < 0 => Attempt failure Err(s"Overflow $value")
    case value => Attempt successful value
  }, identity)

  val uint64: Codec[UInt64] = bytes(8).xmap(UInt64.apply, _.toByteVector padLeft 8)

  val varint: Codec[UInt64] = {
    val large = minimalValue(uint64, 0x100000000L)
    val medium = minimalValue(uint32.xmap(UInt64.apply, _.toBigInt.toLong), 0x10000)
    val small = minimalValue(uint16.xmap(int => UInt64(int), _.toBigInt.toInt), 0xFD)
    val default: Codec[UInt64] = uint8L.xmap(int => UInt64(int), _.toBigInt.toInt)

    discriminatorWithDefault(discriminated[UInt64].by(uint8L)
      .\(0xFF) { case value if value >= UInt64(0x100000000L) => value } (large)
      .\(0xFE) { case value if value >= UInt64(0x10000) => value } (medium)
      .\(0xFD) { case value if value >= UInt64(0xFD) => value } (small),
      default)
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

  def prependmac[A](codec: Codec[A], mac32: Mac32) = Codec[A] (
    (a: A) => codec.encode(a).map(bits1 => mac32.mac(bits1.toByteVector).bits ++ bits1),
    (encodedMacBits: BitVector) => (bytes32 withContext "mac").decode(encodedMacBits) match {
      case Attempt.Successful(dr) if mac32.verify(dr.value, dr.remainder.toByteVector) => codec.decode(dr.remainder)
      case Attempt.Successful(invalidDr) => Attempt Failure scodec.Err(s"Invalid mac detected: $invalidDr")
      case Attempt.Failure(err) => Attempt Failure err
    }
  )

  // Data formats

  private val init = (varsizebinarydata withContext "globalFeatures") :: (varsizebinarydata withContext "localFeatures")
  private val ping = (uint16 withContext "pongLength") :: (varsizebinarydata withContext "data")
  private val pong = varsizebinarydata withContext "data"

  val channelReestablish =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "nextLocalCommitmentNumber") ::
      (uint64Overflow withContext "nextRemoteRevocationNumber") ::
      (optional(bitsRemaining, scalar) withContext "yourLastPerCommitmentSecret") ::
      (optional(bitsRemaining, point) withContext "myCurrentPerCommitmentPoint")

  val channelFlagsCodec = (byte withContext "flags").as[ChannelFlags]

  val errorCodec = {
    (bytes32 withContext "channelId") ::
      (varsizebinarydata withContext "data")
  }.as[Error]

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

  val acceptChannelCodec = {
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
  }.as[AcceptChannel]

  private val fundingCreated =
    (bytes32 withContext "temporaryChannelId") ::
      (bytes32 withContext "txid") ::
      (uint16 withContext "fundingOutputIndex") ::
      (signature withContext "signature")

  private val fundingSigned =
    (bytes32 withContext "channelId") ::
      (signature withContext "signature")

  val fundingLockedCodec = {
    (bytes32 withContext "channelId") ::
      (point withContext "nextPerCommitmentPoint")
  }.as[FundingLocked]

  val shutdownCodec = {
    (bytes32 withContext "channelId") ::
      (varsizebinarydata withContext "scriptPubKey")
  }.as[Shutdown]

  val closingSignedCodec = {
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "feeSatoshis") ::
      (signature withContext "signature")
  }.as[ClosingSigned]

  val updateAddHtlcCodec = {
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (uint64Overflow withContext "amountMsat") ::
      (bytes32 withContext "paymentHash") ::
      (uint32 withContext "expiry") ::
      (OnionCodecs.paymentOnionPacketCodec withContext "onionRoutingPacket")
  }.as[UpdateAddHtlc]

  val updateFulfillHtlcCodec = {
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (bytes32 withContext "paymentPreimage")
  }.as[UpdateFulfillHtlc]

  val updateFailHtlcCodec = {
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (varsizebinarydata withContext "reason")
  }.as[UpdateFailHtlc]

  private val updateFailMalformedHtlc =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (bytes32 withContext "onionHash") ::
      (uint16 withContext "failureCode")

  val commitSigCodec = {
    (bytes32 withContext "channelId") ::
      (signature withContext "signature") ::
      (listOfN(uint16, signature) withContext "htlcSignatures")
  }.as[CommitSig]

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
      (publicKey withContext "bitcoinKey2") ::
      (bytes withContext "unknownFields")

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
      (variableSizeBytes(value = list(nodeaddress), size = uint16) withContext "addresses") ::
      (bytes withContext "unknownFields")

  val channelUpdateWitness =
    (bytes32 withContext "chainHash") ::
      (int64 withContext "shortChannelId") ::
      (uint32 withContext "timestamp") ::
      (byte withContext "messageFlags").flatPrepend { messageFlags =>
        (byte withContext "channelFlags" ) ::
          (uint16 withContext "cltvExpiryDelta") ::
          (uint64Overflow withContext "htlcMinimumMsat") ::
          (uint32 withContext "feeBaseMsat") ::
          (uint32 withContext "feeProportionalMillionths") ::
          (conditional(included = (messageFlags & 1) != 0, uint64Overflow) withContext "htlcMaximumMsat") ::
          (bytes withContext "unknownFields")
      }

  val nodeAnnouncementCodec = {
    (signature withContext "signature") ::
      nodeAnnouncementWitness
  }.as[NodeAnnouncement]

  val channelUpdateCodec = {
    (signature withContext "signature") ::
      channelUpdateWitness
  }.as[ChannelUpdate]

  val queryChannelRangeCodec = {
    ("chainHash" | bytes32) ::
      ("firstBlockNum" | uint32) ::
      ("numberOfBlocks" | uint32)
  }.as[QueryChannelRange]

  val gossipTimestampFilterCodec = {
    ("chainHash" | bytes32) ::
      ("firstTimestamp" | uint32) ::
      ("timestampRange" | uint32)
  }.as[GossipTimestampFilter]

  // Hosted messages codecs

  val stateOverrideCodec = {
    (uint64Overflow withContext "updatedLocalBalanceMsat") ::
      (uint32 withContext "blockDay") ::
      (uint32 withContext "localUpdatesSoFar") ::
      (uint32 withContext "remoteUpdatesSoFar") ::
      (signature withContext "nodeSignature")
  }.as[StateOverride]

  val inFlightHtlcCodec = {
    (bool withContext "incoming") ::
      (uint64Overflow withContext "id") ::
      (uint64Overflow withContext "amountMsat") ::
      (bytes32 withContext "paymentHash") ::
      (uint32 withContext "expiry")
  }.as[InFlightHtlc]

  val stateUpdateCodec = {
    (stateOverrideCodec withContext "stateOverride") ::
      (listOfN(uint16, inFlightHtlcCodec) withContext "inFlightHtlcs")
  }.as[StateUpdate]

  val invokeHostedChannelCodec = {
    (bytes32 withContext "chainHash") ::
      (varsizebinarydata withContext "scriptPubKey")
  }.as[InvokeHostedChannel]

  val initHostedChannelCodec = {
    (uint64 withContext "maxHtlcValueInFlightMsat") ::
      (uint64Overflow withContext "htlcMinimumMsat") ::
      (uint16 withContext "maxAcceptedHtlcs") ::
      (uint64Overflow withContext "channelCapacityMsat") ::
      (uint16 withContext "liabilityDeadlineBlockdays") ::
      (uint64Overflow withContext "minimalOnchainRefundAmountSatoshis") ::
      (uint64Overflow withContext "initialClientBalanceMsat")
  }.as[InitHostedChannel]

  val lastCrossSignedStateCodec = {
    (varsizebinarydata withContext "lastRefundScriptPubKey") ::
      (initHostedChannelCodec withContext "initHostedChannel") ::
      (stateUpdateCodec withContext "lastLocalStateUpdate") ::
      (stateUpdateCodec withContext "lastRemoteStateUpdate")
  }.as[LastCrossSignedState]

  val lightningMessageCodec =
    discriminated[LightningMessage].by(uint16)
      .typecase(cr = init.as[Init], tag = 16)
      .typecase(cr = errorCodec, tag = 17)
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
      .typecase(cr = queryChannelRangeCodec, tag = 263)
      .typecase(cr = gossipTimestampFilterCodec, tag = 265)
      .typecase(cr = announcementSignatures.as[AnnouncementSignatures], tag = 259)
      .typecase(cr = invokeHostedChannelCodec, tag = 65535)
      .typecase(cr = initHostedChannelCodec, tag = 65534)
      .typecase(cr = lastCrossSignedStateCodec, tag = 65533)
      .typecase(cr = stateUpdateCodec, tag = 65532)
      .typecase(cr = stateOverrideCodec, tag = 65531)

  // Not in a spec

  val hopCodec = {
    (publicKey withContext "nodeId") ::
      (int64 withContext "shortChannelId") ::
      (uint16 withContext "cltvExpiryDelta") ::
      (uint64Overflow withContext "htlcMinimumMsat") ::
      (uint32 withContext "feeBaseMsat") ::
      (uint32 withContext "feeProportionalMillionths")
  }.as[Hop]

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

// TLV

trait Tlv
sealed trait OnionTlv extends Tlv
case class GenericTlv(tag: UInt64, value: ByteVector) extends Tlv
case class TlvStream(records: Traversable[Tlv], unknown: Traversable[GenericTlv] = Nil)

object Tlv { me =>
  type TlvUint64Disc = DiscriminatorCodec[Tlv, UInt64]
  type EitherTlv = Either[GenericTlv, Tlv]
  type EitherTlvList = List[EitherTlv]

  private val genericTlv = (varint withContext "tag") :: variableSizeBytesLong(varintoverflow, bytes)
  private val genericTlvCodec = genericTlv.as[GenericTlv].exmap(validateGenericTlv, validateGenericTlv)

  private def validateGenericTlv(generic: GenericTlv): Attempt[GenericTlv] =
    if (generic.tag.toBigInt % 2 == 0) Attempt Failure Err("Unknown even tlv type")
    else Attempt Successful generic

  private def variableSizeUInt64(size: Int, min: Long): Codec[UInt64] =
    minimalValue(bytes(size).xmap(UInt64.apply, _.toByteVector takeRight size), min)

  private def tag(codec: TlvUint64Disc, record: EitherTlv): UInt64 = record match {
    case Right(knownTlv) => codec.encode(knownTlv).flatMap(varint.decode).require.value
    case Left(unknownTlv) => unknownTlv.tag
  }

  val tu64: Codec[UInt64] = discriminated[UInt64].by(uint8)
    .\(0x00) { case value if value < 0x01 => value } (variableSizeUInt64(0, 0x00))
    .\(0x01) { case value if value < 0x0100 => value } (variableSizeUInt64(1, 0x01))
    .\(0x02) { case value if value < 0x010000 => value } (variableSizeUInt64(2, 0x0100))
    .\(0x03) { case value if value < 0x01000000 => value } (variableSizeUInt64(3, 0x010000))
    .\(0x04) { case value if value < 0x0100000000L => value } (variableSizeUInt64(4, 0x01000000))
    .\(0x05) { case value if value < 0x010000000000L => value } (variableSizeUInt64(5, 0x0100000000L))
    .\(0x06) { case value if value < 0x01000000000000L => value } (variableSizeUInt64(6, 0x010000000000L))
    .\(0x07) { case value if value < 0x0100000000000000L => value } (variableSizeUInt64(7, 0x01000000000000L))
    .\(0x08) { case value if value <= UInt64.MaxValue => value } (variableSizeUInt64(8, 0x0100000000000000L))

  val tu32: Codec[Long] = tu64.exmap(f = {
    case value if value > 0xFFFFFFFFL => Attempt Failure Err("tu32 overflow")
    case value => Attempt Successful value.toBigInt.toLong
  }, long => Attempt Successful long)

  val tu16: Codec[Int] = tu32.exmap(f = {
    case value if value > 0xFFFF => Attempt Failure Err("tu16 overflow")
    case value => Attempt Successful value.toBigInt.toInt
  }, long => Attempt Successful long)

  def tlvStream(codec: TlvUint64Disc): Codec[TlvStream] = {
    val withFallback = discriminatorFallback(genericTlvCodec, codec)

    list(withFallback).exmap(
      recordsEitherTlvList => {
        val tags = for (record <- recordsEitherTlvList) yield tag(codec, record)
        val knownTags = recordsEitherTlvList.collect { case Right(known) => known }
        val unknownTags = recordsEitherTlvList.collect { case Left(generic) => generic }
        if (tags.length != tags.distinct.length) Attempt Failure Err("Tlv streams must not contain duplicate records")
        else if (tags != tags.sorted) Attempt Failure Err("Tlv records must be ordered by monotonically-increasing types")
        else Attempt Successful TlvStream(knownTags, unknownTags)
      },

      stream => {
        val knownRecords = stream.records map Right.apply
        val unknownRecords = stream.unknown map Left.apply
        val records = (knownRecords ++ unknownRecords).toList
        val tags = for (record <- records) yield tag(codec, record)
        if (tags.length != tags.distinct.length) Attempt Failure Err("Tlv streams must not contain duplicate records")
        else Attempt Successful tags.zip(records).sortBy { case (tag, _) => tag }.map { case (_, record) => record }
      }
    )
  }

  def lengthPrefixedTlvStream(codec: TlvUint64Disc): Codec[TlvStream] =
    variableSizeBytesLong(value = tlvStream(codec), size = varintoverflow)
}

// ONION

case class OnionRoutingPacket(version: Int, publicKey: ByteVector, payload: ByteVector, hmac: ByteVector)
case class PerHopPayload(shortChannelId: Long, amtToForward: Long, outgoingCltvValue: Long)

object OnionCodecs {
  def onionRoutingPacketCodec(payloadLength: Int) = {
    (uint8 withContext "version") ::
      (bytes(33) withContext "publicKey") ::
      (bytes(payloadLength) withContext "onionPayload") ::
      (bytes32 withContext "hmac")
  }.as[OnionRoutingPacket]

  val paymentOnionPacketCodec: Codec[OnionRoutingPacket] =
    onionRoutingPacketCodec(Sphinx.PaymentPacket.PayloadLength)

  val perHopPayloadCodec: Codec[PerHopPayload] = {
    (constant(ByteVector fromByte 0) withContext "realm") ::
      (uint64Overflow withContext "short_channel_id") ::
      (uint64Overflow withContext "amt_to_forward") ::
      (uint32 withContext "outgoing_cltv_value") ::
      (ignore(8 * 12) withContext "unused_with_v0_version_on_header")
  }.as[PerHopPayload]

  val payloadLengthDecoder = Decoder[Long] { bits: BitVector =>
    varintoverflow.decode(bits) map { decResult: DecodeResult[Long] =>
      val payload = decResult.value + (bits.length - decResult.remainder.length) / 8
      DecodeResult(payload, decResult.remainder)
    }
  }
}