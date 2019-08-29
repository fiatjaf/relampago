package com.lightning.walletapp.ln.wire

import scodec.codecs._
import LightningMessageCodecs._
import com.lightning.walletapp.ln.wire.FailureMessageCodecs._
import com.lightning.walletapp.ln.crypto.Mac32
import scodec.bits.ByteVector
import scodec.Attempt


sealed trait FailureMessage { me =>
  lazy val code: Int = failureMessageCodec.encode(me).flatMap(uint16.decode).require.value
  lazy val isPermanent: Boolean = (code & PERM) != 0
  lazy val isTemporary: Boolean = (code & NODE) != 0
}

case object ExpiryTooFar extends FailureMessage
case object InvalidRealm extends FailureMessage
case object UnknownNextPeer extends FailureMessage
case object TemporaryNodeFailure extends FailureMessage
case object PermanentNodeFailure extends FailureMessage
case object PermanentChannelFailure extends FailureMessage
case object RequiredNodeFeatureMissing extends FailureMessage
case object RequiredChannelFeatureMissing extends FailureMessage
case class FinalIncorrectCltvExpiry(expiry: Long) extends FailureMessage
case class FinalIncorrectHtlcAmount(amountMsat: Long) extends FailureMessage
case class IncorrectOrUnknownPaymentDetails(amountMsat: Long, height: Long) extends FailureMessage
case class UnknownFailureMessage(failureCode: Int) extends FailureMessage { override lazy val code = failureCode }

sealed trait BadOnion extends FailureMessage { def onionHash: ByteVector }
case class InvalidOnionVersion(onionHash: ByteVector) extends BadOnion with FailureMessage
case class InvalidOnionPayload(onionHash: ByteVector) extends BadOnion with FailureMessage
case class InvalidOnionHmac(onionHash: ByteVector) extends BadOnion with FailureMessage
case class InvalidOnionKey(onionHash: ByteVector) extends BadOnion with FailureMessage

sealed trait Update extends FailureMessage { def update: ChannelUpdate }
case class AmountBelowMinimum(amountMsat: Long, update: ChannelUpdate) extends Update
case class ChannelDisabled(messageFlags: Byte, channelFlags: Byte, update: ChannelUpdate) extends Update
case class FeeInsufficient(amountMsat: Long, update: ChannelUpdate) extends Update
case class IncorrectCltvExpiry(expiry: Long, update: ChannelUpdate) extends Update
case class TemporaryChannelFailure(update: ChannelUpdate) extends Update
case class ExpiryTooSoon(update: ChannelUpdate) extends Update

object FailureMessageCodecs {
  private val channelUpdateCodecWithType = lightningMessageCodec.narrow[ChannelUpdate](Attempt successful _.asInstanceOf[ChannelUpdate], identity)
  private val channelUpdateWithLengthCodec = variableSizeBytes(value = choice(channelUpdateCodecWithType, channelUpdateCodec), size = uint16)
  private val disabled = (byte withContext "messageFlags") :: (byte withContext "channelFlags") :: channelUpdateWithLengthCodec
  private val wrongAmount = (uint64Overflow withContext "amountMsat") :: channelUpdateWithLengthCodec
  private val wrongExpiry = (uint32 withContext "expiry") :: channelUpdateWithLengthCodec

  private val incorrectOrUnknownPaymentDetails =
    (withDefaultValue(optional(bitsRemaining, uint64Overflow), 0L) withContext "amountMsat") ::
      (withDefaultValue(optional(bitsRemaining, uint32), 0L) withContext "height")

  val BADONION = 0x8000
  val UPDATE = 0x1000
  val PERM = 0x4000
  val NODE = 0x2000

  val failureMessageCodec =
    discriminatorWithDefault(
      discriminated[FailureMessage].by(uint16)
        .typecase(cr = provide(InvalidRealm), tag = PERM | 1)
        .typecase(cr = provide(TemporaryNodeFailure), tag = NODE | 2)
        .typecase(cr = provide(PermanentNodeFailure), tag = PERM | NODE | 2)
        .typecase(cr = provide(RequiredNodeFeatureMissing), tag = PERM | NODE | 3)
        .typecase(cr = bytes32.as[InvalidOnionPayload], tag = BADONION | PERM)
        .typecase(cr = bytes32.as[InvalidOnionVersion], tag = BADONION | PERM | 4)
        .typecase(cr = bytes32.as[InvalidOnionHmac], tag = BADONION | PERM | 5)
        .typecase(cr = bytes32.as[InvalidOnionKey], tag = BADONION | PERM | 6)
        .typecase(cr = channelUpdateWithLengthCodec.as[TemporaryChannelFailure], tag = UPDATE | 7)
        .typecase(cr = provide(PermanentChannelFailure), tag = PERM | 8)
        .typecase(cr = provide(RequiredChannelFeatureMissing), tag = PERM | 9)
        .typecase(cr = provide(UnknownNextPeer), tag = PERM | 10)
        .typecase(cr = wrongAmount.as[AmountBelowMinimum], tag = UPDATE | 11)
        .typecase(cr = wrongAmount.as[FeeInsufficient], tag = UPDATE | 12)
        .typecase(cr = wrongExpiry.as[IncorrectCltvExpiry], tag = UPDATE | 13)
        .typecase(cr = channelUpdateWithLengthCodec.as[ExpiryTooSoon], tag = UPDATE | 14)
        .typecase(cr = incorrectOrUnknownPaymentDetails.as[IncorrectOrUnknownPaymentDetails], tag = PERM | 15)
        .typecase(cr = (uint32 withContext "expiry").as[FinalIncorrectCltvExpiry], tag = 18)
        .typecase(cr = (uint64Overflow withContext "amountMsat").as[FinalIncorrectHtlcAmount], tag = 19)
        .typecase(cr = disabled.as[ChannelDisabled], tag = UPDATE | 20)
        .typecase(cr = provide(ExpiryTooFar), tag = 21),
      uint16.xmap(UnknownFailureMessage(_).asInstanceOf[FailureMessage], (_: FailureMessage).code)
    )

  /**
    * An onion-encrypted failure from an intermediate node:
    * +----------------+----------------------------------+-----------------+----------------------+-----+
    * | HMAC(32 bytes) | failure message length (2 bytes) | failure message | pad length (2 bytes) | pad |
    * +----------------+----------------------------------+-----------------+----------------------+-----+
    * with failure message length + pad length = 256
    */

  def failureOnionCodec(mac: Mac32) = prependmac(paddedFixedSizeBytesDependent(260,
    variableSizeBytes(value = failureMessageCodec, size = uint16) withContext "failureMessage",
    nBits => variableSizeBytes(value = ignore(nBits - 2 * 8), size = uint16) withContext "padding"
  ).as[FailureMessage], mac)
}