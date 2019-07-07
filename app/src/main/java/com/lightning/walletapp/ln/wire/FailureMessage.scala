package com.lightning.walletapp.ln.wire

import scodec.codecs._
import LightningMessageCodecs._
import scodec.bits.ByteVector
import scodec.Attempt


sealed trait FailureMessage
case object ExpiryTooFar extends FailureMessage

sealed trait Perm extends FailureMessage
sealed trait Node extends FailureMessage
case object UnknownNextPeer extends Perm
case object IncorrectPaymentAmount extends Perm
case object PermanentChannelFailure extends Perm
case object RequiredChannelFeatureMissing extends Perm
case class FinalIncorrectCltvExpiry(expiry: Long) extends Perm
case class FinalIncorrectHtlcAmount(amountMsat: Long) extends Perm
case class IncorrectOrUnknownPaymentDetails(amountMsat: Long) extends Perm
case object FinalExpiryTooSoon extends Perm
case object InvalidRealm extends Perm

case object TemporaryNodeFailure extends Node
case object PermanentNodeFailure extends Perm with Node
case object RequiredNodeFeatureMissing extends Perm with Node

sealed trait BadOnion extends FailureMessage { def onionHash: ByteVector }
case class InvalidOnionVersion(onionHash: ByteVector) extends BadOnion with Perm
case class InvalidOnionHmac(onionHash: ByteVector) extends BadOnion with Perm
case class InvalidOnionKey(onionHash: ByteVector) extends BadOnion with Perm

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
  private val unknownPayment = withDefaultValue(optional(bitsRemaining, uint64Overflow), 0L) withContext "amountMsat"
  private val wrongAmount = (uint64Overflow withContext "amountMsat") :: channelUpdateWithLengthCodec
  private val wrongExpiry = (uint32 withContext "expiry") :: channelUpdateWithLengthCodec

  val BADONION = 0x8000
  val UPDATE = 0x1000
  val PERM = 0x4000
  val NODE = 0x2000

  val failureMessageCodec = discriminated[FailureMessage].by(uint16)
    .typecase(cr = provide(InvalidRealm), tag = PERM | 1)
    .typecase(cr = provide(TemporaryNodeFailure), tag = NODE | 2)
    .typecase(cr = provide(PermanentNodeFailure), tag = PERM | 2)
    .typecase(cr = provide(RequiredNodeFeatureMissing), tag = PERM | NODE | 3)
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
    .typecase(cr = unknownPayment.as[IncorrectOrUnknownPaymentDetails], tag = PERM | 15)
    .typecase(cr = provide(IncorrectPaymentAmount), tag = PERM | 16)
    .typecase(cr = provide(FinalExpiryTooSoon), tag = 17)
    .typecase(cr = (uint32 withContext "expiry").as[FinalIncorrectCltvExpiry], tag = 18)
    .typecase(cr = (uint64Overflow withContext "amountMsat").as[FinalIncorrectHtlcAmount], tag = 19)
    .typecase(cr = disabled.as[ChannelDisabled], tag = UPDATE | 20)
    .typecase(cr = provide(ExpiryTooFar), tag = 21)
}