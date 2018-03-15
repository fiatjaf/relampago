package com.lightning.wallet.ln.wire

import scodec.codecs._
import LightningMessageCodecs._
import fr.acinq.bitcoin.BinaryData


sealed trait FailureMessage
case object ExpiryTooFar extends FailureMessage
case object FinalExpiryTooSoon extends FailureMessage
case class FinalIncorrectCltvExpiry(expiry: Long) extends FailureMessage
case class FinalIncorrectHtlcAmount(amountMsat: Long) extends FailureMessage

sealed trait Perm extends FailureMessage
sealed trait Node extends FailureMessage
case object UnknownNextPeer extends Perm // exclude channel for 60 minutes, for all payments
case object UnknownPaymentHash extends Perm // halt payment
case object IncorrectPaymentAmount extends Perm // halt payment
case object PermanentChannelFailure extends Perm // exclude channel for 60 minutes, for all payments
case object RequiredChannelFeatureMissing extends Perm // exclude channel for 60 minutes, for all payments
case object InvalidRealm extends Perm // halt payment

// shortChanIds drop 1 dropRight 1: exclude channels for 5 minutes, for target payment

// resourceId, resourceType, targetNodeId, expiresAt, forAllPayments

// SELECT resourceId, resourceType WHERE expiresAt > ? AND (forAllPayments = 1 OR targetNodeId = ?) ORDER BY id DESC LIMIT 250

case object TemporaryNodeFailure extends Node // exclude node for 10 minutes, for all payments
case object PermanentNodeFailure extends Perm with Node // exclude node for 60 minutes, for all payments
case object RequiredNodeFeatureMissing extends Perm with Node // exclude node for 60 minutes, for all payments

sealed trait BadOnion extends FailureMessage { def onionHash: BinaryData }
case class InvalidOnionVersion(onionHash: BinaryData) extends BadOnion with Perm // halt payment
case class InvalidOnionHmac(onionHash: BinaryData) extends BadOnion with Perm // halt payment
case class InvalidOnionKey(onionHash: BinaryData) extends BadOnion with Perm // halt payment

sealed trait Update extends FailureMessage { def update: ChannelUpdate }
case class AmountBelowMinimum(amountMsat: Long, update: ChannelUpdate) extends Update // halt payment
case class ChannelDisabled(flags: BinaryData, update: ChannelUpdate) extends Update // exclude channel for 10 minutes, for all payments
case class FeeInsufficient(amountMsat: Long, update: ChannelUpdate) extends Update // halt payment
case class IncorrectCltvExpiry(expiry: Long, update: ChannelUpdate) extends Update // halt payment
case class TemporaryChannelFailure(update: ChannelUpdate) extends Update // exclude channel for 10 minutes, for target payment
case class ExpiryTooSoon(update: ChannelUpdate) extends Update // halt payment

object FailureMessageCodecs {
  private val sha256Codec = binarydata(32) withContext "sha256Codec"
  private val channelUpdateWithLengthCodec = variableSizeBytes(uint16, channelUpdateCodec) withContext "channelUpdate"
  private val disabled = (binarydata(2) withContext "flags") :: channelUpdateWithLengthCodec
  private val amount = (uint64 withContext "amountMsat") :: channelUpdateWithLengthCodec
  private val expiry = (uint32 withContext "expiry") :: channelUpdateWithLengthCodec

  val BADONION = 0x8000
  val UPDATE = 0x1000
  val PERM = 0x4000
  val NODE = 0x2000

  val failureMessageCodec = discriminated[FailureMessage].by(uint16)
    .typecase(cr = provide(InvalidRealm), tag = PERM | 1)
    .typecase(cr = provide(TemporaryNodeFailure), tag = NODE | 2)
    .typecase(cr = provide(PermanentNodeFailure), tag = PERM | 2)
    .typecase(cr = provide(RequiredNodeFeatureMissing), tag = PERM | NODE | 3)
    .typecase(cr = sha256Codec.as[InvalidOnionVersion], tag = BADONION | PERM | 4)
    .typecase(cr = sha256Codec.as[InvalidOnionHmac], tag = BADONION | PERM | 5)
    .typecase(cr = sha256Codec.as[InvalidOnionKey], tag = BADONION | PERM | 6)
    .typecase(cr = channelUpdateWithLengthCodec.as[TemporaryChannelFailure], tag = UPDATE | 7)
    .typecase(cr = provide(PermanentChannelFailure), tag = PERM | 8)
    .typecase(cr = provide(RequiredChannelFeatureMissing), tag = PERM | 9)
    .typecase(cr = provide(UnknownNextPeer), tag = PERM | 10)
    .typecase(cr = amount.as[AmountBelowMinimum], tag = UPDATE | 11)
    .typecase(cr = amount.as[FeeInsufficient], tag = UPDATE | 12)
    .typecase(cr = expiry.as[IncorrectCltvExpiry], tag = UPDATE | 13)
    .typecase(cr = channelUpdateWithLengthCodec.as[ExpiryTooSoon], tag = UPDATE | 14)
    .typecase(cr = provide(UnknownPaymentHash), tag = PERM | 15)
    .typecase(cr = provide(IncorrectPaymentAmount), tag = PERM | 16)
    .typecase(cr = provide(FinalExpiryTooSoon), tag = 17)
    .typecase(cr = (uint32 withContext "expiry").as[FinalIncorrectCltvExpiry], tag = 18)
    .typecase(cr = (uint32 withContext "amountMsat").as[FinalIncorrectHtlcAmount], tag = 19)
    .typecase(cr = disabled.as[ChannelDisabled], tag = UPDATE | 20)
    .typecase(cr = provide(ExpiryTooFar), tag = 21)
}

