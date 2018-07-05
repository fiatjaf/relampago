package com.lightning.walletapp.ln.wire

import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}
import com.lightning.walletapp.ln.Tools.UserId


trait FundMsg { def userId: UserId }
case class Fail(code: Int, reason: String, userId: UserId = "noUserId") extends FundMsg
case class Start(userId: UserId, fundingAmount: Satoshi, extra: Option[String] = None) extends FundMsg
case class FundingTxCreated(start: Start, expiration: Long) extends FundMsg { def userId: UserId = start.userId }
case class FundingTxAwaits(start: Start, expiration: Long) extends FundMsg { def userId: UserId = start.userId }
case class FundingTxSigned(userId: UserId, txHash: BinaryData, outIndex: Int) extends FundMsg
case class SignFundingTx(userId: UserId, pubkeyScript: BinaryData) extends FundMsg
case class BroadcastFundingTx(userId: UserId, txHash: BinaryData) extends FundMsg
case class FundingTxBroadcasted(userId: UserId, tx: Transaction) extends FundMsg