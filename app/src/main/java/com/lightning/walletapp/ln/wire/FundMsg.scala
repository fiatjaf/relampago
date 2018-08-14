package com.lightning.walletapp.ln.wire

import spray.json._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import fr.acinq.bitcoin.{Satoshi, BinaryData, Transaction}
import com.lightning.walletapp.ln.Tools.UserId


object FundMsg {
  val FAIL_VERIFY_ERROR = 101
  val FAIL_NOT_VERIFIED_YET = 102
  val FAIL_INTERNAL_ERROR = 301

  val FAIL_RESERVE_FAILED = 201
  val FAIL_RESERVE_EXPIRED = 202
  val FAIL_AMOUNT_TOO_LARGE = 203
  val FAIL_AMOUNT_TOO_SMALL = 204
  val FAIL_FUNDING_IS_TRIED = 205
  val FAIL_FUNDING_EXISTS = 206
  val FAIL_PUBLISH_ERROR = 207
}

// Setup
trait FundMsg { def userId: UserId }
case class Started(start: Start, expiry: Long, fee: Satoshi) extends FundMsg {
  val endPoint = s"ws://${start.host}:${start.port}/${start.toJson.toString.hex}"
  def userId: UserId = start.userId
}

case class Start(userId: UserId, fundingAmount: Satoshi, host: String, port: Int, extra: Option[String] = None) extends FundMsg
case class Fail(code: Int, reason: String, userId: UserId = "noUserId") extends FundMsg { def report = s"Funding ID: $userId<br>$reason" }

// Switching remote peers
case class PrepareFundingTx(userId: UserId, pubkeyScript: BinaryData) extends FundMsg
case class FundingTxReady(userId: UserId, txHash: BinaryData, outIndex: Int) extends FundMsg

// Finalizing
case class BroadcastFundingTx(userId: UserId, txHash: BinaryData) extends FundMsg
case class FundingTxBroadcasted(userId: UserId, tx: Transaction) extends FundMsg