package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.Scripts._
import fr.acinq.bitcoin.DeterministicWallet._
import com.lightning.walletapp.Utils.{app, dbFileName}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import com.lightning.walletapp.ln.LNParams.DepthAndDead
import fr.acinq.eclair.UInt64


object LNParams { me =>
  type DepthAndDead = (Int, Boolean)
  val chainHash = Block.TestnetGenesisBlock.hash
  val theirReserveToFundingRatio = 50
  val localFeatures = "02"
  val globalFeatures = ""
  val minDepth = 1

  val maxCltvDelta = 7 * 144L
  final val minHtlcValue = MilliSatoshi(1000L)
  final val maxHtlcValue = MilliSatoshi(4000000000L)
  final val maxChannelCapacity = MilliSatoshi(16777216000L)
  final val minChannelCapacity = MilliSatoshi(300000000L)

  var db: LNOpenHelper = _
  var extendedNodeKey: ExtendedPrivateKey = _
  var extendedCloudKey: ExtendedPrivateKey = _

  lazy val bag = PaymentInfoWrap
  lazy val broadcaster: Broadcaster = LocalBroadcaster
  lazy val nodePublicKey: PublicKey = nodePrivateKey.publicKey
  lazy val nodePrivateKey: PrivateKey = extendedNodeKey.privateKey
  lazy val cloudSecret = sha256(extendedCloudKey.privateKey.toBin.data)
  lazy val cloudId = sha256(cloudSecret.data)

  def setup(seed: BinaryData) = generate(seed) match { case m =>
    extendedNodeKey = derivePrivateKey(m, hardened(46) :: hardened(0) :: Nil)
    extendedCloudKey = derivePrivateKey(m, hardened(92) :: hardened(0) :: Nil)
    db = new LNOpenHelper(app, dbFileName)
  }

  def isFeeNotOk(msat: Long, fee: Long, hops: Int) =
    fee > 10000 * (hops + 1) + msat / 25

  // On-chain fee calculations
  def shouldUpdateFee(network: Long, commit: Long) = {
    val mismatch = 2.0 * (network - commit) / (commit + network)
    mismatch < -0.25 || mismatch > 0.25
  }

  def makeLocalParams(theirReserve: Long, finalScriptPubKey: BinaryData, idx: Long) = {
    val Seq(fund, revoke, pay, delay, htlc, sha) = for (n <- 0L to 5L) yield derivePrivateKey(extendedNodeKey, idx :: n :: Nil)
    LocalParams(maxHtlcValueInFlightMsat = UInt64(4000000000L), theirReserve, toSelfDelay = 144, maxAcceptedHtlcs = 25,
      fund.privateKey, revoke.privateKey, pay.privateKey, delay.privateKey, htlc.privateKey, finalScriptPubKey,
      dustLimit = Satoshi(5460L), shaSeed = sha256(sha.privateKey.toBin), isFunder = true)
  }
}

object AddErrorCodes {
  import com.lightning.walletapp.R.string._
  val ERR_AMOUNT_OVERFLOW = err_ln_amount_overflow
  val ERR_REMOTE_AMOUNT_HIGH = err_ln_remote_amount_high
  val ERR_REMOTE_AMOUNT_LOW = err_ln_remote_amount_low
  val ERR_TOO_MANY_HTLC = err_ln_too_many
  val ERR_FAILED = err_general
}

trait PublishStatus {
  val txn: Transaction
  def isPublishable = true
}

trait DelayedPublishStatus extends PublishStatus {
  // Is publishable if parent depth > 0 AND parent is not dead AND no CLTV or CSV delays
  override def isPublishable = parent match { case pd \ false \ 0L => pd > 0L case _ => false }
  val parent: (DepthAndDead, Long)
}

case class HideReady(txn: Transaction) extends PublishStatus
case class ShowReady(txn: Transaction, fee: Satoshi, amount: Satoshi) extends PublishStatus
case class HideDelayed(parent: (DepthAndDead, Long), txn: Transaction) extends DelayedPublishStatus
case class ShowDelayed(parent: (DepthAndDead, Long), txn: Transaction, fee: Satoshi, amount: Satoshi)
  extends DelayedPublishStatus

trait Broadcaster extends ChannelListener { me =>
  def getTx(txid: BinaryData): Option[org.bitcoinj.core.Transaction]
  def getBlockHashString(txid: BinaryData): Option[String]
  def getStatus(txid: BinaryData): DepthAndDead
  def currentHeight: Long
  def perKwSixSat: Long
  def perKwTwoSat: Long

  // Parent state and next tier cltv delay
  // actual negative delay will be represented as 0L
  def cltv(parent: Transaction, child: Transaction) = {
    val parentDepth \ parentIsDead = getStatus(parent.txid)
    val cltvDelay = math.max(cltvBlocks(child) - currentHeight, 0L)
    parentDepth -> parentIsDead -> cltvDelay
  }

  // Parent state and cltv + next tier csv delay
  // actual negative delay will be represented as 0L
  def csv(parent: Transaction, child: Transaction) = {
    val parentDepth \ parentIsDead = getStatus(parent.txid)
    val cltvDelay = math.max(cltvBlocks(parent) - currentHeight, 0L)
    val csvDelay = math.max(csvTimeout(child) - parentDepth, 0L)
    parentDepth -> parentIsDead -> (cltvDelay + csvDelay)
  }

  val blocksPerDay = 144
  def csvShowDelayed(t1: TransactionWithInputInfo, t2: TransactionWithInputInfo) =
    ShowDelayed(csv(t1.tx, t2.tx), t2.tx, t1 -- t2, t2.tx.allOutputsAmount)
}