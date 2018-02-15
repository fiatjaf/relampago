package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.lightning.wallet.lnutils._
import com.lightning.wallet.ln.Scripts._
import fr.acinq.bitcoin.DeterministicWallet._

import scala.util.{Failure, Success}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import com.lightning.wallet.lnutils.CloudDataSaver.TryCloudData
import com.lightning.wallet.ln.LNParams.DepthAndDead
import com.lightning.wallet.Utils.app
import fr.acinq.eclair.UInt64


object LNParams { me =>
  type DepthAndDead = (Int, Boolean)
  val maxChannelCapacity = MilliSatoshi(16777216000L)
  val maxHtlcValue = MilliSatoshi(4194304000L)
  val minHtlcValue = MilliSatoshi(1000L)
  val dustLimit = Satoshi(573L)

  val chainHash = Block.TestnetGenesisBlock.hash
  val theirReserveToFundingRatio = 0.01 // 1%
  val maxReserveToFundingRatio = 0.05 // 5%
  val localFeatures = "02"
  val globalFeatures = ""
  val minDepth = 1

  var extendedNodeKey: ExtendedPrivateKey = _
  var extendedCloudKey: ExtendedPrivateKey = _
  var cloud: StateMachine[CloudData] with Cloud = _
  var db: CipherOpenHelper = _

  lazy val nodePrivateKey: PrivateKey = extendedNodeKey.privateKey // Corresponding pubkey is node id
  lazy val cloudPrivateKey: PrivateKey = extendedCloudKey.privateKey // Sign messages to private maintenance server
  lazy val cloudPublicKey: PublicKey = cloudPrivateKey.publicKey // Check signed messages on private maintenance server

  lazy val cloudSecret: BinaryData = sha256(cloudPrivateKey.toBin) // Secret for encrypting data for maintenance server
  lazy val cloudId: BinaryData = sha256(cloudPublicKey.toBin) // ID for retrieving data from maintenance server
  lazy val broadcaster: Broadcaster = LocalBroadcaster
  lazy val bag = PaymentInfoWrap

  def setup(seed: BinaryData) = generate(seed) match { case master =>
    extendedCloudKey = derivePrivateKey(master, hardened(92) :: hardened(0) :: Nil)
    extendedNodeKey = derivePrivateKey(master, hardened(46) :: hardened(0) :: Nil)
    db = new CipherOpenHelper(app, 1, sha256(seed).toString)
    cloud = me getCloud CloudDataSaver.tryGetObject
  }

  // CLOUD

  def getCloud(tryData: TryCloudData) = tryData match {
    case Failure(why) => new PublicCloud(bag) { data = CloudDataSaver.empty }
    case Success(saved) if saved.url.isEmpty => new PublicCloud(bag) { data = saved }
    case Success(saved) => new PrivateCloud { data = saved }
  }

  // FEE RELATED

  def isFeeNotOk(feeMsat: Long) = feeMsat > 50000000
  def shouldUpdateFee(oldPerKw: Long, newPerKw: Long) = {
    val mismatch = (newPerKw - oldPerKw) / (oldPerKw + newPerKw)
    math.abs(2.0 * mismatch) > 0.25
  }

  // MISC

  def makeLocalParams(reserve: Long, finalScriptPubKey: BinaryData, idx: Long) = {
    val Seq(fund, revoke, pay, delay, htlc, sha) = for (n <- 0L to 5L) yield derivePrivateKey(extendedNodeKey, idx :: n :: Nil)
    LocalParams(UInt64(Long.MaxValue), reserve, toSelfDelay = 144, maxAcceptedHtlcs = 25, fund.privateKey, revoke.privateKey,
      pay.privateKey, delay.privateKey, htlc.privateKey, finalScriptPubKey, sha256(sha.privateKey.toBin), isFunder = true)
  }
}

object AddErrorCodes {
  import com.lightning.wallet.R.string._
  val ERR_AMOUNT_OVERFLOW = err_ln_amount_overflow
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
  def txStatus(txid: BinaryData): DepthAndDead
  def currentHeight: Long
  def ratePerKwSat: Long

  // Parent state and next tier cltv delay
  // Actual negative delay will be represented as 0L
  def cltv(parent: Transaction, child: Transaction) = {
    val parentDepth \ parentIsDead = txStatus(parent.txid)
    val cltvDelay = math.max(cltvBlocks(child) - currentHeight, 0L)
    parentDepth -> parentIsDead -> cltvDelay
  }

  // Parent state and cltv + next tier csv delay
  // Actual negative delay will be represented as 0L
  def csv(parent: Transaction, child: Transaction) = {
    val parentDepth \ parentIsDead = txStatus(parent.txid)
    val cltvDelay = math.max(cltvBlocks(parent) - currentHeight, 0L)
    val csvDelay = math.max(csvTimeout(child) - parentDepth, 0L)
    parentDepth -> parentIsDead -> (cltvDelay + csvDelay)
  }

  def csvShowDelayed(t1: TransactionWithInputInfo, t2: TransactionWithInputInfo) =
    ShowDelayed(csv(t1.tx, t2.tx), t2.tx, t1 -- t2, t2.tx.allOutputsAmount)

  def cltvShowDelayed(commit: Transaction, t1: TransactionWithInputInfo) =
    ShowDelayed(cltv(commit, t1.tx), t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
}