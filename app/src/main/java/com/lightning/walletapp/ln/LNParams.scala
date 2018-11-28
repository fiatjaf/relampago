package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.Scripts._
import fr.acinq.bitcoin.DeterministicWallet._

import com.lightning.walletapp.Utils.{app, dbFileName}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import com.lightning.walletapp.ln.LNParams.DepthAndDead
import com.lightning.walletapp.ln.wire.NodeAnnouncement
import com.lightning.walletapp.ChannelManager
import fr.acinq.eclair.UInt64


object LNParams { me =>
  type DepthAndDead = (Int, Boolean)
  val chainHash = Block.LivenetGenesisBlock.hash
  val channelReserveToFundingRatio = 100
  val localFeatures = "02"
  val globalFeatures = ""
  val minDepth = 1

  val maxCltvDelta = 7 * 144L
  val maxToSelfDelay = 2000L

  val minCapacitySat = 300000L
  final val dust = Satoshi(2730L)
  final val minFeeratePerKw = 253
  final val maxHtlcValueMsat = 10000000000L
  final val maxCapacity = Satoshi(16777215L)
  final val minHtlcValue = MilliSatoshi(1000L)

  var db: LNOpenHelper = _
  var extendedNodeKey: ExtendedPrivateKey = _
  var extendedCloudKey: ExtendedPrivateKey = _

  lazy val bag = PaymentInfoWrap
  lazy val broadcaster: Broadcaster = ChannelManager
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
    fee > 25000 * (hops + 1) + msat / 50

  // On-chain fee calculations
  def shouldUpdateFee(network: Long, commit: Long) = {
    val mismatch = 2.0 * (network - commit) / (commit + network)
    mismatch < -0.25 || mismatch > 0.25
  }

  def makeLocalParams(ann: NodeAnnouncement, theirReserve: Long, finalScriptPubKey: BinaryData, idx: Long, isFunder: Boolean) = {
    val Seq(fund, revoke, pay, delay, htlc, sha) = for (ord <- 0L to 5L) yield derivePrivateKey(extendedNodeKey, idx :: ord :: Nil)
    val toSelfDelay = if (ann.nodeId == JointNode.jointNodeKey) 2880 else 1440

    LocalParams(maxHtlcValueInFlightMsat = UInt64(maxHtlcValueMsat), theirReserve, toSelfDelay,
      maxAcceptedHtlcs = 25, fund.privateKey, revoke.privateKey, pay.privateKey, delay.privateKey,
      htlc.privateKey, finalScriptPubKey, dust, shaSeed = sha256(sha.privateKey.toBin), isFunder)
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
  def delay = parent match { case pd \ false \ blocksLeft => blocksLeft case _ => Long.MinValue }
  val parent: (DepthAndDead, Long)
}

case class HideReady(txn: Transaction) extends PublishStatus
case class ShowReady(txn: Transaction, fee: Satoshi, amount: Satoshi) extends PublishStatus
case class HideDelayed(parent: (DepthAndDead, Long), txn: Transaction) extends DelayedPublishStatus
case class ShowDelayed(parent: (DepthAndDead, Long), txn: Transaction, commitTx: Transaction,
                       fee: Satoshi, amount: Satoshi) extends DelayedPublishStatus

trait Broadcaster extends ChannelListener {
  def getTx(txid: BinaryData): Option[org.bitcoinj.core.Transaction]
  def getStatus(txid: BinaryData): DepthAndDead
  def currentHeight: Long
  def perKwThreeSat: Long
  def perKwSixSat: Long

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
  def csvShowDelayed(t1: TransactionWithInputInfo, t2: TransactionWithInputInfo, commitTx: Transaction) =
    ShowDelayed(parent = csv(t1.tx, t2.tx), t2.tx, commitTx, fee = t1 -- t2, t2.tx.allOutputsAmount)
}