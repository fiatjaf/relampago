package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.Scripts._
import fr.acinq.bitcoin.DeterministicWallet._
import com.lightning.walletapp.Utils.{app, dbFileName}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import com.lightning.walletapp.ln.LNParams.DepthAndDead
import com.lightning.walletapp.ln.wire.{ChannelUpdate, NodeAnnouncement}
import com.lightning.walletapp.ChannelManager
import fr.acinq.eclair.UInt64


object LNParams { me =>
  type DepthAndDead = (Int, Boolean)
  val chainHash = Block.LivenetGenesisBlock.hash
  val channelReserveToFundingRatio = 100
  val localFeatures = "02"
  val globalFeatures = ""
  val minDepth = 1

  val maxToSelfDelay = 2016
  val maxCltvDelta = 7 * 144L
  val minCapacitySat = 300000L
  final val dust = Satoshi(2730L)
  final val minFeeratePerKw = 253
  final val maxHtlcValueMsat = 4000000000L
  final val maxCapacity = Satoshi(16777215L)
  final val minHtlcValue = MilliSatoshi(1000L)

  var db: LNOpenHelper = _
  private[this] var master: ExtendedPrivateKey = _
  lazy val extendedNodeKey = derivePrivateKey(master, hardened(46L) :: hardened(0L) :: Nil)
  lazy val extendedCloudKey = derivePrivateKey(master, hardened(92L) :: hardened(0L) :: Nil)
  // HashingKey is used for creating domain-specific identifiers when using "linkable payment" LNUrl
  lazy val hashingKey = derivePrivateKey(master, hardened(138L) :: 0L :: Nil).privateKey.toBin.data
  // Cloud secret is used to encrypt Olympus and GDrive data, cloud ID is used as identifier
  lazy val cloudSecret = sha256(extendedCloudKey.privateKey.toBin.data)
  lazy val cloudId = sha256(cloudSecret.data)

  lazy val bag = PaymentInfoWrap
  lazy val broadcaster: Broadcaster = ChannelManager
  lazy val nodePublicKey: PublicKey = nodePrivateKey.publicKey
  lazy val nodePrivateKey: PrivateKey = extendedNodeKey.privateKey

  def setup(walletSeed: BinaryData) = {
    master = generate(seed = walletSeed)
    db = new LNOpenHelper(app, dbFileName)
    app.olympus = new OlympusWrap
  }

  def isFeeNotOk(msat: Long, fee: Long, hops: Int) =
    fee > 25000 * (hops + 1) + msat / 50

  def shouldUpdateFee(network: Long, commit: Long) = {
    val mismatch = 2.0 * (network - commit) / (commit + network)
    mismatch < -0.25 || mismatch > 0.25
  }

  def getLinkingKey(domainName: String) = {
    val prefix = crypto.Digests.hmacSha256(hashingKey.toArray, domainName getBytes "UTF-8") take 8
    derivePrivateKey(master, hardened(138L) :: 0L :: BigInt(prefix).toLong :: Nil).privateKey
  }

  def updateExtraHop(upd: ChannelUpdate) = for (chan <- ChannelManager.notClosingOrRefunding) Channel.updateHop(chan, upd)
  def updateFeerate = for (chan <- ChannelManager.notClosingOrRefunding) chan process CMDFeerate(broadcaster.perKwThreeSat)

  def backupFileName = s"blw${chainHash.toString}-${cloudId.toString}.bkup"
  def makeLocalParams(ann: NodeAnnouncement, theirReserve: Long, finalScriptPubKey: BinaryData, idx: Long, isFunder: Boolean) = {
    val Seq(fund, revoke, pay, delay, htlc, sha) = for (ord <- 0L to 5L) yield derivePrivateKey(extendedNodeKey, idx :: ord :: Nil)
    LocalParams(UInt64(maxHtlcValueMsat), theirReserve, toSelfDelay = if (ann.nodeId == JointNode.jointNodeKey) 2880 else 1440,
      maxAcceptedHtlcs = 25, fund.privateKey, revoke.privateKey, pay.privateKey, delay.privateKey, htlc.privateKey,
      finalScriptPubKey, dust, shaSeed = sha256(sha.privateKey.toBin), isFunder)
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