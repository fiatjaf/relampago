package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.Scripts._
import fr.acinq.bitcoin.DeterministicWallet._
import com.lightning.walletapp.Utils.{app, dbFileName}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import com.lightning.walletapp.ln.LNParams.DepthAndDead
import com.lightning.walletapp.ln.wire.NodeAnnouncement
import com.lightning.walletapp.ChannelManager
import com.lightning.walletapp.ln.Tools.Bytes
import java.io.ByteArrayInputStream
import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector
import java.nio.ByteOrder


object LNParams {
  type DepthAndDead = (Int, Boolean)
  val localFeatures = ByteVector.fromValidHex("8a") // data_loss_protect, channel_range_queries
  val globalFeatures = ByteVector.fromValidHex("0200") // variable_length_onion
  val chainHash = Block.TestnetGenesisBlock.hash

  val minDepth = 1
  val blocksPerDay = 144
  val minCapacityMsat = 200000000L
  val channelReserveToFundingRatio = 200 // 0.5%

  val minHostedCltvDelta = blocksPerDay * 3
  val minHostedOnChainRefundSat = 1000000L
  val minHostedLiabilityBlockdays = 1000
  val maxHostedBlockHeight = 100000L

  final val dust = Satoshi(546)
  final val maxToSelfDelay = 2016
  final val minFeeratePerKw = 253
  final val maxCltvDelta = blocksPerDay * 7L
  final val maxCapacity = MilliSatoshi(16777215000L)

  var db: LNOpenHelper = _
  private[this] var master: ExtendedPrivateKey = _
  lazy val extendedNodeKey = derivePrivateKey(master, hardened(46L) :: hardened(0L) :: Nil)
  lazy val extendedCloudKey = derivePrivateKey(master, hardened(92L) :: hardened(0L) :: Nil)
  // HashingKey is used for creating domain-specific identifiers when using "linkable payment" LNUrl
  lazy val hashingKey = derivePrivateKey(master, hardened(138L) :: 0L :: Nil).privateKey.toBin
  // Cloud secret is used to encrypt Olympus data, cloud ID is used as identifier
  lazy val cloudSecret = sha256(extendedCloudKey.privateKey.toBin)
  lazy val cloudId = sha256(cloudSecret)

  lazy val broadcaster: Broadcaster = ChannelManager
  lazy val bag: PaymentInfoBag with ChannelListener = PaymentInfoWrap
  lazy val nodePrivateKey: PrivateKey = extendedNodeKey.privateKey
  lazy val nodePublicKey: PublicKey = nodePrivateKey.publicKey

  def setup(seed: Bytes) = {
    master = generate(ByteVector view seed)
    db = new LNOpenHelper(app, dbFileName)
    app.olympus = new OlympusWrap
  }

  def hopFee(msat: Long, base: Long, proportional: Long) = base + (proportional * msat) / 1000000L
  def maxAcceptableFee(msat: Long, hops: Int, percent: Long = 100L) = 25000 * (hops + 1) + msat / percent

  def estTotalRouteFee(route: PaymentRoute) = totalRouteFee(route, 10000000L)
  def totalRouteFee(route: PaymentRoute, msat: Long) = route.reverse.foldLeft(msat) {
    case amount \ hop => amount + hopFee(amount, hop.feeBaseMsat, hop.feeProportionalMillionths)
  } - msat

  def isFeeBreach(route: PaymentRoute, msat: Long, percent: Long) =
    totalRouteFee(route, msat) > maxAcceptableFee(msat, route.size, percent)

  def shouldUpdateFee(network: Long, commit: Long) = {
    val mismatch = 2.0 * (network - commit) / (commit + network)
    mismatch < -0.25 || mismatch > 0.25
  }

  def backupFileName = s"blw${chainHash.toHex}-${cloudId.toHex}.bkup"
  def updateFeerate = for (chan <- ChannelManager.all) chan process CMDFeerate(broadcaster.perKwThreeSat)

  def makeLocalParams(ann: NodeAnnouncement, theirReserve: Long, finalScriptPubKey: ByteVector, fundKey: PrivateKey, isFunder: Boolean) = {
    // It's always possible to re-derive all secret keys because a keyPath is generated from funding pubKey which will be present on a blockchain
    val Seq(revocationSecret, paymentKey, delayedPaymentKey, htlcKey, shaSeed) = makeChanKeys(fundKey.publicKey)
    LocalParams(UInt64(maxCapacity.amount), theirReserve, toSelfDelay = 2016, maxAcceptedHtlcs = 25, fundKey,
      revocationSecret.privateKey, paymentKey.privateKey, delayedPaymentKey.privateKey, htlcKey.privateKey,
      finalScriptPubKey, dust, sha256(shaSeed.privateKey.toBin), isFunder)
  }

  def makeChanKeys(fundKey: PublicKey) = {
    val channelKeyPath: Vector[Long] = makeKeyPath(material = fundKey.hash160)
    for (idx <- 1L to 5L) yield derivePrivateKey(extendedNodeKey, channelKeyPath :+ idx)
  }

  def makeLinkingKey(domain: String): PrivateKey = {
    val material = crypto.Mac32.hmac256(key = hashingKey, message = domain)
    derivePrivateKey(extendedNodeKey, makeKeyPath(material) :+ 0L).privateKey
  }

  def makeKeyPath(material: ByteVector): Vector[Long] = {
    require(material.size > 15, "Material size must be at least 16")
    val stream = new ByteArrayInputStream(material.slice(0, 16).toArray)
    def generateNewBranch = Protocol.uint32(stream, ByteOrder.BIG_ENDIAN)
    Vector.fill(4)(generateNewBranch)
  }
}

object ChanErrorCodes {
  import com.lightning.walletapp.R.string._
  val ERR_LOCAL_AMOUNT_HIGH = err_ln_local_amount_high
  val ERR_REMOTE_AMOUNT_HIGH = err_ln_remote_amount_high
  val ERR_REMOTE_AMOUNT_LOW = err_ln_remote_amount_low
  val ERR_TOO_MANY_HTLC = err_ln_too_many

  final val ERR_HOSTED_WRONG_BLOCKDAY = ByteVector.fromValidHex("0001")
  final val ERR_HOSTED_WRONG_LOCAL_SIG = ByteVector.fromValidHex("0002")
  final val ERR_HOSTED_WRONG_REMOTE_SIG = ByteVector.fromValidHex("0003")
  final val ERR_HOSTED_UPDATE_CLTV_TOO_LOW = ByteVector.fromValidHex("0004")
  final val ERR_HOSTED_TOO_MANY_STATE_UPDATES = ByteVector.fromValidHex("0005")
  final val ERR_HOSTED_TIMED_OUT_OUTGOING_HTLC = ByteVector.fromValidHex("0006")
  final val ERR_HOSTED_IN_FLIGHT_HTLC_WHILE_RESTORING = ByteVector.fromValidHex("0007")
  final val ERR_HOSTED_CHANNEL_DENIED = ByteVector.fromValidHex("0008")

  def translateTag(error: wire.Error) = error.data take 2 match {
    case ERR_HOSTED_WRONG_BLOCKDAY => new LightningException(app getString err_ln_hosted_wrong_blockday)
    case ERR_HOSTED_WRONG_LOCAL_SIG => new LightningException(app getString err_ln_hosted_wrong_local_sig)
    case ERR_HOSTED_WRONG_REMOTE_SIG => new LightningException(app getString err_ln_hosted_wrong_remote_sig)
    case ERR_HOSTED_UPDATE_CLTV_TOO_LOW => new LightningException(app getString err_ln_hosted_update_cltv_too_low)
    case ERR_HOSTED_TOO_MANY_STATE_UPDATES => new LightningException(app getString err_ln_hosted_too_many_state_updates)
    case ERR_HOSTED_TIMED_OUT_OUTGOING_HTLC => new LightningException(app getString err_ln_hosted_timed_out_outgoing_htlc)
    case ERR_HOSTED_IN_FLIGHT_HTLC_WHILE_RESTORING => new LightningException(app getString err_ln_hosted_in_flight_htlc_while_restoring)
    case ERR_HOSTED_CHANNEL_DENIED => new LightningException(app getString err_ln_hosted_channel_denied)
    case _ => error.exception
  }
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
case class ShowDelayed(parent: (DepthAndDead, Long), txn: Transaction, commitTx: Transaction, fee: Satoshi, amount: Satoshi) extends DelayedPublishStatus

trait Broadcaster extends ChannelListener {
  def getTx(txid: ByteVector): Option[org.bitcoinj.core.Transaction]
  def getStatus(txid: ByteVector): DepthAndDead

  def currentBlockDay: Int
  def currentHeight: Int

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

  def csvShowDelayed(t1: TransactionWithInputInfo, t2: TransactionWithInputInfo, commitTx: Transaction) =
    ShowDelayed(parent = csv(t1.tx, t2.tx), t2.tx, commitTx, fee = t1 -- t2, t2.tx.allOutputsAmount)
}