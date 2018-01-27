package com.lightning.wallet

import R.string._
import spray.json._
import org.bitcoinj.core._
import com.lightning.wallet.ln._

import scala.concurrent.duration._
import com.softwaremill.quicklens._
import com.lightning.wallet.lnutils._
import com.lightning.wallet.ln.Tools._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.ImplicitConversions._
import android.app.Application.ActivityLifecycleCallbacks

import collection.JavaConverters.seqAsJavaListConverter
import com.lightning.wallet.lnutils.Connector.CMDStart
import java.util.concurrent.TimeUnit.MILLISECONDS

import com.lightning.wallet.ln.Channel.CLOSING
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.Wallet.BalanceType
import org.bitcoinj.crypto.KeyCrypterScrypt
import fr.acinq.bitcoin.Crypto.PublicKey
import com.google.protobuf.ByteString

import scala.collection.mutable
import android.widget.Toast
import android.os.Bundle

import scala.util.Try
import java.io.File

import com.lightning.wallet.ln.wire.{Init, LightningMessage, NodeAnnouncement}
import com.google.common.util.concurrent.Service.State.{RUNNING, STARTING}
import org.bitcoinj.uri.{BitcoinURI, BitcoinURIParseException}
import android.content.{ClipData, ClipboardManager, Context}
import com.lightning.wallet.Utils.{app, appName}
import org.bitcoinj.wallet.{Protos, Wallet}
import android.app.{Activity, Application}
import com.google.common.net.InetAddresses
import rx.lang.scala.{Observable => Obs}


class WalletApp extends Application { me =>
  lazy val params = org.bitcoinj.params.TestNet3Params.get
  lazy val prefs = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val chainFile = new File(getFilesDir, s"$appName.spvchain")
  lazy val walletFile = new File(getFilesDir, s"$appName.wallet")
  var currentActivity: Option[Activity] = None
  var kit: WalletKit = _

  lazy val plur = getString(lang) match {
    case "eng" | "esp" => (opts: Array[String], num: Long) => if (num == 1) opts(1) else opts(2)
    case "chn" | "jpn" => (phraseOptions: Array[String], num: Long) => phraseOptions(1)
    case "rus" | "ukr" => (phraseOptions: Array[String], num: Long) =>

      val reminder100 = num % 100
      val reminder10 = reminder100 % 10
      if (reminder100 > 10 & reminder100 < 20) phraseOptions(3)
      else if (reminder10 > 1 & reminder10 < 5) phraseOptions(2)
      else if (reminder10 == 1) phraseOptions(1)
      else phraseOptions(3)
  }

  // Various utilities

  def isAlive = if (null == kit) false else kit.state match {
    case STARTING | RUNNING => null != db && null != cloud
    case _ => false
  }

  def plurOrZero(opts: Array[String], number: Long) =
    if (number > 0) plur(opts, number) format number else opts(0)

  def toast(code: Int): Unit = toast(me getString code)
  def toast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def clipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBufferTry = Try(clipboardManager.getPrimaryClip.getItemAt(0).getText.toString)
  def notMixedCase(s: String) = s.toLowerCase == s || s.toUpperCase == s
  def getTo(base58: String) = Address.fromBase58(params, base58)

  Utils.appReference = me
  override def onCreate = wrap(super.onCreate) {
    // These cannot be lazy vals because values may change
    Utils.denom = Utils.denoms apply prefs.getInt(AbstractKit.DENOM_TYPE, 0)
    Utils.fiatName = prefs.getString(AbstractKit.FIAT_TYPE, Utils.strDollar)

    me registerActivityLifecycleCallbacks new ActivityLifecycleCallbacks {
      override def onActivitySaveInstanceState(activity: Activity, state: Bundle) = none
      override def onActivityCreated(activity: Activity, state: Bundle) = currentActivity = Some(activity)
      override def onActivityStarted(activity: Activity) = currentActivity = Some(activity)
      override def onActivityResumed(activity: Activity) = currentActivity = Some(activity)
      override def onActivityPaused(activity: Activity) = currentActivity = None
      override def onActivityDestroyed(activity: Activity) = none
      override def onActivityStopped(activity: Activity) = none
    }
  }

  def setBuffer(text: String) = {
    // Set clipboard contents to given text and notify user via toast
    clipboardManager setPrimaryClip ClipData.newPlainText(appName, text)
    me toast getString(copied_to_clipboard).format(text)
  }

  def encryptWallet(wallet: Wallet, pass: CharSequence) = {
    val salt = ByteString copyFrom KeyCrypterScrypt.randomSalt
    val builder = Protos.ScryptParameters.newBuilder.setSalt(salt)
    val crypter = new KeyCrypterScrypt(builder.setN(65536).build)
    wallet.encrypt(crypter, crypter deriveKey pass)
  }

  object TransData {
    var value: Any = _
    val lnLink = "(?i)(lightning:)?([a-zA-Z0-9]+)\\W*".r
    def recordValue(rawText: String) = value = rawText match {
      case raw if raw startsWith "bitcoin" => new BitcoinURI(params, raw)
      case lnLink(_, body) if notMixedCase(body) => PaymentRequest read body.toLowerCase
      case _ => getTo(rawText)
    }

    def onFail(err: Int => Unit): PartialFunction[Throwable, Unit] = {
      case _: org.bitcoinj.core.WrongNetworkException => err(err_different_net)
      case _: org.bitcoinj.core.AddressFormatException => err(err_address)
      case _: BitcoinURIParseException => err(err_uri)
      case _: Throwable => err(err_general)
    }
  }

  object ChannelManager {
    type ChannelVec = Vector[Channel]
    type RPIOpt = Option[RuntimePaymentInfo]

    val operationalListeners = mutable.Set(broadcaster, bag, StorageWrap, Notificator)
    // Obtain a vector of stored channels which would receive CMDSpent, CMDBestHeight and nothing else
    var all: ChannelVec = for (data <- ChannelWrap.get) yield createChannel(operationalListeners, data)
    def fromNode(of: ChannelVec, ann: NodeAnnouncement): ChannelVec = of.filter(_.data.announce == ann)
    def notClosing: ChannelVec = all.filter(_.state != Channel.CLOSING)

    val chainEventsListener = new TxTracker with BlocksListener {
      override def txConfirmed(tx: Transaction) = for (chan <- notClosing) chan process CMDConfirmed(tx)
      // No matter how many blocks are left on start, we only send a CMD once the last block has been processed
      def tellHeight(left: Int) = if (left < 1) for (chan <- all) chan process CMDBestHeight(broadcaster.currentHeight)
      override def onBlocksDownloaded(peer: Peer, block: Block, fb: FilteredBlock, left: Int) = tellHeight(left)
      override def onChainDownloadStarted(peer: Peer, left: Int) = tellHeight(left)

      override def coinsSent(tx: Transaction) = {
        // We always attempt to extract a payment preimage
        // just assuming any incoming tx may contain it

        val spent = CMDSpent(tx)
        bag.extractPreimage(spent.tx)
        for (chan <- all) chan process spent
      }
    }

    val socketEventsListener = new ConnectionListener {
      def reConnect(cs: ChannelVec, ann: NodeAnnouncement) = if (cs.nonEmpty) {
        // Immediately inform affected channels and try to reconnect in 5 seconds
        Obs.just(ann).delay(5.seconds).subscribe(ConnectionManager.connectTo, none)
        cs.foreach(_ process CMDOffline)
      }

      override def onMessage(ann: NodeAnnouncement, msg: LightningMessage) = fromNode(notClosing, ann).foreach(_ process msg)
      override def onOperational(ann: NodeAnnouncement, their: Init) = fromNode(notClosing, ann).foreach(_ process CMDOnline)
      override def onTerminalError(ann: NodeAnnouncement) = fromNode(notClosing, ann).foreach(_ process CMDShutdown)
      override def onDisconnect(ann: NodeAnnouncement) = reConnect(fromNode(notClosing, ann), ann)
    }

    def initConnect = for (chan <- notClosing) ConnectionManager connectTo chan.data.announce
    def createChannel(interested: mutable.Set[ChannelListener], bootstrap: ChannelData) = new Channel {
      def STORE(hasCommitments: HasCommitments) = runAnd(hasCommitments)(ChannelWrap put hasCommitments)
      // First add listeners, then specifically call doProcess so it runs on current thread
      val listeners = interested
      doProcess(bootstrap)

      def SEND(lightningMessage: LightningMessage) =
        ConnectionManager.connections.get(data.announce)
          .foreach(_.handler process lightningMessage)

      def CLOSEANDWATCH(close: ClosingData) = {
        val commits = close.localCommit.map(_.commitTx) ++ close.remoteCommit.map(_.commitTx) ++ close.nextRemoteCommit.map(_.commitTx)
        // Collect all the commit txs output publicKeyScripts and watch these scripts locally for future possible payment preimages
        kit.watchScripts(commits.flatMap(_.txOut).map(_.publicKeyScript) map bitcoinLibScript2bitcoinjScript)
        // Ask server for child txs which spend our commit txs outputs and extract preimages from them
        cloud.connector.getChildTxs(commits).foreach(_ foreach bag.extractPreimage, Tools.errlog)
        BECOME(STORE(close), CLOSING)

        close.tier12States.map(_.txn) match {
          case Nil => Tools log "Closing channel does not have tier 1-2 transactions"
          case txs => cloud doProcess CloudAct(txs.toJson.toString.hex, Nil, "txs/schedule")
        }
      }
    }

    // Get routes from maintenance server and update RoutingData if they exist
    // if payment request contains extra routing info then we also ask for assisted routes
    // once direct route and assisted routes are fetched we combine them into single sequence
    // and then we make an onion out of the first available route while saving the rest
    var getOutPaymentObs: RuntimePaymentInfo => Obs[RPIOpt] = outPaymentObs

    def outPaymentObs(rpi: RuntimePaymentInfo) =
      Obs from all.find(_.isOperational) flatMap { chan =>
        def findRoutes(target: PublicKey) = chan.data.announce.nodeId match {
          case peerNodeId if peerNodeId == target => Obs just Vector(Vector.empty)
          case _ => cloud.connector.findRoutes(rpi.rd, chan.data.announce.nodeId, target)
        }

        def augmentAssisted(tag: RoutingInfoTag) = for {
          publicRoutes <- findRoutes(tag.route.head.nodeId)
        } yield publicRoutes.map(_ ++ tag.route)

        val allAssisted = Obs.zip(Obs from rpi.pr.routingInfo map augmentAssisted)
        findRoutes(rpi.pr.nodeId).zipWith(allAssisted orElse Vector.empty) { case direct \ assisted =>
          // We have got direct and assisted routes, now combine them into single vector and proceed
          completeRPI(rpi.modify(_.rd.routes) setTo direct ++ assisted.flatten)
        }
      }
  }

  abstract class WalletKit extends AbstractKit {
    type ScriptSeq = Seq[org.bitcoinj.script.Script]
    def blockingSend(tx: Transaction) = nonBlockingSend(tx).get.toString
    def nonBlockingSend(tx: Transaction) = peerGroup.broadcastTransaction(tx, 1).broadcast
    def watchFunding(cs: Commitments) = watchScripts(cs.commitInput.txOut.publicKeyScript :: Nil)
    def watchScripts(scripts: ScriptSeq) = wallet addWatchedScripts scripts.asJava
    def conf0Balance = wallet getBalance BalanceType.ESTIMATED_SPENDABLE
    def conf1Balance = wallet getBalance BalanceType.AVAILABLE_SPENDABLE
    def currentAddress = wallet currentAddress KeyPurpose.RECEIVE_FUNDS
    def shutDown = none

    def useCheckPoints(time: Long) = {
      val pts = getAssets open "checkpoints-testnet.txt"
      CheckpointManager.checkpoint(params, pts, store, time)
    }

    def decryptSeed(pass: String) = wallet.getKeyCrypter match { case crypter =>
      wallet.getKeyChainSeed.decrypt(crypter, pass, crypter deriveKey pass)
    }

    def setupAndStartDownload = {
      wallet addCoinsSentEventListener Vibr
      wallet addCoinsReceivedEventListener Vibr
      wallet addTransactionConfidenceEventListener ChannelManager.chainEventsListener
      wallet addCoinsSentEventListener ChannelManager.chainEventsListener
      wallet.autosaveToFile(walletFile, 400, MILLISECONDS, null)
      wallet.watchMode = true

      peerGroup addPeerDiscovery new DnsDiscovery(params)
      peerGroup.setMinRequiredProtocolVersion(70015)
      peerGroup.setUserAgent(appName, "0.01")
      peerGroup.setDownloadTxDependencies(3)
      peerGroup.setPingIntervalMsec(10000)
      peerGroup.setMaxConnections(5)
      peerGroup.addWallet(wallet)

      ConnectionManager.listeners += ChannelManager.socketEventsListener
      startBlocksDownload(ChannelManager.chainEventsListener)
      ChannelManager.initConnect
      RatesSaver.saveObject

      // This is needed to clear possible leftover acts
      // such a situation may happen if we had no connection
      if (cloud.data.acts.nonEmpty) cloud doProcess CMDStart
    }
  }
}

object Vibr extends TxTracker {
  override def coinsSent(tx: Transaction) = vibrate(btcBroadcasted)
  override def coinsReceived(tx: Transaction) = vibrate(btcBroadcasted)
  def vibrate(pattern: Pattern) = if (null != vib && vib.hasVibrator) vib.vibrate(pattern, -1)
  lazy val vib = app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  val btcBroadcasted = Array(0L, 75, 250, 75, 250)
  val lnSettled = Array(0L, 85, 200)
  type Pattern = Array[Long]
}