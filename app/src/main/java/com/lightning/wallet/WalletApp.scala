package com.lightning.wallet

import R.string._
import spray.json._
import org.bitcoinj.core._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import com.lightning.wallet.lnutils._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Tools._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.muddzdev.styleabletoastlibrary.StyleableToast
import collection.JavaConverters.seqAsJavaListConverter
import java.util.concurrent.TimeUnit.MILLISECONDS
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.Wallet.BalanceType
import org.bitcoinj.crypto.KeyCrypterScrypt
import fr.acinq.bitcoin.Crypto.PublicKey
import com.google.protobuf.ByteString
import fr.acinq.bitcoin.BinaryData
import java.net.InetSocketAddress
import android.app.Application
import scala.util.Try
import java.io.File

import com.google.common.util.concurrent.Service.State.{RUNNING, STARTING}
import org.bitcoinj.uri.{BitcoinURI, BitcoinURIParseException}
import android.content.{ClipData, ClipboardManager, Context}
import org.bitcoinj.wallet.{Protos, SendRequest, Wallet}
import com.lightning.wallet.Utils.{app, appName}
import rx.lang.scala.{Observable => Obs}


class WalletApp extends Application { me =>
  lazy val params = org.bitcoinj.params.TestNet3Params.get
  lazy val prefs = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val chainFile = new File(getFilesDir, s"$appName.spvchain")
  lazy val walletFile = new File(getFilesDir, s"$appName.wallet")
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
    if (number > 0) plur(opts, number) format number
    else opts(0)

  def toast(code: Int): Unit = toast(me getString code)
  def toast(message: String): Unit = StyleableToast.makeText(me, message, R.style.infoToast).show
  def clipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBufferTry = Try(clipboardManager.getPrimaryClip.getItemAt(0).getText.toString)
  def notMixedCase(s: String) = s.toLowerCase == s || s.toUpperCase == s
  def getTo(base58: String) = Address.fromBase58(params, base58)

  Utils.appReference = me
  override def onCreate = wrap(super.onCreate) {
    // These cannot be lazy vals because values may change
    Utils.denom = Utils denoms prefs.getInt(AbstractKit.DENOM_TYPE, 0)
    Utils.fiatName = prefs.getString(AbstractKit.FIAT_TYPE, Utils.strDollar)
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
    var value: Any = new String
    val lnLink = "(?i)(lightning:)?([a-zA-Z0-9]+)\\W*".r
    val nodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:\\.\\-_]+):([0-9]+)".r

    def recordValue(rawText: String) = value = rawText match {
      case raw if raw startsWith "bitcoin" => new BitcoinURI(params, raw)
      case lnLink(_, body) if notMixedCase(body) => PaymentRequest read body.toLowerCase
      case nodeLink(key, host, port) => NodeAnnouncement(null, BinaryData.empty, 0L, PublicKey(key),
        (0L.toByte, 0L.toByte, 0L.toByte), key take 16, new InetSocketAddress(host, port.toInt) :: Nil)

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
    val operationalListeners = Set(broadcaster, bag, StorageWrap)
    // All stored channels which would receive CMDSpent, CMDBestHeight and nothing else
    var all = for (data <- ChannelWrap.get) yield createChannel(operationalListeners, data)

    def fromNode(of: Vector[Channel], ann: NodeAnnouncement) = for (c <- of if c.data.announce == ann) yield c
    def canSend(msat: Long) = for (c <- all if c.state == Channel.OPEN && isOperational(c) && estimateCanSend(c) > msat) yield c
    def notClosingOrRefunding = for (c <- all if c.state != Channel.CLOSING && c.state != Channel.REFUNDING) yield c
    def notClosing = for (c <- all if c.state != Channel.CLOSING) yield c

    def activeInFlightHashes = notClosingOrRefunding.flatMap(inFlightOutgoingHtlcs).map(_.add.paymentHash)
    def frozenInFlightHashes = all.diff(notClosingOrRefunding).flatMap(inFlightOutgoingHtlcs).map(_.add.paymentHash)
    def initConnect = for (chan <- notClosing) ConnectionManager connectTo chan.data.announce

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
        all.foreach(_ process spent)
      }
    }

    val socketEventsListener = new ConnectionListener {
      override def onMessage(ann: NodeAnnouncement, msg: LightningMessage) = msg match {
        // Channel level Error will fall under ChannelMessage case but node level Error should be sent to all chans
        case err: Error if err.channelId == BinaryData("00" * 32) => fromNode(notClosing, ann).foreach(_ process err)
        case cm: ChannelMessage => notClosing.find(chan => chan(_.channelId) contains cm.channelId).foreach(_ process cm)
        case cu: ChannelUpdate => fromNode(notClosing, ann).foreach(_ process cu)
        case _ =>
      }

      override def onOperational(ann: NodeAnnouncement, their: Init) = fromNode(notClosing, ann).foreach(_ process CMDOnline)
      override def onTerminalError(ann: NodeAnnouncement) = fromNode(notClosing, ann).foreach(_ process CMDShutdown)
      override def onDisconnect(ann: NodeAnnouncement) = maybeReconnect(fromNode(notClosing, ann), ann)

      def maybeReconnect(cs: Vector[Channel], ann: NodeAnnouncement) = if (cs.nonEmpty) {
        // Immediately inform affected channels and try to reconnect back again in 5 seconds
        Obs.just(ann).delay(5.seconds).subscribe(ConnectionManager.connectTo, none)
        cs.foreach(_ process CMDOffline)
      }
    }

    def createChannel(initialListeners: Set[ChannelListener], bootstrap: ChannelData) = new Channel {
      def STORE(hasCommitmentsData: HasCommitments) = runAnd(hasCommitmentsData)(ChannelWrap put hasCommitmentsData)
      def SEND(msg: LightningMessage) = ConnectionManager.connections.get(data.announce).foreach(_.handler process msg)
      // First add listeners, then specifically call doProcess so it runs on current thread
      listeners = initialListeners
      doProcess(bootstrap)

      def CLOSEANDWATCH(cd: ClosingData) = {
        val commits = cd.localCommit.map(_.commitTx) ++ cd.remoteCommit.map(_.commitTx) ++ cd.nextRemoteCommit.map(_.commitTx)
        // Collect all the commit txs publicKeyScripts and watch these scripts locally for future possible payment preimages
        kit.watchScripts(commits.flatMap(_.txOut).map(_.publicKeyScript) map bitcoinLibScript2bitcoinjScript)
        // Ask server for child txs which spend our commit txs outputs and extract preimages from them
        cloud.connector.getChildTxs(commits).foreach(_ foreach bag.extractPreimage, Tools.errlog)
        BECOME(STORE(cd), CLOSING)

        cd.tier12States.map(_.txn) match {
          case Nil => Tools log "Closing channel does not have tier 1-2 transactions"
          case txs => cloud doProcess CloudAct(txs.toJson.toString.hex, Nil, "txs/schedule")
        }
      }
    }

    def withRoutesAndOnionRPI(rpi: RuntimePaymentInfo) = {
      val isFrozen = frozenInFlightHashes.contains(rpi.pr.paymentHash)
      if (isFrozen) Obs error new LightningException(me getString err_ln_frozen)
      else withRoutesAndOnionRPIFrozenAllowed(rpi)
    }

    def withRoutesAndOnionRPIFrozenAllowed(rpi: RuntimePaymentInfo) = {
      val sources = canSend(rpi.firstMsat).map(_.data.announce.nodeId).toSet
      val paymentRecordOpt = bag.getPaymentInfo(rpi.pr.paymentHash).toOption
      val isFulfilled = paymentRecordOpt.exists(_.actualStatus == SUCCESS)
      val isInFlight = activeInFlightHashes.contains(rpi.pr.paymentHash)

      if (isInFlight) Obs error new LightningException(me getString err_ln_in_flight)
      else if (isFulfilled) Obs error new LightningException(me getString err_ln_fulfilled)
      else if (sources.isEmpty) Obs error new LightningException(me getString err_ln_no_route)
      else completeRPI(sources, rpi)
    }

    def completeRPI(sources: Set[PublicKey], rpi: RuntimePaymentInfo): Obs[FullOrEmptyRPI] = {
      def findRemoteRoutes(target: PublicKey) = cloud.connector.findRoutes(rpi.rd, sources, target)
      def findRoutes(target: PublicKey) = if (sources contains target) Obs just Vector(Vector.empty) else findRemoteRoutes(target)
      def findAssisted(tag: RoutingInfoTag) = findRoutes(tag.route.head.nodeId).map(rts => for (pub <- rts) yield pub ++ tag.route)
      // If payment request contains extra routing info then we ask for assisted routes, otherwise we directly ask for recipient id
      val result = if (rpi.pr.routingInfo.isEmpty) findRoutes(rpi.pr.nodeId) else Obs from rpi.pr.routingInfo flatMap findAssisted
      // Update RPI with routes and then we can make an onion out of the first available route while saving the rest
      for (routes <- result) yield useFirstRoute(routes, rpi)
    }

    def send(rpi: RuntimePaymentInfo, noRouteLeft: RuntimePaymentInfo => Unit): Unit = {
      // Find a local channel which has enough funds, is online and belongs to a correct node
      // empty used route means we're sending to our peer and should use it's nodeId as a target
      val target = if (rpi.rd.usedRoute.isEmpty) rpi.pr.nodeId else rpi.rd.usedRoute.head.nodeId
      val channelOpt = canSend(rpi.firstMsat).find(_.data.announce.nodeId == target)

      channelOpt match {
        case Some(targetChannel) => targetChannel process rpi
        case None => sendEither(useRoutesLeft(rpi), noRouteLeft)
      }
    }

    def sendEither(foeRPI: FullOrEmptyRPI, noRouteLeft: RuntimePaymentInfo => Unit) = foeRPI match {
      case Right(rpiWithValidPaymentRoutePresent) => send(rpiWithValidPaymentRoutePresent, noRouteLeft)
      case Left(rpiWithEmptyPaymentRoute) => noRouteLeft(rpiWithEmptyPaymentRoute)
    }
  }

  abstract class WalletKit extends AbstractKit {
    type ScriptSeq = Seq[org.bitcoinj.script.Script]
    def blockingSend(unsigned: SendRequest): String = blockingSend(sign(unsigned).tx)
    def blockingSend(transaction: Transaction): String = nonBlockingSend(transaction).get.toString
    def nonBlockingSend(transaction: Transaction) = peerGroup.broadcastTransaction(transaction, 1).broadcast
    def watchFunding(cs: Commitments) = watchScripts(cs.commitInput.txOut.publicKeyScript :: Nil)
    def watchScripts(scripts: ScriptSeq) = wallet addWatchedScripts scripts.asJava
    def conf0Balance = wallet getBalance BalanceType.ESTIMATED_SPENDABLE
    def conf1Balance = wallet getBalance BalanceType.AVAILABLE_SPENDABLE
    def currentAddress = wallet currentAddress KeyPurpose.RECEIVE_FUNDS
    def shutDown = none

    def sign(unsigned: SendRequest) = {
      // Create a tx ready for broadcast
      wallet finalizeReadyTx unsigned
      unsigned.tx.verify
      unsigned
    }

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
      peerGroup.setUserAgent(appName, "0.06")
      peerGroup.setDownloadTxDependencies(0)
      peerGroup.setPingIntervalMsec(10000)
      peerGroup.setMaxConnections(5)
      peerGroup.addWallet(wallet)

      // Mark all abandoned payments as failed right away
      ConnectionManager.listeners += ChannelManager.socketEventsListener
      startBlocksDownload(ChannelManager.chainEventsListener)
      PaymentInfoWrap.markFailedAndFrozen
      ChannelManager.initConnect
      RatesSaver.initialize
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