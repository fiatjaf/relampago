package com.lightning.walletapp

import R.string._
import spray.json._
import org.bitcoinj.core._
import scala.concurrent.duration._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import rx.lang.scala.{Observable => Obs}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import java.net.{InetAddress, InetSocketAddress}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}
import android.content.{ClipData, ClipboardManager, Context}
import com.google.common.util.concurrent.Service.State.{RUNNING, STARTING}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.RGB
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import collection.JavaConverters.seqAsJavaListConverter
import com.lightning.walletapp.lnutils.olympus.CloudAct
import java.util.concurrent.TimeUnit.MILLISECONDS
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.Wallet.BalanceType
import fr.acinq.bitcoin.Hash.Zeroes
import org.bitcoinj.uri.BitcoinURI
import fr.acinq.bitcoin.Crypto
import android.app.Application
import java.util.Collections
import android.widget.Toast
import android.net.Uri
import scala.util.Try
import java.io.File


class WalletApp extends Application { me =>
  lazy val params = org.bitcoinj.params.MainNetParams.get
  lazy val prefs = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val walletFile = new File(getFilesDir, walletFileName)
  lazy val chainFile = new File(getFilesDir, chainFileName)
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

  def toast(code: Int): Unit = toast(me getString code)
  def toast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def clipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def isAlive = if (null == kit) false else kit.state match { case STARTING | RUNNING => null != db case _ => false }
  def plurOrZero(opts: Array[String], number: Long) = if (number > 0) plur(opts, number) format number else opts(0)
  def getBufferTry = Try(clipboardManager.getPrimaryClip.getItemAt(0).getText.toString)
  def toAddress(rawText: String) = Address.fromString(app.params, rawText)

  Utils.appReference = me
  override def onCreate = wrap(super.onCreate) {
    // These cannot be lazy vals because values may change
    Utils.denom = Utils denoms prefs.getInt(AbstractKit.DENOM_TYPE, 0)
    Utils.fiatName = prefs.getString(AbstractKit.FIAT_TYPE, Utils.strDollar)
  }

  def setBuffer(text: String) = {
    clipboardManager setPrimaryClip ClipData.newPlainText("wallet", text)
    me toast getString(copied_to_clipboard).format(text)
  }

  def mkNodeAnnouncement(nodeId: PublicKey, host: String, port: Int) = {
    val nodeAddress: InetSocketAddress = new InetSocketAddress(host, port)
    val nodeColors: RGB = Tuple3(Byte.MinValue, Byte.MinValue, Byte.MinValue)
    val sig = Crypto encodeSignature Crypto.sign(random getBytes 32, LNParams.nodePrivateKey)
    NodeAnnouncement(sig, "", 0L, nodeId, nodeColors, host, NodeAddress(nodeAddress) :: Nil)
  }

  object TransData {
    var value: Any = _
    private[this] val prefixes = PaymentRequest.prefixes.values mkString "|"
    private[this] val nodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:\\.\\-_]+):([0-9]+)".r
    private[this] val lnLink = s"(?im).*?($prefixes)([0-9]{1,}[a-z0-9]+){1}".r.unanchored

    def recordValue(rawText: String) = value = rawText match {
      case _ if rawText startsWith "bitcoin" => new BitcoinURI(params, rawText)
      case _ if rawText startsWith "BITCOIN" => new BitcoinURI(params, rawText.toLowerCase)
      case nodeLink(key, host, port) => mkNodeAnnouncement(PublicKey(key), host, port.toInt)
      case lnLink(pre, x) => PaymentRequest read s"$pre$x".toLowerCase
      case _ => toAddress(rawText)
    }
  }

  object ChannelManager {
    var currentBlocksLeft = Int.MaxValue
    val CMDLocalShutdown = CMDShutdown(None)
    val operationalListeners = Set(broadcaster, bag, GossipCatcher)
    // All stored channels which would receive CMDSpent, CMDBestHeight and nothing else
    var all: Vector[Channel] = for (data <- ChannelWrap.get) yield createChannel(operationalListeners, data)
    def fromNode(of: Vector[Channel], nodeId: PublicKey) = for (c <- of if c.data.announce.nodeId == nodeId) yield c
    def notClosingOrRefunding = for (c <- all if c.state != Channel.CLOSING && c.state != Channel.REFUNDING) yield c
    def notClosing = for (c <- all if c.state != Channel.CLOSING) yield c

    def canSendNow(msat: Long) = for {
      // Open AND online AND can handle amount + on-chain fee
      chan <- all if isOperational(chan) && chan.state == OPEN
      feerateMsat <- chan(_.localCommit.spec.feeratePerKw * 1000L)
      if estimateCanSend(chan) - feerateMsat >= msat
    } yield chan

    def frozenInFlightHashes = for {
      chan <- all if chan.state == Channel.CLOSING
      theirDust <- chan(_.remoteParams.dustLimitSatoshis * 1000L).toList
      // Frozen means a channel has been broken and we have a non-dust HTLC
      // which will either be taken by peer with us getting a payment preimage
      // or it will eventually be taken by us and thus fulfilled as on-chain tx
      htlc <- inFlightHtlcs(chan) if htlc.add.amountMsat > theirDust
    } yield htlc.add.paymentHash

    def activeInFlightHashes = notClosingOrRefunding.flatMap(inFlightHtlcs).map(_.add.paymentHash)
    def initConnect = for (chan <- notClosing) ConnectionManager connectTo chan.data.announce

    val socketEventsListener = new ConnectionListener {
      override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
        // Ignore routing messages except ChannelUpdate since it may contain payment parameters
        case chanUpdate: ChannelUpdate => fromNode(notClosing, nodeId).foreach(_ process chanUpdate)
        case error: Error if error.channelId == Zeroes => fromNode(notClosing, nodeId).foreach(_ process error)
        case m: ChannelMessage => notClosing.find(chan => chan(_.channelId) contains m.channelId).foreach(_ process m)
        case _ =>
      }

      override def onOperational(nodeId: PublicKey, their: Init) = fromNode(notClosing, nodeId).foreach(_ process CMDOnline)
      override def onTerminalError(nodeId: PublicKey) = fromNode(notClosing, nodeId).foreach(_ process CMDLocalShutdown)
      override def onIncompatible(nodeId: PublicKey) = onTerminalError(nodeId)

      override def onDisconnect(nodeId: PublicKey) = if (fromNode(notClosing, nodeId).nonEmpty) {
        val delayedRetryAttempt = Obs from ConnectionManager.connections.get(nodeId) delay 5.seconds
        delayedRetryAttempt.subscribe(worker => ConnectionManager connectTo worker.ann, none)
        fromNode(notClosing, nodeId).foreach(_ process CMDOffline)
      }
    }

    val chainEventsListener = new TxTracker with BlocksListener {
      override def txConfirmed(txj: Transaction) = for (c <- notClosing) c process CMDConfirmed(txj)
      override def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = onChainDownload(left)
      override def onChainDownloadStarted(peer: Peer, left: Int) = onChainDownload(left)

      def onChainDownload(blocksLeft: Int) = {
        // Should wait until all blocks are downloaded
        // otherwise peers may lie about current height
        currentBlocksLeft = blocksLeft

        if (currentBlocksLeft < 1) {
          // Send out pending payments and report best height to channels
          for (c <- all) c process CMDBestHeight(broadcaster.currentHeight)
          PaymentInfoWrap.resolvePending
        }
      }

      def onCoinsSent(w: Wallet, txj: Transaction, a: Coin, b: Coin) = {
        // Always attempt to extract a payment preimage by simply assuming
        // any incoming tx may contain it, also send all txs to chans

        val cmdSpent = CMDSpent(txj)
        for (c <- all) c process cmdSpent
        bag.extractPreimage(cmdSpent.tx)
      }
    }

    def createChannel(initialListeners: Set[ChannelListener], bootstrap: ChannelData) = new Channel { self =>
      def SEND(m: LightningMessage) = for (w <- ConnectionManager.connections get data.announce.nodeId) w.handler process m
      def STORE(data: HasCommitments) = runAnd(data)(ChannelWrap put data)

      def CLOSEANDWATCH(cd: ClosingData) = {
        val tier12txs = for (state <- cd.tier12States) yield state.txn
        if (tier12txs.nonEmpty) OlympusWrap tellClouds CloudAct(tier12txs.toJson.toString.hex, Nil, "txs/schedule")
        repeat(OlympusWrap getChildTxs cd.commitTxs.map(_.txid), pickInc, 7 to 8).foreach(_ foreach bag.extractPreimage, none)
        // Collect all the commit txs publicKeyScripts and watch these scripts locally for future possible payment preimages
        kit.wallet.addWatchedScripts(kit closingPubKeyScripts cd)
        BECOME(STORE(cd), CLOSING)
      }

      def ASKREFUNDTX(ref: RefundingData) = {
        // Failsafe check in case if we are still in REFUNDING state after app is restarted
        val txsObs = OlympusWrap getChildTxs Seq(ref.commitments.commitInput.outPoint.txid)
        txsObs.foreach(txs => for (tx <- txs) self process CMDSpent(tx), Tools.errlog)
      }

      def ASKREFUNDPEER(some: HasCommitments, point: Point) = {
        val msg = "please publish your local commitment" getBytes "UTF-8"
        val ref = RefundingData(some.announce, Some(point), some.commitments)
        val fundingScript = some.commitments.commitInput.txOut.publicKeyScript

        // Start watching for remote spend and ask peer to broadcast it
        app.kit.wallet.addWatchedScripts(Collections singletonList fundingScript)
        BECOME(STORE(ref), REFUNDING) SEND Error(ref.commitments.channelId, msg)
      }

      // First add listeners, then call
      // doProcess on current thread
      listeners = initialListeners
      doProcess(bootstrap)
    }

    def checkIfSendable(rd: RoutingData) = {
      val isDone = bag.getPaymentInfo(rd.pr.paymentHash).filter(_.actualStatus == SUCCESS)
      if (activeInFlightHashes contains rd.pr.paymentHash) Left(me getString err_ln_in_flight)
      else if (frozenInFlightHashes contains rd.pr.paymentHash) Left(me getString err_ln_frozen)
      else if (canSendNow(rd.firstMsat).isEmpty) Left(me getString err_ln_no_chan)
      else if (isDone.isSuccess) Left(me getString err_ln_fulfilled)
      else Right(rd)
    }

    def fetchRoutes(rd: RoutingData) = {
      // Double ordering: first randomize channels without payments, then order those with
      val free \ busy = canSendNow(rd.firstMsat).partition(chan => inFlightHtlcs(chan).isEmpty)
      val peers = scala.util.Random.shuffle(free) ++ busy.sortBy(chan => inFlightHtlcs(chan).size)
      val from = for (chan <- peers) yield chan.data.announce.nodeId

      def getRoutes(targetId: PublicKey) = from contains targetId match {
        case false if rd.useCache => RouteWrap.findRoutes(from, targetId, rd)
        case false => BadEntityWrap.findRoutes(from, targetId, rd)
        case true => Obs just Vector(Vector.empty)
      }

      def withExtraPart = for {
        tag <- Obs from rd.pr.routingInfo
        partialRoutes <- getRoutes(tag.route.head.nodeId)
        completeRoutes = partialRoutes.map(_ ++ tag.route)
      } yield Obs just completeRoutes

      val cheapestRoutesObs =
        if (peers.isEmpty) Obs error new LightningException(me getString err_ln_no_chan)
        else if (rd.pr.routingInfo.isEmpty) getRoutes(targetId = rd.pr.nodeId)
        else Obs.zip(withExtraPart).map(_.flatten.toVector)

      // This may filter out too long or too expensive routes
      for (routes <- cheapestRoutesObs) yield useFirstRoute(routes, rd)
    }

    def sendEither(foeRD: FullOrEmptyRD, noRoutes: RoutingData => Unit): Unit = foeRD match {
      // Find a local channel which is online, can send an amount and belongs to a correct peer
      // or do the work required to reverse the payment in case if no channel is found

      case Right(rd) =>
        // Empty used route means we're sending to our peer and should use it's nodeId as a target
        val targetPeerId = if (rd.usedRoute.nonEmpty) rd.usedRoute.head.nodeId else rd.pr.nodeId
        val channelOpt = canSendNow(rd.firstMsat).find(_.data.announce.nodeId == targetPeerId)
        channelOpt.map(_ process rd) getOrElse sendEither(useRoutesLeft(rd), noRoutes)

      case Left(emptyRD) =>
        // All routes have been filtered out
        // or there were no routes at all
        noRoutes(emptyRD)
    }
  }

  abstract class WalletKit extends AbstractKit {
    type ScriptSeq = Seq[org.bitcoinj.script.Script]
    def currentAddress = wallet currentAddress KeyPurpose.RECEIVE_FUNDS
    def conf0Balance = wallet getBalance BalanceType.ESTIMATED_SPENDABLE // Returns all utxos
    def conf1Balance = wallet getBalance BalanceType.AVAILABLE_SPENDABLE // Uses coin selector
    def blockingSend(txj: Transaction) = peerGroup.broadcastTransaction(txj, 1).broadcast.get.toString
    def shutDown = none

    def closingPubKeyScripts(cd: ClosingData) = {
      val scripts = cd.commitTxs.flatMap(_.txOut).map(_.publicKeyScript)
      val jScripts = for (scr <- scripts) yield bitcoinLibScript2bitcoinjScript(scr)
      jScripts.asJava
    }

    def sign(unsigned: SendRequest) = {
      // Create a tx ready for broadcast
      wallet finalizeReadyTx unsigned
      unsigned.tx.verify
      unsigned
    }

    def useCheckPoints(time: Long) = {
      val pts = getAssets open "checkpoints.txt"
      CheckpointManager.checkpoint(params, pts, store, time)
    }

    def setupAndStartDownload = {
      wallet.setAcceptRiskyTransactions(true)
      wallet.addTransactionConfidenceEventListener(ChannelManager.chainEventsListener)
      wallet.addCoinsSentEventListener(ChannelManager.chainEventsListener)
      wallet.autosaveToFile(walletFile, 400, MILLISECONDS, null)
      wallet.setCoinSelector(new MinDepthReachedCoinSelector)

      try {
        Notificator.removeResyncNotification
        val shouldReschedule = ChannelManager.notClosingOrRefunding.exists(hasReceivedPayments)
        val fastPeer = InetAddress getByName Uri.parse(OlympusWrap.clouds.head.connector.url).getHost
        if (shouldReschedule) Notificator.scheduleResyncNotificationOnceAgain
        peerGroup addAddress new PeerAddress(app.params, fastPeer, 8333)
      } catch none

      peerGroup addPeerDiscovery new DnsDiscovery(params)
      peerGroup.setMinRequiredProtocolVersion(70015)
      peerGroup.setDownloadTxDependencies(0)
      peerGroup.setPingIntervalMsec(10000)
      peerGroup.setMaxConnections(5)
      peerGroup.addWallet(wallet)

      ConnectionManager.listeners += ChannelManager.socketEventsListener
      startBlocksDownload(ChannelManager.chainEventsListener)
      // Try to clear act leftovers if no channels left
      OlympusWrap tellClouds OlympusWrap.CMDStart
      PaymentInfoWrap.markFailedAndFrozen
      ChannelManager.initConnect
      RatesSaver.initialize
    }
  }
}

object Vibrator {
  def vibrate = if (null != vib && vib.hasVibrator) vib.vibrate(Array(0L, 85, 200), -1)
  lazy val vib = app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
}