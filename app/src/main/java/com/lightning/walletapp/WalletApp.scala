package com.lightning.walletapp

import R.string._
import spray.json._
import org.bitcoinj.core._
import scala.concurrent.duration._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.wire._
import scala.collection.JavaConverters._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.google.common.util.concurrent.Service.State._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import rx.lang.scala.{Observable => Obs}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import java.net.{InetAddress, InetSocketAddress}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}
import fr.acinq.bitcoin.{Crypto, MilliSatoshi, Satoshi}
import android.content.{ClipData, ClipboardManager, Context}

import com.lightning.walletapp.ln.wire.LightningMessageCodecs.RGB
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import com.lightning.walletapp.lnutils.olympus.CloudAct
import concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.TimeUnit.MILLISECONDS
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.Wallet.BalanceType
import fr.acinq.bitcoin.Hash.Zeroes
import org.bitcoinj.uri.BitcoinURI
import scala.concurrent.Future
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
  def notMixedCase(s: String) = s.toLowerCase == s || s.toUpperCase == s

  Utils.appReference = me
  override def onCreate = wrap(super.onCreate) {
    // These cannot be lazy vals because values may change
    Utils.denom = Utils denoms prefs.getInt(AbstractKit.DENOM_TYPE, 0)
    Utils.fiatName = prefs.getString(AbstractKit.FIAT_TYPE, Utils.strDollar)
  }

  def setBuffer(text: String) = {
    val content = ClipData.newPlainText("wallet", text)
    me toast getString(copied_to_clipboard).format(text)
    clipboardManager setPrimaryClip content
  }

  def mkNodeAnnouncement(nodeId: PublicKey, host: String, port: Int) = {
    val nodeAddress: InetSocketAddress = new InetSocketAddress(host, port)
    val nodeColors: RGB = Tuple3(Byte.MinValue, Byte.MinValue, Byte.MinValue)
    val sig = Crypto encodeSignature Crypto.sign(random getBytes 32, LNParams.nodePrivateKey)
    NodeAnnouncement(sig, "", 0L, nodeId, nodeColors, host, NodeAddress(nodeAddress) :: Nil)
  }

  object TransData { self =>
    var value: Any = new String
    private[this] val prefixes = PaymentRequest.prefixes.values mkString "|"
    private[this] val lnUrl = s"(?im).*?(lnurl)([0-9]{1,}[a-z0-9]+){1}".r.unanchored
    private[this] val lnPayReq = s"(?im).*?($prefixes)([0-9]{1,}[a-z0-9]+){1}".r.unanchored
    private[this] val funder = "(lnbcfunder|lntbfunder|lnbcrtfunder):([a-z0-9]+)".r
    val nodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:\\.\\-_]+):([0-9]+)".r

    case object DoNotEraseValue
    type Checker = PartialFunction[Any, Any]
    def checkAndMaybeErase(check: Checker) = check(value) match {
      // Sometimes we need to forward a value between activity switches
      case DoNotEraseValue => Tools log "app.TransData.value retained"
      case _ => value = null
    }

    def recordValue(rawText: String) = value = rawText take 2880 match {
      case _ if rawText startsWith "bitcoin" => new BitcoinURI(params, rawText)
      case _ if rawText startsWith "BITCOIN" => new BitcoinURI(params, rawText.toLowerCase)
      case nodeLink(key, host, port) => mkNodeAnnouncement(PublicKey(key), host, port.toInt)
      case lnPayReq(prefix, req) => PaymentRequest.read(s"$prefix$req".toLowerCase)
      case lnUrl(prefix, data) => LNUrl(s"$prefix$data".toLowerCase)
      case funder(_, paramsHex) => to[Started](paramsHex.hex2asci)
      case _ => toAddress(rawText)
    }
  }

  object ChannelManager {
    var currentBlocksLeft = Int.MaxValue
    var initialChainHeight = broadcaster.currentHeight // last seen height
    val operationalListeners = Set(broadcaster, bag, GossipCatcher)
    val CMDLocalShutdown = CMDShutdown(None)

    // All stored channels which would receive CMDSpent, CMDBestHeight and nothing else
    var all: Vector[Channel] = for (data <- ChannelWrap.get) yield createChannel(operationalListeners, data)
    def fromNode(of: Vector[Channel], nodeId: PublicKey) = for (c <- of if c.data.announce.nodeId == nodeId) yield c
    def notClosing = for (c <- all if c.state != CLOSING) yield c

    def notClosingOrRefunding: Vector[Channel] = for {
      chan <- all if isOpening(chan) || isOperational(chan)
    } yield chan

    def chanReports = for {
      chan <- all if isOperational(chan)
      commitments <- chan apply identity
    } yield ChanReport(chan, commitments)

    def delayedPublishes = {
      // Select all ShowDelayed which can't be published yet because cltv/csv delay is not cleared
      val statuses = all.map(_.data).collect { case cd: ClosingData => cd.bestClosing.getState }.flatten
      statuses.collect { case sd: ShowDelayed if !sd.isPublishable && sd.delay > Long.MinValue => sd }
    }

    def activeInFlightHashes = notClosingOrRefunding.flatMap(inFlightHtlcs).map(_.add.paymentHash)
    def frozenInFlightHashes = all.map(_.data).collect { case cd: ClosingData => cd.frozenPublishedHashes }.flatten
    def initConnect = for (c <- notClosing) ConnectionManager.connectTo(c.data.announce, notify = false)

    val socketEventsListener = new ConnectionListener {
      override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
        case update: ChannelUpdate => fromNode(notClosing, nodeId).foreach(_ process update)
        case errAll: Error if errAll.channelId == Zeroes => fromNode(notClosing, nodeId).foreach(_ process errAll)
        case m: ChannelMessage => notClosing.find(chan => chan(_.channelId) contains m.channelId).foreach(_ process m)
        case _ =>
      }

      override def onTerminalError(nodeId: PublicKey) = fromNode(notClosing, nodeId).foreach(_ process CMDLocalShutdown)
      override def onOperational(nodeId: PublicKey) = fromNode(notClosing, nodeId).foreach(_ process CMDOnline)
      override def onIncompatible(nodeId: PublicKey) = onTerminalError(nodeId)

      override def onDisconnect(nodeId: PublicKey) = for {
        affectedChans <- Obs just fromNode(notClosing, nodeId) if affectedChans.nonEmpty
        _ = affectedChans.foreach(affectedChan => affectedChan process CMDOffline)
        announce <- Obs just affectedChans.head.data.announce delay 5.seconds
      } ConnectionManager.connectTo(announce, notify = false)
    }

    val chainEventsListener = new TxTracker with BlocksListener {
      override def onChainDownloadStarted(peer: Peer, left: Int) = onChainDownload(left)
      override def txConfirmed(txj: Transaction) = for (c <- notClosing) c process CMDConfirmed(txj)
      def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = onChainDownload(left)
      def onCoinsReceived(w: Wallet, txj: Transaction, a: Coin, b: Coin) = onChainTx(txj)
      def onCoinsSent(w: Wallet, txj: Transaction, a: Coin, b: Coin) = onChainTx(txj)

      def onChainDownload(blocksLeft: Int) = {
        // Should wait until all blocks are downloaded
        // otherwise peers may lie about current height
        currentBlocksLeft = blocksLeft

        if (currentBlocksLeft < 1) {
          // Send out pending payments and report current chain heights to channels
          val cmd = CMDBestHeight(broadcaster.currentHeight, initialChainHeight)
          // Set initial height to current to not show repeated warnings
          initialChainHeight = cmd.heightNow
          PaymentInfoWrap.resolvePending
          for (c <- all) c process cmd
        }
      }

      def onChainTx(txj: Transaction) = {
        val cmdOnChainSpent = CMDSpent(txj)
        for (c <- all) c process cmdOnChainSpent
        bag.extractPreimage(cmdOnChainSpent.tx)
      }
    }

    def createChannel(initialListeners: Set[ChannelListener], bootstrap: ChannelData) = new Channel { self =>
      def SEND(m: LightningMessage) = for (w <- ConnectionManager.connections get data.announce.nodeId) w.handler process m
      def STORE(updatedChannelData: HasCommitments) = runAnd(updatedChannelData)(ChannelWrap put updatedChannelData)

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
        txsObs.foreach(txs => txs map CMDSpent foreach process, none)
      }

      def ASKREFUNDPEER(some: HasCommitments, point: Point) = {
        val ref = RefundingData(some.announce, Some(point), some.commitments)
        val fundingScript = some.commitments.commitInput.txOut.publicKeyScript
        app.kit.wallet.addWatchedScripts(Collections singletonList fundingScript)
        BECOME(STORE(ref), REFUNDING) SEND makeReestablish(some, Long.MaxValue)
      }

      // First add listeners, then call
      // doProcess on current thread
      listeners = initialListeners
      doProcess(bootstrap)
    }

    def checkIfSendable(rd: RoutingData) = {
      val bestRepOpt = chanReports.sortBy(rep => -rep.estimateFinalCanSend).headOption
      val isPaid = bag.getPaymentInfo(rd.pr.paymentHash).filter(_.actualStatus == SUCCESS)
      if (frozenInFlightHashes contains rd.pr.paymentHash) Left(me getString err_ln_frozen)
      else if (isPaid.isSuccess) Left(me getString err_ln_fulfilled)
      else bestRepOpt match {

        // We should explain to user what exactly is going on here
        case Some(rep) if rep.estimateFinalCanSend < rd.firstMsat =>
          val sendingNow = coloredOut apply MilliSatoshi(rd.firstMsat)
          val finalCanSend = coloredIn apply MilliSatoshi(rep.estimateFinalCanSend)
          val capacity = coloredIn apply MilliSatoshi(Commitments.latestRemoteCommit(rep.cs).spec.toRemoteMsat)
          val hardReserve = coloredOut apply Satoshi(rep.cs.reducedRemoteState.myFeeSat + rep.cs.remoteParams.channelReserveSatoshis)
          Left(getString(err_ln_second_reserve).format(rep.chan.data.announce.alias, hardReserve, coloredOut(rep.softReserve),
            capacity, finalCanSend, sendingNow).html)

        case None => Left(me getString ln_no_open_chans)
        case Some(reportWhichIsFine) => Right(rd)
      }
    }

    def fetchRoutes(rd: RoutingData) = {
      // First we collect chans which in principle can handle a given payment sum right now
      // after we get the results we first prioritize cheapest routes and then routes which belong to currently less busy chans
      val from = chanReports collect { case rep if rep.estimateFinalCanSend >= rd.firstMsat => rep.chan.data.announce.nodeId }
      val from1 = if (rd.throughPeers.nonEmpty) from.filter(rd.throughPeers.contains) else from

      def withHints = for {
        tag <- Obs from rd.pr.routingInfo
        partialRoutes <- getRoutes(tag.route.head.nodeId)
        completeRoutes = partialRoutes.map(_ ++ tag.route)
      } yield Obs just completeRoutes

      def getRoutes(targetId: PublicKey) = from1 contains targetId match {
        case false if rd.useCache => RouteWrap.findRoutes(from1, targetId, rd)
        case false => BadEntityWrap.findRoutes(from1, targetId, rd)
        case true => Obs just Vector(Vector.empty)
      }

      val paymentRoutesObs =
        if (from1.isEmpty) Obs error new LightningException(me getString ln_no_open_chans)
        else Obs.zip(getRoutes(rd.pr.nodeId) +: withHints).map(_.flatten.toVector)

      for {
        routes <- paymentRoutesObs
        cheapest = routes.sortBy(route => route.map(hop => hop.feeEstimate).sum)
        busyMap = all.map(chan => chan.data.announce.nodeId -> inFlightHtlcs(chan).size).toMap
        unloadest = cheapest.sortBy(route => if (route.nonEmpty) busyMap(route.head.nodeId) else 0)
      } yield useFirstRoute(unloadest, rd)
    }

    def sendEither(foeRD: FullOrEmptyRD, noRoutes: RoutingData => Unit): Unit = foeRD match {
      // Find a local channel which is online, can send an amount and belongs to a correct peer
      case Right(rd) if activeInFlightHashes contains rd.pr.paymentHash =>
      case Left(emptyRD) => noRoutes(emptyRD)

      case Right(rd) =>
        // Empty used route means we're sending to our peer and its nodeId is our target
        val targetId = if (rd.usedRoute.nonEmpty) rd.usedRoute.head.nodeId else rd.pr.nodeId
        val chans = chanReports collect { case rep if rep.estimateFinalCanSend >= rd.firstMsat => rep.chan }
        val res = chans collectFirst { case chan if chan.data.announce.nodeId == targetId => chan process rd }
        res getOrElse sendEither(useRoutesLeft(rd), noRoutes)
    }
  }

  abstract class WalletKit extends AbstractKit {
    def currentAddress = wallet currentAddress KeyPurpose.RECEIVE_FUNDS
    def conf0Balance = wallet getBalance BalanceType.ESTIMATED_SPENDABLE // Returns all utxos
    def conf1Balance = wallet getBalance BalanceType.AVAILABLE_SPENDABLE // Uses coin selector
    def blockSend(txj: Transaction) = peerGroup.broadcastTransaction(txj, 1).broadcast.get
    def shutDown = none

    def closingPubKeyScripts(cd: ClosingData) =
      cd.commitTxs.flatMap(_.txOut).map(_.publicKeyScript)
        .map(bitcoinLibScript2bitcoinjScript).asJava

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
      wallet.addTransactionConfidenceEventListener(ChannelManager.chainEventsListener)
      wallet.addCoinsReceivedEventListener(ChannelManager.chainEventsListener)
      wallet.addCoinsSentEventListener(ChannelManager.chainEventsListener)
      wallet.autosaveToFile(walletFile, 400, MILLISECONDS, null)
      wallet.setCoinSelector(new MinDepthReachedCoinSelector)

      Future {
        val host = Uri.parse(OlympusWrap.clouds.head.connector.url).getHost
        val peer = new PeerAddress(app.params, InetAddress getByName host, 8333)
        peerGroup addAddress peer
      }

      peerGroup addPeerDiscovery new DnsDiscovery(params)
      peerGroup.setMinRequiredProtocolVersion(70015)
      peerGroup.setDownloadTxDependencies(0)
      peerGroup.setPingIntervalMsec(10000)
      peerGroup.setMaxConnections(5)
      peerGroup.addWallet(wallet)

      for {
        _ <- obsOnIO delay 20.seconds
        chan <- ChannelManager.notClosing if chan.state == OFFLINE
        // Enough time passes and we have offline channels, possibly remote peer IP has changed
        res <- retry(OlympusWrap findNodes chan.data.announce.nodeId.toString, pickInc, 1 to 2)
        announce \ _ <- res take 1
      } chan process announce

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