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
import com.lightning.walletapp.ln.crypto.Sphinx.PublicKeyVec
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import collection.JavaConverters.seqAsJavaListConverter
import com.lightning.walletapp.lnutils.olympus.CloudAct
import java.util.concurrent.TimeUnit.MILLISECONDS
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.Wallet.BalanceType
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.wallet.SendRequest
import java.net.InetSocketAddress
import android.app.Application
import android.widget.Toast
import language.postfixOps
import scala.util.Try
import java.io.File

import com.google.common.util.concurrent.Service.State.{RUNNING, STARTING}
import org.bitcoinj.uri.{BitcoinURI, BitcoinURIParseException}
import android.content.{ClipData, ClipboardManager, Context}
import fr.acinq.bitcoin.{BinaryData, Crypto}
import rx.lang.scala.{Observable => Obs}


class WalletApp extends Application { me =>
  lazy val params = org.bitcoinj.params.TestNet3Params.get
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
  def notMixedCase(s: String) = s.toLowerCase == s || s.toUpperCase == s
  def getTo(raw: String) = Address.fromString(params, raw)

  Utils.appReference = me
  override def onCreate = wrap(super.onCreate) {
    // These cannot be lazy vals because values may change
    Utils.denom = Utils denoms prefs.getInt(AbstractKit.DENOM_TYPE, 0)
    Utils.fiatName = prefs.getString(AbstractKit.FIAT_TYPE, Utils.strDollar)
  }

  def setBuffer(text: String, andNotify: Boolean = true) = {
    // Set clipboard contents to given text and notify user via toast
    clipboardManager setPrimaryClip ClipData.newPlainText("wallet", text)
    if (andNotify) me toast getString(copied_to_clipboard).format(text)
  }

  object TransData {
    var value: Any = _
    val prefixes = PaymentRequest.prefixes.values mkString "|"
    val lnLink = s"(?im).*?([$prefixes]{4,6}[0-9]{1,}\\w+){1}".r.unanchored
    val nodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:\\.\\-_]+):([0-9]+)".r

    def recordValue(rawText: String) = value = rawText match {
      case raw if raw startsWith "bitcoin" => new BitcoinURI(params, raw)
      case lnLink(body) if notMixedCase(body) => PaymentRequest read body.toLowerCase
      case nodeLink(key, hostName, port) => mkNA(PublicKey(key), hostName, port.toInt)
      case _ => getTo(rawText)
    }

    def mkNA(nodeId: PublicKey, hostName: String, port: Int) = {
      // Make a fake node announcement with a dummy signature so it can be serialized
      val sig = Crypto encodeSignature Crypto.sign(random getBytes 32, LNParams.nodePrivateKey)
      NodeAnnouncement(sig, "", 0L, nodeId, (Byte.MinValue, Byte.MinValue, Byte.MinValue),
        hostName, new InetSocketAddress(hostName, port) :: Nil)
    }

    def onFail(err: Int => Unit): PartialFunction[Throwable, Unit] = {
      case _: org.bitcoinj.core.AddressFormatException => err(err_qr_parse)
      case _: BitcoinURIParseException => err(err_uri)
      case _: Throwable => err(err_general)
    }
  }

  object ChannelManager {
    val operationalListeners = Set(broadcaster, bag, GossipCatcher)
    // All stored channels which would receive CMDSpent, CMDBestHeight and nothing else
    var all: Vector[Channel] = for (data <- ChannelWrap.get) yield createChannel(operationalListeners, data)
    def fromNode(of: Vector[Channel], ann: NodeAnnouncement) = for (c <- of if c.data.announce == ann) yield c
    def canSendNow(amount: Long) = for (c <- all if isOperationalOpen(c) && estimateCanSend(c) >= amount) yield c
    def notClosingOrRefunding = for (c <- all if c.state != Channel.CLOSING && c.state != Channel.REFUNDING) yield c
    def notClosing = for (c <- all if c.state != Channel.CLOSING) yield c

    def frozenInFlightHashes = all.diff(notClosingOrRefunding).flatMap(inFlightOutgoingHtlcs).map(_.add.paymentHash)
    def activeInFlightHashes = notClosingOrRefunding.flatMap(inFlightOutgoingHtlcs).map(_.add.paymentHash)
    def initConnect = for (chan <- notClosing) ConnectionManager connectTo chan.data.announce

    val socketEventsListener = new ConnectionListener {
      override def onMessage(ann: NodeAnnouncement, msg: LightningMessage) = msg match {
        // Chan level Error will fall under ChannelMessage case but node level Error should be sent to all chans
        case err: Error if err.channelId == BinaryData("00" * 32) => fromNode(notClosing, ann).foreach(_ process err)
        case cm: ChannelMessage => notClosing.find(chan => chan(_.channelId) contains cm.channelId).foreach(_ process cm)
        case cu: ChannelUpdate => fromNode(notClosing, ann).foreach(_ process cu)
        case _ =>
      }

      override def onDisconnect(ann: NodeAnnouncement) = maybeReconnect(fromNode(notClosing, ann), ann)
      override def onOperational(ann: NodeAnnouncement, their: Init) = fromNode(notClosing, ann).foreach(_ process CMDOnline)
      override def onTerminalError(ann: NodeAnnouncement) = fromNode(notClosing, ann).foreach(_ process CMDShutdown)
      override def onIncompatible(ann: NodeAnnouncement) = onTerminalError(ann)

      def maybeReconnect(cs: Vector[Channel], ann: NodeAnnouncement) = if (cs.nonEmpty) {
        // Immediately inform affected channels and try to reconnect back again in 5 seconds
        Obs.just(ann).delay(5.seconds).subscribe(ConnectionManager.connectTo, none)
        cs.foreach(_ process CMDOffline)
      }
    }

    val chainEventsListener = new TxTracker with BlocksListener {
      override def txConfirmed(txj: Transaction) = for (c <- notClosing) c process CMDConfirmed(txj)
      def tellHeight(left: Int) = if (left < 1) for (c <- all) c process CMDBestHeight(broadcaster.currentHeight)
      override def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = tellHeight(left)
      override def onChainDownloadStarted(peer: Peer, left: Int) = tellHeight(left)

      override def coinsSent(txj: Transaction) = {
        // We always attempt to extract a payment preimage
        // just assuming any incoming tx may contain it

        val spent = CMDSpent(txj)
        bag.extractPreimage(spent.tx)
        for (c <- all) c process spent
      }
    }

    def createChannel(initialListeners: Set[ChannelListener], bootstrap: ChannelData) = new Channel {
      def STORE(hasCommitmentsData: HasCommitments) = runAnd(hasCommitmentsData)(ChannelWrap put hasCommitmentsData)
      def SEND(msg: LightningMessage) = ConnectionManager.connections.get(data.announce).foreach(_.handler process msg)

      def CLOSEANDWATCH(cd: ClosingData) = {
        val commits = cd.localCommit.map(_.commitTx) ++ cd.remoteCommit.map(_.commitTx) ++ cd.nextRemoteCommit.map(_.commitTx)
        // Collect all the commit txs publicKeyScripts and watch these scripts locally for future possible payment preimages
        repeat(OlympusWrap getChildTxs commits, pickInc, 7 to 8).foreach(_ foreach bag.extractPreimage, Tools.errlog)
        kit.watchScripts(commits.flatMap(_.txOut).map(_.publicKeyScript) map bitcoinLibScript2bitcoinjScript)
        BECOME(STORE(cd), CLOSING)

        val tier12txs = for (state <- cd.tier12States) yield state.txn
        if (tier12txs.isEmpty) Tools log "Closing channel does not have tier 1-2 transactions"
        else OlympusWrap tellClouds CloudAct(tier12txs.toJson.toString.hex, Nil, "txs/schedule")
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
      val peers = canSendNow(rd.firstMsat).map(_.data.announce.nodeId)
      def getRoutes(targetId: PublicKey) = peers contains targetId match {
        case false if rd.useCache => RouteWrap.findRoutes(peers, targetId, rd)
        case false => BadEntityWrap.findRoutes(peers, targetId, rd)
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

      for {
        routes <- cheapestRoutesObs
        shortest = routes.sortBy(_.size)
      } yield useFirstRoute(shortest, rd)
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
    def blockingSend(tx: Transaction) = peerGroup.broadcastTransaction(tx, 1).broadcast.get.toString
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

    def setupAndStartDownload = {
      wallet.setAcceptRiskyTransactions(true)
      wallet.addTransactionConfidenceEventListener(ChannelManager.chainEventsListener)
      wallet.addCoinsSentEventListener(ChannelManager.chainEventsListener)
      wallet.autosaveToFile(walletFile, 400, MILLISECONDS, null)
      peerGroup addPeerDiscovery new DnsDiscovery(params)
      peerGroup.setMinRequiredProtocolVersion(70015)
      peerGroup.setDownloadTxDependencies(0)
      peerGroup.setPingIntervalMsec(10000)
      peerGroup.setMaxConnections(5)
      peerGroup.addWallet(wallet)

      Notificator.removeResyncNotification
      if (ChannelManager.notClosingOrRefunding.nonEmpty)
        Notificator.scheduleResyncNotificationOnceAgain

      ConnectionManager.listeners += ChannelManager.socketEventsListener
      // Passing bitcoinj listener ensures onChainDownloadStarted is called
      startBlocksDownload(ChannelManager.chainEventsListener)
      // Try to clear act leftovers if no channels left
      OlympusWrap tellClouds OlympusWrap.CMDStart
      PaymentInfoWrap.markFailedAndFrozen
      ChannelManager.initConnect
      RatesSaver.initialize
    }
  }
}

object Vibr {
  def vibrate = if (null != vib && vib.hasVibrator) vib.vibrate(Array(0L, 85, 200), -1)
  lazy val vib = app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
}