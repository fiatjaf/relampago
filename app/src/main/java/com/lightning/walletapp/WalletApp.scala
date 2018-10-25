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
import com.lightning.walletapp.ln.wire.FundMsg._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.google.common.util.concurrent.Service.State._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import org.bitcoinj.core.TransactionConfidence.ConfidenceType._

import rx.lang.scala.{Observable => Obs}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import java.net.{InetAddress, InetSocketAddress}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}
import android.content.{ClipData, ClipboardManager, Context}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Satoshi}

import com.lightning.walletapp.ln.wire.LightningMessageCodecs.revocationInfoCodec
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.RGB
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import com.lightning.walletapp.lnutils.olympus.TxUploadAct
import concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.TimeUnit.MILLISECONDS
import com.lightning.walletapp.helper.RichCursor
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.Wallet.BalanceType
import fr.acinq.bitcoin.Hash.Zeroes
import org.bitcoinj.uri.BitcoinURI
import scala.concurrent.Future
import android.app.Application
import java.util.Collections
import scodec.bits.BitVector
import android.widget.Toast
import scodec.DecodeResult
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
      case _ => toAddress(rawText)
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
      wallet.setCoinSelector(new MinDepthReachedCoinSelector)
      wallet.autosaveToFile(walletFile, 1000, MILLISECONDS, null)

      Future {
        val host = Uri.parse(OlympusWrap.clouds.head.connector.url).getHost
        val peer = new PeerAddress(app.params, InetAddress getByName host, 8333)
        peerGroup addAddress peer
      }

      wallet.addCoinsSentEventListener(ChannelManager.chainEventsListener)
      wallet.addCoinsReceivedEventListener(ChannelManager.chainEventsListener)
      wallet.addTransactionConfidenceEventListener(ChannelManager.chainEventsListener)
      peerGroup.addDisconnectedEventListener(ChannelManager.chainEventsListener)
      peerGroup addPeerDiscovery new DnsDiscovery(params)
      peerGroup.setMinRequiredProtocolVersion(70015)
      peerGroup.setDownloadTxDependencies(0)
      peerGroup.setPingIntervalMsec(10000)
      peerGroup.setMaxConnections(5)
      peerGroup.addWallet(wallet)

      for {
        _ <- obsOnIO delay 20.seconds
        chan <- ChannelManager.notClosing if chan.state == SLEEPING
        // Can call findNodes without `retry` wrapper because it gives `Obs.empty` on error
        Vector(ann1 \ _, _*) <- OlympusWrap findNodes chan.data.announce.nodeId.toString
      } chan process ann1

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

object ChannelManager extends Broadcaster {
  val CMDLocalShutdown: Command = CMDShutdown(None)
  val operationalListeners: Set[ChannelListener] = Set(ChannelManager, bag, GossipCatcher)
  private[this] var initialChainHeight: Long = app.kit.wallet.getLastBlockSeenHeight
  var currentBlocksLeft: Int = Int.MaxValue

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

  val chainEventsListener = new TxTracker with BlocksListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(connectedPeer: Peer, numPeers: Int) = if (numPeers < 1) onBlock = oneTimeRun
    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = onBlock(left)
    override def onChainDownloadStarted(peer: Peer, left: Int) = onBlock(left)

    override def txConfirmed(txj: Transaction) = for (c <- notClosing) c process CMDConfirmed(txj)
    def onCoinsReceived(w: Wallet, txj: Transaction, a: Coin, b: Coin) = onChainTx(txj)
    def onCoinsSent(w: Wallet, txj: Transaction, a: Coin, b: Coin) = onChainTx(txj)

    var onBlock: Int => Unit = oneTimeRun
    lazy val oneTimeRun: Int => Unit = left => {
      // Let currentBlocksLeft become less than MaxValue
      runAnd(onBlock = standardRun)(standardRun apply left)
      // Can send chain height because currentBlocksLeft is updated
      val cmd = CMDBestHeight(currentHeight, initialChainHeight)
      for (channel <- all) channel process cmd
      PaymentInfoWrap.resolvePending
    }

    lazy val standardRun: Int => Unit = left => {
      // LN payment before BTC peers on app start: tried in `oneTimeRun`
      // LN payment before BTC peers on app restart: tried in `oneTimeRun`
      // LN payment after BTC peers on app start: tried immediately since `currentBlocksLeft` < `Int.MacValue`
      // LN payment after BTC peers on app restart: tried immediately since `currentBlocksLeft` < `Int.MacValue`
      // LN payment after BTC peers disconnected: tried in `oneTimeRun` because `onPeerDisconnected` resets `onBlock`
      if (left < 1) initialChainHeight = currentHeight
      currentBlocksLeft = left
    }

    def onChainTx(txj: Transaction) = {
      val cmdOnChainSpent = CMDSpent(txj)
      for (c <- all) c process cmdOnChainSpent
      bag.extractPreimage(cmdOnChainSpent.tx)
    }
  }

  // BROADCASTER IMPLEMENTATION

  def perKwSixSat = RatesSaver.rates.feeSix.value / 4
  def perKwThreeSat = RatesSaver.rates.feeThree.value / 4
  // We may still be syncing but anyway a final chain height is known here
  def currentHeight = app.kit.wallet.getLastBlockSeenHeight + currentBlocksLeft

  def getTx(txid: BinaryData) = {
    val wrapped = Sha256Hash wrap txid.toArray
    Option(app.kit.wallet getTransaction wrapped)
  }

  def getStatus(txid: BinaryData) = getTx(txid) map { tx =>
    val isTxDead = tx.getConfidence.getConfidenceType == DEAD
    tx.getConfidence.getDepthInBlocks -> isTxDead
  } getOrElse 0 -> false

  override def onProcessSuccess = {
    case (_, close: ClosingData, _: Command) =>
      // Repeatedly spend everything we can in this state in case it was unsuccessful before
      val tier12Publishable = for (state <- close.tier12States if state.isPublishable) yield state.txn
      val toSend = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ tier12Publishable
      for (tx <- toSend) try app.kit blockSend tx catch none

    case (chan, wbr: WaitBroadcastRemoteData, _: ChannelReestablish) if wbr.isOutdated && wbr.fail.isEmpty =>
      // External funder may never publish our funding tx so we should mark it as failure once enough time passes
      chan process Fail(FAIL_PUBLISH_ERROR, "Funding has expired")

    case (chan, wait: WaitFundingDoneData, _: ChannelReestablish) if wait.our.isEmpty =>
      // CMDConfirmed may be sent to an offline channel and there will be no reaction
      // so always double check a funding state here as a failsafe measure

      for {
        txj <- getTx(wait.fundingTx.txid)
        depth \ isDead = getStatus(wait.fundingTx.txid)
        if depth >= LNParams.minDepth && !isDead
      } chan process CMDConfirmed(txj)
  }

  override def onBecome = {
    // Repeatedly resend a funding tx, update feerate on becoming open
    case (_, wait: WaitFundingDoneData, _, _) => app.kit blockSend wait.fundingTx
    case (chan, _: NormalData, SLEEPING, OPEN) => chan process CMDFeerate(perKwThreeSat)
  }

  // CHANNEL CREATION AND MANAGEMENT

  // All stored channels which would receive CMDSpent, CMDBestHeight and nothing else
  var all: Vector[Channel] = for (chanState <- ChannelWrap.get) yield createChannel(operationalListeners, chanState)
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

  def createChannel(initialListeners: Set[ChannelListener], bootstrap: ChannelData) = new Channel { self =>
    def SEND(m: LightningMessage) = for (w <- ConnectionManager.connections get data.announce.nodeId) w.handler process m
    def STORE(updatedChannelData: HasCommitments) = runAnd(updatedChannelData)(ChannelWrap put updatedChannelData)

    def REV(cs: Commitments, rev: RevokeAndAck) = for {
      // We use old commitments to save a punishment for
      // remote commit before it gets dropped forever

      tx <- cs.remoteCommit.txOpt
      myBalance = cs.localCommit.spec.toLocalMsat
      revocationInfo = Helpers.Closing.makeRevocationInfo(cs, tx, rev.perCommitmentSecret)
      serialized = LightningMessageCodecs.serialize(revocationInfoCodec encode revocationInfo)
    } db.change(RevokedInfoTable.newSql, tx.txid, cs.channelId, myBalance, serialized)

    def GETREV(tx: fr.acinq.bitcoin.Transaction) = {
      // Extract RevocationInfo for a given breach transaction
      val cursor = db.select(RevokedInfoTable.selectTxIdSql, tx.txid)
      val rc = RichCursor(cursor).headTry(_ string RevokedInfoTable.info)

      for {
        serialized <- rc.toOption
        bitVec = BitVector(BinaryData(serialized).data)
        DecodeResult(ri, _) <- revocationInfoCodec.decode(bitVec).toOption
      } yield Helpers.Closing.claimRevokedRemoteCommitTxOutputs(ri, tx)
    }

    def CLOSEANDWATCHREVHTLC(cd: ClosingData) = {
      // After publishing a revoked remote commit our peer may further publish Timeout and Success HTLC outputs
      // our job here is to watch every output of every revoked commit tx and re-spend it before their CSV delay runs out
      repeat(OlympusWrap getChildTxs cd.commitTxs.map(_.txid), pickInc, 7 to 8).foreach(_ map CMDSpent foreach process, none)
      app.kit.wallet.addWatchedScripts(app.kit closingPubKeyScripts cd)
      BECOME(STORE(cd), CLOSING)
    }

    def CLOSEANDWATCH(cd: ClosingData) = {
      val tier12txs = for (state <- cd.tier12States) yield state.txn
      if (tier12txs.nonEmpty) OlympusWrap tellClouds TxUploadAct(tier12txs.toJson.toString.hex, Nil, "txs/schedule")
      repeat(OlympusWrap getChildTxs cd.commitTxs.map(_.txid), pickInc, 7 to 8).foreach(_ foreach bag.extractPreimage, none)
      // Collect all the commit txs publicKeyScripts and watch these scripts locally for future possible payment preimages
      app.kit.wallet.addWatchedScripts(app.kit closingPubKeyScripts cd)
      BECOME(STORE(cd), CLOSING)
    }

    def ASKREFUNDTX(ref: RefundingData) = {
      // Failsafe check in case if we are still in REFUNDING state after app is restarted
      val txsObs = OlympusWrap getChildTxs Seq(ref.commitments.commitInput.outPoint.txid)
      txsObs.foreach(_ map CMDSpent foreach process, none)
    }

    def ASKREFUNDPEER(some: HasCommitments, point: Point) = {
      val ref = RefundingData(some.announce, Some(point), some.commitments)
      val fundingScript = some.commitments.commitInput.txOut.publicKeyScript
      app.kit.wallet.addWatchedScripts(Collections singletonList fundingScript)
      BECOME(STORE(ref), REFUNDING) SEND makeReestablish(some, 0L)
    }

    // First add listeners, then call
    // doProcess on current thread
    listeners = initialListeners
    doProcess(bootstrap)
  }

  // SENDING PAYMENTS

  def checkIfSendable(rd: RoutingData) = {
    val bestRepOpt = chanReports.sortBy(rep => -rep.estimateFinalCanSend).headOption
    val isPaid = bag.getPaymentInfo(rd.pr.paymentHash).filter(_.actualStatus == SUCCESS)
    if (frozenInFlightHashes contains rd.pr.paymentHash) Left(app getString err_ln_frozen)
    else if (isPaid.isSuccess) Left(app getString err_ln_fulfilled)
    else bestRepOpt match {

      // We should explain to user what exactly is going on here
      case Some(rep) if rep.estimateFinalCanSend < rd.firstMsat =>
        val sendingNow = coloredOut apply MilliSatoshi(rd.firstMsat)
        val finalCanSend = coloredIn apply MilliSatoshi(rep.estimateFinalCanSend)
        val capacity = coloredIn apply MilliSatoshi(Commitments.latestRemoteCommit(rep.cs).spec.toRemoteMsat)
        val hardReserve = coloredOut apply Satoshi(rep.cs.reducedRemoteState.myFeeSat + rep.cs.remoteParams.channelReserveSatoshis)
        Left(app.getString(err_ln_second_reserve).format(rep.chan.data.announce.alias, hardReserve, coloredOut(rep.softReserve),
          capacity, finalCanSend, sendingNow).html)

      case None => Left(app getString ln_no_open_chans)
      case _ => Right(rd)
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
      if (from1.isEmpty) Obs error new LightningException(app getString ln_no_open_chans)
      else Obs.zip(getRoutes(rd.pr.nodeId) +: withHints).map(_.flatten.toVector)

    for {
      routes <- paymentRoutesObs
      busyMap = all.map(chan => chan.data.announce.nodeId -> inFlightHtlcs(chan).size).toMap
      unloadest = routes.sortBy(route => if (route.nonEmpty) busyMap(route.head.nodeId) else 0)
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

object Vibrator {
  def vibrate = if (null != vib && vib.hasVibrator) vib.vibrate(Array(0L, 85, 200), -1)
  lazy val vib = app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
}