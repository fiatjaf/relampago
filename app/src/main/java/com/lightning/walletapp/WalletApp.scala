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
import com.google.common.util.concurrent.Service.State._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import org.bitcoinj.core.TransactionConfidence.ConfidenceType._

import rx.lang.scala.{Observable => Obs}
import fr.acinq.bitcoin.{BinaryData, Crypto}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}
import androidx.work.{ExistingWorkPolicy, WorkManager}
import android.content.{ClipData, ClipboardManager, Context}
import com.lightning.walletapp.helper.{AwaitService, RichCursor}
import android.app.{Application, NotificationChannel, NotificationManager}
import com.lightning.walletapp.lnutils.JsonHttpUtils.{obsOnIO, pickInc, repeat}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.revocationInfoCodec
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import com.lightning.walletapp.lnutils.olympus.TxUploadAct
import concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.TimeUnit.MILLISECONDS
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.Wallet.BalanceType
import java.util.Collections.singletonList
import fr.acinq.bitcoin.Hash.Zeroes
import org.bitcoinj.uri.BitcoinURI
import java.net.InetSocketAddress
import scala.concurrent.Future
import scodec.bits.BitVector
import android.widget.Toast
import scodec.DecodeResult
import android.os.Build
import scala.util.Try
import java.io.File


class WalletApp extends Application { me =>
  lazy val params = org.bitcoinj.params.MainNetParams.get
  lazy val prefs = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val walletFile = new File(getFilesDir, walletFileName)
  lazy val chainFile = new File(getFilesDir, chainFileName)
  var olympus: OlympusWrap = _
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
  def plurOrZero(opts: Array[String], num: Long) = if (num > 0) plur(opts, num) format num else opts(0)
  def getBufferTry = Try(clipboardManager.getPrimaryClip.getItemAt(0).getText.toString)
  def toAddress(rawText: String) = Address.fromString(app.params, rawText)
  def notMixedCase(s: String) = s.toLowerCase == s || s.toUpperCase == s

  def isAlive =
    if (null == kit || null == olympus || null == db || null == extendedNodeKey) false
    else kit.state match { case STARTING | RUNNING => true case _ => false }

  Utils.appReference = me
  override def onCreate = wrap(super.onCreate) {
    // These cannot be lazy vals because values may change
    Utils.fiatCode = prefs.getString(AbstractKit.FIAT_TYPE, "usd")
    Utils.denom = Utils denoms prefs.getInt(AbstractKit.DENOM_TYPE, 0)

    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N_MR1) {
      val importanceLevel = NotificationManager.IMPORTANCE_DEFAULT
      val srvChan = new NotificationChannel(AwaitService.CHANNEL_ID, "NC", importanceLevel)
      me getSystemService classOf[NotificationManager] createNotificationChannel srvChan
    }
  }

  def setBuffer(text: String) = {
    val content = ClipData.newPlainText("wallet", text)
    me toast getString(copied_to_clipboard).format(text)
    clipboardManager setPrimaryClip content
  }

  def mkNodeAnnouncement(id: PublicKey, isa: InetSocketAddress, alias: String) = {
    val dummySig = Crypto encodeSignature Crypto.sign(random getBytes 32, randomPrivKey)
    NodeAnnouncement(dummySig, "", 0L, id, (-128, -128, -128), alias, NodeAddress(isa) :: Nil)
  }

  object TransData { self =>
    var value: Any = new String
    private[this] val prefixes = PaymentRequest.prefixes.values mkString "|"
    private[this] val lnUrl = s"(?im).*?(lnurl)([0-9]{1,}[a-z0-9]+){1}".r.unanchored
    private[this] val lnPayReq = s"(?im).*?($prefixes)([0-9]{1,}[a-z0-9]+){1}".r.unanchored
    private[this] val shortNodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:\\.\\-_]+)".r
    val nodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:\\.\\-_]+):([0-9]+)".r

    case object DoNotEraseValue
    type Checker = PartialFunction[Any, Any]
    def checkAndMaybeErase(check: Checker) = check(value) match {
      // Sometimes we need to forward a value between activity switches
      case DoNotEraseValue => Tools log "app.TransData.value retained"
      case _ => value = null
    }

    def bitcoinUri(bitcoinUriLink: String) = {
      val uri = new BitcoinURI(params, bitcoinUriLink)
      require(null != uri.getAddress, "No address detected")
      uri
    }

    def recordValue(rawText: String) = value = rawText take 2880 match {
      case bitcoinUriLink if bitcoinUriLink startsWith "bitcoin" => bitcoinUri(bitcoinUriLink)
      case bitcoinUriLink if bitcoinUriLink startsWith "BITCOIN" => bitcoinUri(bitcoinUriLink.toLowerCase)
      case nodeLink(key, host, port) => mkNodeAnnouncement(PublicKey(key), new InetSocketAddress(host, port.toInt), host)
      case shortNodeLink(key, host) => mkNodeAnnouncement(PublicKey(key), new InetSocketAddress(host, 9735), host)
      case lnPayReq(prefix, data) => PaymentRequest.read(s"$prefix$data")
      case lnUrl(prefix, data) => LNUrl.fromBech32(s"$prefix$data")
      case _ => toAddress(rawText)
    }
  }

  abstract class WalletKit extends AbstractKit {
    def currentAddress = wallet currentAddress KeyPurpose.RECEIVE_FUNDS
    def conf0Balance = wallet getBalance BalanceType.ESTIMATED_SPENDABLE // Returns all utxos
    def blockSend(txj: Transaction) = peerGroup.broadcastTransaction(txj, 1).broadcast.get
    def shutDown = none

    def fundingPubScript(some: HasCommitments) = singletonList(some.commitments.commitInput.txOut.publicKeyScript: org.bitcoinj.script.Script)
    def closingPubKeyScripts(cd: ClosingData) = cd.commitTxs.flatMap(_.txOut).map(_.publicKeyScript: org.bitcoinj.script.Script).asJava
    def useCheckPoints(time: Long) = CheckpointManager.checkpoint(params, getAssets open "checkpoints.txt", store, time)

    def sign(unsigned: SendRequest) = {
      // Create a tx ready for broadcast
      wallet finalizeReadyTx unsigned
      unsigned.tx.verify
      unsigned
    }

    def setupAndStartDownload = {
      wallet.allowSpendingUnconfirmedTransactions
      wallet.autosaveToFile(walletFile, 1000, MILLISECONDS, null)
      wallet.addCoinsSentEventListener(ChannelManager.chainEventsListener)
      wallet.addCoinsReceivedEventListener(ChannelManager.chainEventsListener)
      wallet.addTransactionConfidenceEventListener(ChannelManager.chainEventsListener)
      peerGroup.addDisconnectedEventListener(ChannelManager.chainEventsListener)

      // Speed up initial connection by using a random top node
      Future(peerGroup addAddress TopNodes.randomPeerAddress)
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
        Vector(ann1 \ _, _*) <- app.olympus findNodes chan.data.announce.nodeId.toString
      } chan process ann1

      ConnectionManager.listeners += ChannelManager.socketEventsListener
      startBlocksDownload(ChannelManager.chainEventsListener)
      // Try to clear act leftovers if no channels are left
      app.olympus tellClouds OlympusWrap.CMDStart
      PaymentInfoWrap.markFailedAndFrozen
      ChannelManager.initConnect
      RatesSaver.subscription
    }
  }
}

object ChannelManager extends Broadcaster {
  val CMDLocalShutdown: Command = CMDShutdown(None)
  val operationalListeners: Set[ChannelListener] = Set(ChannelManager, bag, GossipCatcher)
  private[this] var initialChainHeight: Long = app.kit.wallet.getLastBlockSeenHeight
  // Blocks download has not started yet and we don't know how many is left
  var currentBlocksLeft: Int = Int.MaxValue

  val socketEventsListener = new ConnectionListener {
    override def onTerminalError(nodeId: PublicKey) = fromNode(notClosing, nodeId).foreach(_ process CMDLocalShutdown)
    override def onOperational(nodeId: PublicKey, isCompat: Boolean) = fromNode(notClosing, nodeId).foreach(_ process CMDOnline)

    override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
      case update: ChannelUpdate => fromNode(notClosing, nodeId).foreach(_ process update)
      case errAll: Error if errAll.channelId == Zeroes => fromNode(notClosing, nodeId).foreach(_ process errAll)
      case m: ChannelMessage => notClosing.find(_.hasCsOr(_.commitments.channelId, null) == m.channelId).foreach(_ process m)
      case _ =>
    }

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
      // Set standardRun to be executed on new blocks
      // then set `currentBlocksLeft` less than MaxValue
      onBlock = standardRun
      standardRun(left)

      // Can already send payments at this point
      // because `currentBlocksLeft` is updated
      PaymentInfoWrap.resolvePending
    }

    lazy val standardRun: Int => Unit = left => {
      // LN payment before BTC peers on app start: tried in `oneTimeRun`
      // LN payment before BTC peers on app restart: tried in `oneTimeRun`
      // LN payment after BTC peers on app start: tried immediately since `currentBlocksLeft` < `Int.MacValue`
      // LN payment after BTC peers on app restart: tried immediately since `currentBlocksLeft` < `Int.MacValue`
      // LN payment after BTC peers disconnected: tried in `oneTimeRun` because `onPeerDisconnected` resets `onBlock`

      currentBlocksLeft = left
      if (currentBlocksLeft < 1) {
        // Send this once rescan is done to spare resources
        val cmd = CMDBestHeight(currentHeight, initialChainHeight)
        for (channel <- all) channel process cmd
        initialChainHeight = currentHeight
      }
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

  def currentHeight =
    // We may still be syncing but anyway a final chain height is known here
    if (currentBlocksLeft == Int.MaxValue) app.kit.wallet.getLastBlockSeenHeight
    else app.kit.wallet.getLastBlockSeenHeight + currentBlocksLeft

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
  val chanBackupWork = BackupWorker.workRequest(dbFileName, backupFileName, cloudSecret)
  var all = for (chanState <- ChannelWrap doGet db) yield createChannel(operationalListeners, chanState)
  def backUp = WorkManager.getInstance.beginUniqueWork("Backup", ExistingWorkPolicy.REPLACE, chanBackupWork).enqueue
  def fromNode(of: Vector[Channel], nodeId: PublicKey) = for (c <- of if c.data.announce.nodeId == nodeId) yield c
  def notClosing = for (c <- all if c.state != CLOSING) yield c

  def notClosingOrRefunding: Vector[Channel] = for {
    chan <- all if isOpening(chan) || isOperational(chan)
  } yield chan

  def delayedPublishes = {
    // Select all ShowDelayed which can't be published yet because cltv/csv delay is not cleared
    val statuses = all.map(_.data).collect { case cd: ClosingData => cd.bestClosing.getState }.flatten
    statuses.collect { case sd: ShowDelayed if !sd.isPublishable && sd.delay > Long.MinValue => sd }
  }

  def activeInFlightHashes = notClosingOrRefunding.flatMap(inFlightHtlcs).map(_.add.paymentHash)
  def frozenInFlightHashes = all.map(_.data).collect { case cd: ClosingData => cd.frozenPublishedHashes }.flatten
  def initConnect = for (c <- notClosing) ConnectionManager.connectTo(c.data.announce, notify = false)

  def createChannel(initialListeners: Set[ChannelListener], bootstrap: ChannelData): Channel = new Channel { self =>
    def SEND(m: LightningMessage) = for (w <- ConnectionManager.connections get data.announce.nodeId) w.handler process m

    def STORE(data: HasCommitments) = runAnd(data) {
      // Put updated data into db, schedule gdrive upload,
      // replace if upload already is pending, return data
      ChannelWrap put data
      backUp
    }

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
      repeat(app.olympus getChildTxs cd.commitTxs.map(_.txid), pickInc, 7 to 8).foreach(_ map CMDSpent foreach process, none)
      app.kit.wallet.addWatchedScripts(app.kit closingPubKeyScripts cd)
      BECOME(STORE(cd), CLOSING)
    }

    def CLOSEANDWATCH(cd: ClosingData) = {
      val tier12txs = for (state <- cd.tier12States) yield state.txn
      if (tier12txs.nonEmpty) app.olympus tellClouds TxUploadAct(tier12txs.toJson.toString.hex, Nil, "txs/schedule")
      repeat(app.olympus getChildTxs cd.commitTxs.map(_.txid), pickInc, 7 to 8).foreach(_ foreach bag.extractPreimage, none)
      // Collect all the commit txs publicKeyScripts and watch these scripts locally for future possible payment preimages
      app.kit.wallet.addWatchedScripts(app.kit closingPubKeyScripts cd)
      BECOME(STORE(cd), CLOSING)
    }

    def ASKREFUNDTX(ref: RefundingData) = {
      // Failsafe check in case if we are still in REFUNDING state after app is restarted
      val txsObs = app.olympus getChildTxs Seq(ref.commitments.commitInput.outPoint.txid)
      txsObs.foreach(_ map CMDSpent foreach process, none)
    }

    def ASKREFUNDPEER(some: HasCommitments, point: Point) = {
      val ref = RefundingData(some.announce, Some(point), some.commitments)
      app.kit.wallet.addWatchedScripts(app.kit fundingPubScript some)
      BECOME(STORE(ref), REFUNDING) SEND makeReestablish(some, 0L)
    }

    // First add listeners, then call
    // doProcess on current thread
    listeners = initialListeners
    doProcess(bootstrap)
  }

  // SENDING PAYMENTS

  def checkIfSendable(rd: RoutingData) =
    if (frozenInFlightHashes contains rd.pr.paymentHash) Left(app getString err_ln_frozen)
    else if (bag.getPaymentInfo(rd.pr.paymentHash).filter(_.status == SUCCESS).isSuccess) Left(app getString err_ln_fulfilled)
    // It may happen such that we had enough funds while were deciding whether to pay, but do not have enough funds currently
    else all.filter(isOperational).sortBy(estimateCanSend)(Ordering[Long].reverse).headOption match {
      case Some(chan) if estimateCanSend(chan) < rd.firstMsat => Left(app getString dialog_sum_big)
      case None => Left(app getString ln_no_open_chans)
      case _ => Right(rd)
    }

  def fetchRoutes(rd: RoutingData) = {
    // First we collect chans which in principle can handle a given payment sum right now, then prioritize less busy chans
    val from = all filter isOperational collect { case chan if estimateCanSend(chan) >= rd.firstMsat => chan.data.announce.nodeId }

    def withHints = for {
      tag <- Obs from rd.pr.routingInfo
      partialRoutes <- getRoutes(tag.route.head.nodeId)
      completeRoutes = partialRoutes.map(_ ++ tag.route)
    } yield Obs just completeRoutes

    def getRoutes(target: PublicKey) =
      if (rd.isReflexive) from.filter(_ != target) match {
        case restFrom if restFrom.isEmpty => Obs just Vector(Vector.empty)
        case restFrom if rd.useCache => RouteWrap.findRoutes(restFrom, target, rd)
        case restFrom => BadEntityWrap.findRoutes(restFrom, target, rd)
      } else from contains target match {
        case true => Obs just Vector(Vector.empty)
        case false if rd.routes.nonEmpty => Obs just Vector.empty
        case false if rd.useCache => RouteWrap.findRoutes(from, target, rd)
        case false => BadEntityWrap.findRoutes(from, target, rd)
      }

    val paymentRoutesObs =
      if (from.isEmpty) Obs error new LightningException
      else if (rd.isReflexive) Obs.zip(observables = withHints).map(_.flatten.toVector)
      // Normally rd does not conain routes at this point but it may have a (phone -> Joint -> payee) one
      else Obs.zip(getRoutes(rd.pr.nodeId) +: Obs.just(rd.routes) +: withHints).map(_.flatten.toVector)

    for {
      routeVec <- paymentRoutesObs
      busyMap = all.map(chan => chan.data.announce.nodeId -> inFlightHtlcs(chan).size).toMap
      unloadest = routeVec.sortBy(route => if (route.nonEmpty) busyMap(route.head.nodeId) else 0)
    } yield useFirstRoute(unloadest, rd)
  }

  def sendEither(foeRD: FullOrEmptyRD, noRoutes: RoutingData => Unit): Unit = foeRD match {
    // Find a local channel which is online, can send an amount and belongs to a correct peer
    case Right(rd) if activeInFlightHashes contains rd.pr.paymentHash =>
    case Left(emptyRD) => noRoutes(emptyRD)

    case Right(rd) =>
      // Empty used route means we're sending to peer and its nodeId is our target
      val targetNodeId = if (rd.usedRoute.isEmpty) rd.pr.nodeId else rd.usedRoute.head.nodeId
      val notViaShortChanId = if (rd.usedRoute.isEmpty) 0L else rd.usedRoute.last.shortChannelId

      all filter isOperational find { chan =>
        val isLoop = chan.hasCsOr(_.commitments.extraHop.exists(_.shortChannelId == notViaShortChanId), false)
        !isLoop && chan.data.announce.nodeId == targetNodeId && estimateCanSend(chan) >= rd.firstMsat
      } match {
        case None => sendEither(useFirstRoute(rd.routes, rd), noRoutes)
        case Some(targetChannel) => targetChannel process rd
      }
  }
}

object Vibrator {
  def vibrate = if (null != vib && vib.hasVibrator) vib.vibrate(Array(0L, 85, 200), -1)
  lazy val vib = app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
}