package com.lightning.walletapp

import R.string._
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
import com.lightning.walletapp.ln.crypto.Sphinx._
import com.google.common.util.concurrent.Service.State._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import org.bitcoinj.core.TransactionConfidence.ConfidenceType._

import scala.util.{Success, Try}
import rx.lang.scala.{Observable => Obs}
import scodec.bits.{BitVector, ByteVector}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}
import androidx.work.{ExistingWorkPolicy, WorkManager}
import android.content.{ClipData, ClipboardManager, Context}
import com.lightning.walletapp.helper.{AwaitService, RichCursor}
import com.lightning.walletapp.lnutils.JsonHttpUtils.{pickInc, repeat}
import com.lightning.walletapp.lnutils.olympus.{OlympusWrap, TxUploadAct}
import android.app.{Application, NotificationChannel, NotificationManager}
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.TimeUnit.MILLISECONDS
import android.support.v7.app.AppCompatDelegate
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.Wallet.BalanceType
import rx.lang.scala.schedulers.IOScheduler
import java.util.Collections.singletonList
import fr.acinq.bitcoin.Protocol.Zeroes
import org.bitcoinj.uri.BitcoinURI
import scala.concurrent.Future
import android.widget.Toast
import scodec.DecodeResult
import android.os.Build
import java.io.File


class WalletApp extends Application { me =>
  lazy val params = org.bitcoinj.params.TestNet3Params.get
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
  def plur1OrZero(opts: Array[String], num: Long) = if (num > 0) plur(opts, num).format(num) else opts(0)
  def getBufferUnsafe = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString
  def notMixedCase(s: String) = s.toLowerCase == s || s.toUpperCase == s

  def isAlive =
    if (null == kit || null == olympus || null == db || null == extendedNodeKey) false
    else kit.state match { case STARTING | RUNNING => true case _ => false }

  Utils.appReference = me
  override def onCreate = wrap(super.onCreate) {
    // These cannot be lazy vals because values may change
    Utils.fiatCode = prefs.getString(AbstractKit.FIAT_TYPE, "usd")
    Utils.denom = Utils denoms prefs.getInt(AbstractKit.DENOM_TYPE, 0)
    AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM)

    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N_MR1) {
      val importanceLevel = NotificationManager.IMPORTANCE_DEFAULT
      val srvChan = new NotificationChannel(AwaitService.CHANNEL_ID, "NC", importanceLevel)
      me getSystemService classOf[NotificationManager] createNotificationChannel srvChan
    }
  }

  def setBuffer(bufferText: String, notify: Boolean = true) = {
    val bufferContent = ClipData.newPlainText("wallet", bufferText)
    if (notify) me toast getString(copied_to_clipboard).format(bufferText)
    clipboardManager setPrimaryClip bufferContent
  }

  def mkNodeAnnouncement(id: PublicKey, na: NodeAddress, alias: String) =
    NodeAnnouncement(signature = sign(Zeroes, randomPrivKey), features = ByteVector.empty,
      timestamp = 0L, nodeId = id, (-128, -128, -128), alias take 16, addresses = na :: Nil)

  def emptyRD(pr: PaymentRequest, firstMsat: Long, useCache: Boolean) = {
    val useOnChainFeeBlock = prefs.getBoolean(AbstractKit.CAP_LN_FEES, false)
    RoutingData(pr, routes = Vector.empty, usedRoute = Vector.empty, onion = PacketAndSecrets(emptyOnionPacket, Vector.empty),
      firstMsat = firstMsat, lastMsat = 0L, lastExpiry = 0L, callsLeft = if (useOnChainFeeBlock) 8 else 4, useCache = useCache,
      airLeft = 0, onChainFeeBlock = useOnChainFeeBlock, onChainFeeBlockWasUsed = false, retriedRoutes = Vector.empty)
  }

  object TransData {
    var value: Any = new String
    private[this] val prefixes = PaymentRequest.prefixes.values mkString "|"
    private[this] val lnUrl = s"(?im).*?(lnurl)([0-9]{1,}[a-z0-9]+){1}".r.unanchored
    private[this] val lnPayReq = s"(?im).*?($prefixes)([0-9]{1,}[a-z0-9]+){1}".r.unanchored
    private[this] val shortNodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:\\.\\-_]+)".r.unanchored
    val nodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:\\.\\-_]+):([0-9]+)".r.unanchored

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

    def toBitcoinUri(addr: String) = bitcoinUri(s"bitcoin:$addr")
    def recordValue(rawInputText: String) = value = parse(rawInputText)
    def parse(rawInputTextToParse: String) = rawInputTextToParse take 2880 match {
      case bitcoinUriLink if bitcoinUriLink startsWith "bitcoin" => bitcoinUri(bitcoinUriLink)
      case bitcoinUriLink if bitcoinUriLink startsWith "BITCOIN" => bitcoinUri(bitcoinUriLink.toLowerCase)
      case nodeLink(key, host, port) => mkNodeAnnouncement(PublicKey(ByteVector fromValidHex key), NodeAddress.fromParts(host, port.toInt), host)
      case shortNodeLink(key, host) => mkNodeAnnouncement(PublicKey(ByteVector fromValidHex key), NodeAddress.fromParts(host, port = 9735), host)
      case lnPayReq(prefix, data) => PaymentRequest.read(s"$prefix$data")
      case lnUrl(prefix, data) => LNUrl.fromBech32(s"$prefix$data")
      case _ => toBitcoinUri(rawInputTextToParse)
    }
  }

  abstract class WalletKit extends AbstractKit {
    def currentAddress = wallet currentAddress KeyPurpose.RECEIVE_FUNDS
    def conf0Balance = wallet getBalance BalanceType.ESTIMATED_SPENDABLE // Returns all utxos
    def blockSend(txj: Transaction) = peerGroup.broadcastTransaction(txj, 1).broadcast.get
    def shutDown = none

    def trustedNodeTry = Try(nodeaddress.decode(BitVector fromValidHex wallet.getDescription).require.value)
    def fundingPubScript(some: HasNormalCommits) = singletonList(some.commitments.commitInput.txOut.publicKeyScript: org.bitcoinj.script.Script)
    def closingPubKeyScripts(cd: ClosingData) = cd.commitTxs.flatMap(_.txOut).map(_.publicKeyScript: org.bitcoinj.script.Script).asJava
    def useCheckPoints(time: Long) = CheckpointManager.checkpoint(params, getAssets open "checkpoints-testnet.txt", store, time)

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
      peerGroup.addDisconnectedEventListener(ChannelManager.chainEventsListener)

      Future {
        trustedNodeTry match {
          case Success(nodeAddress) =>
            val isa = NodeAddress.toInetSocketAddress(nodeAddress)
            val trusted = new PeerAddress(params, isa.getAddress, isa.getPort)
            peerGroup.addAddress(trusted)

          case _ =>
            peerGroup addPeerDiscovery new DnsDiscovery(params)
            peerGroup.setMaxConnections(5)
        }
      }

      peerGroup.setMinRequiredProtocolVersion(70015)
      peerGroup.setDownloadTxDependencies(0)
      peerGroup.setPingIntervalMsec(10000)
      peerGroup.addWallet(wallet)

      for {
        _ <- Obs just null subscribeOn IOScheduler.apply delay 20.seconds
        offlineChannel <- ChannelManager.all if offlineChannel.permanentOffline
        if offlineChannel.data.announce.addresses.headOption.forall(_.canBeUpdatedIfOffline)
        // Call findNodes without `retry` wrapper because it gives harmless `Obs.empty` on error
        Vector(ann1 \ _, _*) <- app.olympus findNodes offlineChannel.data.announce.nodeId.toString
      } offlineChannel process ann1

      ConnectionManager.listeners += ChannelManager.socketEventsListener
      startBlocksDownload(ChannelManager.chainEventsListener)
      // Try to clear act leftovers if no channels are left
      app.olympus tellClouds OlympusWrap.CMDStart
      // Did not really happen, just sync our db
      PaymentInfoWrap.onHostedCommitsRestored
      ChannelManager.initConnect
      RatesSaver.subscription
    }
  }
}

object ChannelManager extends Broadcaster {
  val operationalListeners = Set(ChannelManager, bag)
  val CMDLocalShutdown = CMDShutdown(scriptPubKey = None)
  private val chanBackupWork = BackupWorker.workRequest(backupFileName, cloudSecret)
  var currentBlocksLeft = Option.empty[Int]

  val socketEventsListener = new ConnectionListener {
    override def onOperational(nodeId: PublicKey, isCompat: Boolean) =
      fromNode(nodeId).foreach(_ process CMDSocketOnline)

    override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
      case channelUpdate: ChannelUpdate => fromNode(nodeId).foreach(_ process channelUpdate)
      case nodeLevelError: Error if nodeLevelError.channelId == Zeroes => fromNode(nodeId).foreach(_ process nodeLevelError)
      case cm: ChannelMessage => fromNode(nodeId).find(_.getCommits.map(_.channelId) contains cm.channelId).foreach(_ process cm)
      case _ =>
    }

    override def onHostedMessage(ann: NodeAnnouncement, msg: HostedChannelMessage) =
      fromNode(ann.nodeId).find(_.getCommits.map(_.channelId) contains ann.hostedChanId).foreach(_ process msg)

    override def onDisconnect(nodeId: PublicKey) = {
      fromNode(nodeId).foreach(_ process CMDSocketOffline)
      Obs.just(null).delay(5.seconds).foreach(_ => initConnect)
    }
  }

  val chainEventsListener = new TxTracker with BlocksListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(offPeer: Peer, numPeers: Int) = if (numPeers < 1) currentBlocksLeft = None
    def onBlocksDownloaded(peer: Peer, b: Block, fb: FilteredBlock, left: Int) = onBlock(left)
    override def onChainDownloadStarted(peer: Peer, left: Int) = onBlock(left)

    def onCoinsReceived(wallet: Wallet, txj: Transaction, a: Coin, b: Coin) = onChainTx(txj)
    def onCoinsSent(wallet: Wallet, txj: Transaction, a: Coin, b: Coin) = onChainTx(txj)

    def onBlock(blocksLeft: Int) = {
      val firstCall = currentBlocksLeft.isEmpty
      currentBlocksLeft = Some(blocksLeft)

      // Let channels know immediately, important for hosted ones
      // then also repeat on each next incoming last block to save resouces
      if (firstCall || blocksLeft < 1) all.foreach(_ process CMDChainTipKnown)
      // Don't call this on each new block but only on (re)connection
      if (firstCall) PaymentInfoWrap.resolvePending
    }

    def onChainTx(txj: Transaction) = {
      val cmdOnChainSpent = CMDSpent(txj)
      all.foreach(_ process cmdOnChainSpent)
      bag.extractPreimage(cmdOnChainSpent.tx)
    }
  }

  // BROADCASTER IMPLEMENTATION

  def perKwSixSat: Long = RatesSaver.rates.feeSix.value / 4
  def perKwThreeSat: Long = RatesSaver.rates.feeThree.value / 4
  def blockDaysLeft: Int = currentBlocksLeft.map(_ / blocksPerDay).getOrElse(Int.MaxValue)
  def currentHeight: Int = app.kit.wallet.getLastBlockSeenHeight + currentBlocksLeft.getOrElse(0)
  def currentBlockDay: Int = currentHeight / blocksPerDay

  def getTx(txid: ByteVector) = {
    val wrapped = Sha256Hash wrap txid.toArray
    Option(app.kit.wallet getTransaction wrapped)
  }

  def getStatus(txid: ByteVector) = getTx(txid) map { tx =>
    val isTxDead = tx.getConfidence.getConfidenceType == DEAD
    tx.getConfidence.getDepthInBlocks -> isTxDead
  } getOrElse 0 -> false

  // CHANNEL LISTENER IMPLEMENTATION

  override def onProcessSuccess = {
    case (_: NormalChannel, close: ClosingData, _: Command) =>
      // Repeatedly spend everything we can in this state in case it was unsuccessful before
      val tier12Publishable = for (state <- close.tier12States if state.isPublishable) yield state.txn
      val toSend = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ tier12Publishable
      for (tx <- toSend) try app.kit blockSend tx catch none

    case (chan: NormalChannel, wait: WaitFundingDoneData, CMDChainTipKnown) if wait.our.isEmpty =>
      // Once all pending blocks are received, check if funding transaction has been confirmed
      val fundingDepth \ _ = broadcaster.getStatus(txid = wait.fundingTx.txid)
      if (fundingDepth >= minDepth) chan process CMDConfirmed(wait.fundingTx)

    case (chan: Channel, _, CMDSocketOnline) =>
      // Memo that channel was online at least once
      chan.permanentOffline = false
  }

  override def onBecome = {
    // Repeatedly resend a funding tx, update feerate on becoming open
    case (_: NormalChannel, wait: WaitFundingDoneData, _, _) => app.kit blockSend wait.fundingTx
    case (chan: NormalChannel, _: NormalData, SLEEPING, OPEN) => chan process CMDFeerate(perKwThreeSat)
  }

  // CHANNEL CREATION AND MANAGEMENT

  var all: Vector[Channel] = ChannelWrap doGet db collect {
    case normal: HasNormalCommits => createChannel(operationalListeners, normal)
    case hosted: HostedCommits => createHostedChannel(operationalListeners, hosted)
    case other => throw new RuntimeException(s"Can't create channel with $other")
  }

  def delayedPublishes = {
    val statuses = all.map(_.data).collect { case cd: ClosingData => cd.bestClosing.getState }
    // Select all ShowDelayed which can't be published yet because cltv/csv delays are not cleared on them
    statuses.flatten.collect { case sd: ShowDelayed if !sd.isPublishable && sd.delay > Long.MinValue => sd }
  }

  // AIR

  def airCanSendInto(targetChan: Channel) = for {
    canSend <- all.filter(isOperational).diff(targetChan :: Nil).map(_.estCanSendMsat)
    // While rebalancing, payments from other channels will lose some off-chain fee
    canSendFeeIncluded = canSend - maxAcceptableFee(canSend, hops = 3)
    // Estimation should be smaller than original but not negative
    if canSendFeeIncluded < canSend && canSendFeeIncluded > 0L
  } yield canSendFeeIncluded

  def estimateAIRCanSend = {
    // We are ultimately bound by the useful capacity of the largest channel
    val airCanSend = mostFundedChanOpt.map(chan => chan.estCanSendMsat + airCanSendInto(chan).sum)
    val largestCapOpt = all.filter(isOperational).map(_.estNextUsefulCapacityMsat).reduceOption(_ max _)
    math.min(airCanSend getOrElse 0L, largestCapOpt getOrElse 0L)
  }

  def accumulatorChanOpt(rd: RoutingData) =
    all.filter(chan => isOperational(chan) && channelAndHop(chan).nonEmpty)
      .filter(_.estNextUsefulCapacityMsat >= rd.withMaxOffChainFeeAdded)
      .sortBy(_.estCanReceiveMsat).headOption // Smallest receivable

  // CHANNEL

  def mostFundedChanOpt = all.filter(isOperational).sortBy(_.estCanSendMsat).lastOption
  def activeInFlightHashes = all.filter(isOperational).flatMap(_.inFlightHtlcs).map(_.add.paymentHash)
  // We need to connect the rest of channels including special cases like REFUNDING normal channel and SUSPENDED hosted channel
  def initConnect = for (chan <- all if chan.state != CLOSING) ConnectionManager.connectTo(chan.data.announce, notify = false)
  def hasNormalChanWith(nodeId: PublicKey) = fromNode(nodeId).exists(chan => isOpeningOrOperational(chan) && !chan.isHosted)
  def backUp = WorkManager.getInstance.beginUniqueWork("Backup", ExistingWorkPolicy.REPLACE, chanBackupWork).enqueue
  def fromNode(nodeId: PublicKey) = for (chan <- all if chan.data.announce.nodeId == nodeId) yield chan
  def attachListener(lst: ChannelListener) = for (chan <- all) chan.listeners += lst
  def detachListener(lst: ChannelListener) = for (chan <- all) chan.listeners -= lst

  def createHostedChannel(initListeners: Set[ChannelListener], bootstrap: ChannelData) = new HostedChannel { self =>
    def SEND(msg: LightningMessage) = for (work <- ConnectionManager.workers get data.announce.nodeId) work.handler process msg
    def STORE[T <: ChannelData](data: T) = runAnd(data)(ChannelWrap put data)
    listeners = initListeners
    doProcess(bootstrap)
  }

  def createChannel(initListeners: Set[ChannelListener], bootstrap: ChannelData) = new NormalChannel { self =>
    def SEND(msg: LightningMessage) = for (work <- ConnectionManager.workers get data.announce.nodeId) work.handler process msg

    def STORE[T <: ChannelData](data: T) = runAnd(data) {
      // Put updated data into db and schedule gdrive upload
      ChannelWrap put data
      backUp
    }

    def REV(cs: NormalCommits, rev: RevokeAndAck) = for {
      tx <- cs.remoteCommit.txOpt // We use old commitments to save a punishment for remote commit before it gets dropped
      myBalance = cs.remoteCommit.spec.toRemoteMsat // Local commit is cleared by now, remote still has relevant balance
      watchtowerFee = broadcaster.perKwThreeSat * 3 // Scorched earth policy becuase watchtower can't regenerate tx
      revocationInfo = Helpers.Closing.makeRevocationInfo(cs, tx, rev.perCommitmentSecret, watchtowerFee)
      serialized = LightningMessageCodecs.serialize(revocationInfoCodec encode revocationInfo)
    } db.change(RevokedInfoTable.newSql, tx.txid, cs.channelId, myBalance, serialized)

    def GETREV(cs: NormalCommits, tx: fr.acinq.bitcoin.Transaction) = {
      val databaseCursor = db.select(RevokedInfoTable.selectTxIdSql, tx.txid)
      val rc = RichCursor(databaseCursor).headTry(_ string RevokedInfoTable.info)

      for {
        serialized <- rc.toOption
        bitVec = BitVector.fromValidHex(serialized)
        DecodeResult(ri, _) <- revocationInfoCodec.decode(bitVec).toOption
        perCommitmentSecret <- Helpers.Closing.extractCommitmentSecret(cs, tx)
        riWithCurrentFeeRate = ri.copy(feeRate = ri.feeRate max broadcaster.perKwThreeSat)
        ri1 = Helpers.Closing.reMakeRevocationInfo(riWithCurrentFeeRate, cs, tx, perCommitmentSecret)
      } yield Helpers.Closing.claimRevokedRemoteCommitTxOutputs(ri1, tx)
    }

    def CLOSEANDWATCH(cd: ClosingData) = {
      val tier12txs = cd.tier12States.map(_.txn.bin).toVector
      if (tier12txs.nonEmpty) app.olympus tellClouds TxUploadAct(txvec.encode(tier12txs).require.toByteVector, Nil, "txs/schedule")
      // In case of breach after publishing a revoked remote commit our peer may further publish Timeout and Success HTLC outputs
      // our job here is to watch for every output of every revoked commit tx and re-spend it before their CSV delay runs out
      repeat(app.olympus getChildTxs cd.commitTxs.map(_.txid), pickInc, 7 to 8).foreach(_ map CMDSpent foreach process, none)
      repeat(app.olympus getChildTxs cd.commitTxs.map(_.txid), pickInc, 7 to 8).foreach(_ foreach bag.extractPreimage, none)
      // Collect all the commit txs publicKeyScripts and watch these scripts locally for future possible payment preimages
      app.kit.wallet.addWatchedScripts(app.kit closingPubKeyScripts cd)
      BECOME(STORE(cd), CLOSING)
    }

    def ASKREFUNDPEER(some: HasNormalCommits, point: Point) = {
      val msg = ByteVector.fromValidHex("please publish your local commitment".s2hex)
      val ref = RefundingData(some.announce, Some(point), some.commitments)
      val error = Error(some.commitments.channelId, msg)

      // Send both invalid reestablish and an error
      app.kit.wallet.addWatchedScripts(app.kit fundingPubScript some)
      BECOME(STORE(ref), REFUNDING) SEND makeReestablish(some, 0L)
      SEND(error)
    }

    listeners = initListeners
    doProcess(bootstrap)
  }

  // SENDING PAYMENTS

  def checkIfSendable(rd: RoutingData) = {
    val isFulfilledAlready = bag.getPaymentInfo(rd.pr.paymentHash).filter(_.status == SUCCESS)
    if (isFulfilledAlready.isSuccess) Left(err_ln_fulfilled, NOT_SENDABLE) else mostFundedChanOpt.map(_.estCanSendMsat) match {
      // May happen such that we had enough while were deciding whether to pay, but do not have enough funds now, also check extended options
      case Some(max) if max < rd.firstMsat && rd.airLeft > 1 && estimateAIRCanSend >= rd.firstMsat => Left(dialog_sum_big, SENDABLE_AIR)
      case Some(max) if max < rd.firstMsat => Left(dialog_sum_big, NOT_SENDABLE)
      case None => Left(err_no_data, NOT_SENDABLE)
      case _ => Right(rd)
    }
  }

  def fetchRoutes(rd: RoutingData) = {
    // First we collect chans which in principle can handle a given payment sum right now, then prioritize less busy chans
    val from = all.filter(chan => isOperational(chan) && chan.estCanSendMsat >= rd.firstMsat).map(_.data.announce.nodeId).distinct

    def withHints = for {
      tag <- Obs from rd.pr.routingInfo
      partialRoutes <- getRoutes(tag.route.head.nodeId)
    } yield Obs just partialRoutes.map(_ ++ tag.route)

    def getRoutes(target: PublicKey) =
      if (rd.isReflexive) from diff Vector(target) match {
        case restFrom if restFrom.isEmpty => Obs just Vector(Vector.empty)
        case restFrom if rd.useCache => RouteWrap.findRoutes(restFrom, target, rd)
        case restFrom => BadEntityWrap.findRoutes(restFrom, target, rd)
      } else from contains target match {
        case false if rd.useCache => RouteWrap.findRoutes(from, target, rd)
        case false => BadEntityWrap.findRoutes(from, target, rd)
        case true => Obs just Vector(Vector.empty)
      }

    val paymentRoutesObs =
      if (from.isEmpty) Obs error new LightningException("No sources")
      else if (rd.isReflexive) Obs.zip(withHints).map(_.flatten.toVector)
      else Obs.zip(getRoutes(rd.pr.nodeId) +: withHints).map(_.flatten.toVector)

    for {
      rs <- paymentRoutesObs
      busyMap = Tools.toMap[Channel, PublicKey, Int](all, _.data.announce.nodeId, channel => channel.inFlightHtlcs.size)
      openMap = Tools.toMap[Channel, PublicKey, Int](all, _.data.announce.nodeId, channel => if (channel.state == OPEN) 0 else 1)
      foundRoutes = rs.sortBy(estTotalRouteFee).sortBy(busyMap compose rd.nextNodeId).sortBy(openMap compose rd.nextNodeId)
      // We may have out of band routes in RD (e.g. extracted from lnurl-pay), append them to found routes
    } yield useFirstRoute(foundRoutes ++ rd.routes, rd)
  }

  def sendEither(foeRD: FullOrEmptyRD, noRoutes: RoutingData => Unit): Unit = foeRD match {
    // Find a channel which can send an amount and belongs to a correct peer, not necessairly online
    case Right(rd) if activeInFlightHashes contains rd.pr.paymentHash =>
    case Left(emptyRD) => noRoutes(emptyRD)

    case Right(rd) =>
      all filter isOperational find { chan =>
        // Reflexive payment may happen through two chans belonging to the same peer
        // here we must make sure we don't accidently use terminal channel as source one
        val excludeShortChannelId = if (rd.usedRoute.isEmpty) 0L else rd.usedRoute.last.shortChannelId
        val isLoop = chan.getCommits.flatMap(_.updateOpt).exists(_.shortChannelId == excludeShortChannelId)
        !isLoop && chan.data.announce.nodeId == rd.nextNodeId(rd.usedRoute) && chan.estCanSendMsat >= rd.firstMsat
      } match {
        case None => sendEither(useFirstRoute(rd.routes, rd), noRoutes)
        case Some(targetGoodChannel) => targetGoodChannel process rd
      }
  }
}

object Vibrator {
  private var lastVibrated = 0L
  private val vib = app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  def canVibrate = null != vib && vib.hasVibrator && lastVibrated < System.currentTimeMillis - 3000L

  def vibrate = if (canVibrate) {
    vib.vibrate(Array(0L, 85, 200), -1)
    lastVibrated = System.currentTimeMillis
  }
}