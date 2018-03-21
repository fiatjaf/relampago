package com.lightning.wallet

import R.string._
import spray.json._
import org.bitcoinj.core._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import com.lightning.wallet.Utils._
import com.lightning.wallet.lnutils._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.muddzdev.styleabletoastlibrary.StyleableToast
import collection.JavaConverters.seqAsJavaListConverter
import com.lightning.wallet.lnutils.olympus.OlympusWrap
import com.lightning.wallet.lnutils.olympus.CloudAct
import java.util.concurrent.TimeUnit.MILLISECONDS
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.wallet.Wallet.BalanceType
import org.bitcoinj.crypto.KeyCrypterScrypt
import fr.acinq.bitcoin.Crypto.PublicKey
import com.google.protobuf.ByteString
import java.net.InetSocketAddress
import android.app.Application
import scala.util.Try
import java.io.File

import com.google.common.util.concurrent.Service.State.{RUNNING, STARTING}
import org.bitcoinj.uri.{BitcoinURI, BitcoinURIParseException}
import android.content.{ClipData, ClipboardManager, Context}
import org.bitcoinj.wallet.{Protos, SendRequest, Wallet}
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
  def toast(message: String): Unit = StyleableToast.makeText(me, message, R.style.infoToast).show
  def clipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def isAlive = if (null == kit) false else kit.state match { case STARTING | RUNNING => null != db case _ => false }
  def plurOrZero(opts: Array[String], number: Long) = if (number > 0) plur(opts, number) format number else opts(0)
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
    clipboardManager setPrimaryClip ClipData.newPlainText("wallet", text)
    me toast getString(copied_to_clipboard).format(text)
  }

  def encryptWallet(wallet: Wallet, pass: CharSequence) = {
    val salt = ByteString copyFrom KeyCrypterScrypt.randomSalt
    val builder = Protos.ScryptParameters.newBuilder.setSalt(salt)
    val crypter = new KeyCrypterScrypt(builder.setN(65536).build)
    wallet.encrypt(crypter, crypter deriveKey pass)
  }

  object TransData {
    var value: Any = _ // must be null at start
    val lnLink = "(?i)(lightning:)?([a-zA-Z0-9]+)\\W*".r
    val nodeLink = "([a-fA-F0-9]{66})@([a-zA-Z0-9:\\.\\-_]+):([0-9]+)".r

    def recordValue(rawText: String) = value = rawText match {
      case raw if raw startsWith "bitcoin" => new BitcoinURI(params, raw)
      case lnLink(_, body) if notMixedCase(body) => PaymentRequest read body.toLowerCase
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
      case _: org.bitcoinj.core.WrongNetworkException => err(err_different_net)
      case _: org.bitcoinj.core.AddressFormatException => err(err_address)
      case _: BitcoinURIParseException => err(err_uri)
      case _: Throwable => err(err_general)
    }
  }

  object ChannelManager {
    type ChannelVec = Vector[Channel]
    val operationalListeners = Set(broadcaster, bag, GossipCatcher)
    // All stored channels which would receive CMDSpent, CMDBestHeight and nothing else
    var all: ChannelVec = for (data <- ChannelWrap.get) yield createChannel(operationalListeners, data)

    def fromNode(of: ChannelVec, ann: NodeAnnouncement) = for (c <- of if c.data.announce == ann) yield c
    def canSend(amount: Long) = for (c <- all if isOperationalOpen(c) && estimateCanSend(c) > amount) yield c
    def notClosingOrRefunding = for (c <- all if c.state != Channel.CLOSING && c.state != Channel.REFUNDING) yield c
    def notClosing = for (c <- all if c.state != Channel.CLOSING) yield c

    def activeInFlightHashes = notClosingOrRefunding.flatMap(inFlightOutgoingHtlcs).map(_.add.paymentHash)
    def frozenInFlightHashes = all.diff(notClosingOrRefunding).flatMap(inFlightOutgoingHtlcs).map(_.add.paymentHash)
    def initConnect = for (chan <- notClosing) ConnectionManager connectTo chan.data.announce

    val chainEventsListener = new TxTracker with BlocksListener {
      override def txConfirmed(tx: Transaction) = for (chan <- notClosing) chan process CMDConfirmed(tx)
      override def onBlocksDownloaded(peer: Peer, block: Block, fb: FilteredBlock, left: Int) = tellHeight(left)
      override def onChainDownloadStarted(peer: Peer, left: Int) = tellHeight(left)

      override def coinsSent(txj: Transaction) = {
        // We always attempt to extract a payment preimage
        // just assuming any incoming tx may contain it

        val spent = CMDSpent(txj)
        bag.extractPreimg(spent.tx)
        all.foreach(_ process spent)
      }

      def tellHeight(left: Int) = {
        // No matter how many blocks are left we only send a CMD once the last block is done
        if (left < 1) for (chan <- all) chan process CMDBestHeight(broadcaster.currentHeight)
        // The fact that we are downloading blocks means we know a best chain height
        broadcaster.bestHeightObtained = true
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

      def maybeReconnect(chans: ChannelVec, ann: NodeAnnouncement) = if (chans.nonEmpty) {
        // Immediately inform affected channels and try to reconnect back again in 5 seconds
        Obs.just(ann).delay(5.seconds).subscribe(ConnectionManager.connectTo, none)
        chans.foreach(_ process CMDOffline)
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
        OlympusWrap.getChildTxs(commits).foreach(_ foreach bag.extractPreimg, Tools.errlog)
        BECOME(STORE(cd), CLOSING)

        cd.tier12States.map(_.txn) match {
          case Nil => Tools log "Closing channel does not have tier 1-2 transactions"
          case txs => OlympusWrap tellClouds CloudAct(txs.toJson.toString.hex, Nil, "txs/schedule")
        }
      }
    }

    def withRoutesAndOnionRD(rd: RoutingData) = {
      val isFrozen = frozenInFlightHashes.contains(rd.pr.paymentHash)
      if (isFrozen) Obs error new LightningException(me getString err_ln_frozen)
      else withRoutesAndOnionRDFrozenAllowed(rd)
    }

    def withRoutesAndOnionRDFrozenAllowed(rd: RoutingData) = {
      val isInFlight = activeInFlightHashes.contains(rd.pr.paymentHash)
      val isDone = bag.getPaymentInfo(rd.pr.paymentHash).filter(_.actualStatus == SUCCESS)
      val capablePeerNodes = canSend(rd.firstMsat).map(_.data.announce.nodeId).toSet

      if (isInFlight) Obs error new LightningException(me getString err_ln_in_flight)
      else if (isDone.isSuccess) Obs error new LightningException(me getString err_ln_fulfilled)
      else if (capablePeerNodes.isEmpty) Obs error new LightningException(me getString err_ln_no_route)
      else if (broadcaster.bestHeightObtained) addRoutesAndOnion(capablePeerNodes, rd)
      else Obs error new LightningException(me getString dialog_chain_behind)
    }

    def addRoutesAndOnion(peers: Set[PublicKey], rd: RoutingData) = {
      def findRemoteRoutes(targetNodeId: PublicKey) = BadEntityWrap.findRoutes(peers, targetNodeId)
      // If source node contains target node then we are paying directly to our peer, otherwise fetch additional payment routes
      def getRoutes(target: PublicKey) = if (peers contains target) Obs just Vector(Vector.empty) else findRemoteRoutes(target)

      def withExtraPart = for {
        tag <- Obs from rd.pr.routingInfo
        partialRoutes <- getRoutes(tag.route.head.nodeId)
        completeRoutes = partialRoutes.map(_ ++ tag.route)
      } yield Obs just completeRoutes

      // If payment request contains extra routing info then we ask for assisted routes, otherwise we directly ask for payee id
      val routesObs = if (rd.pr.routingInfo.isEmpty) getRoutes(rd.pr.nodeId) else Obs.zip(withExtraPart).map(_.flatten.toVector)
      // Update RD with routes and then we can make an onion out of the first available cheapest route while saving the rest
      // remote call may return empty route here in which case `noRouteLeft` will be fired later
      for (routes <- routesObs) yield useFirstRoute(routes.sortBy(_.size), rd)
    }

    def send(rd: RoutingData, noRouteLeft: RoutingData => Unit): Unit = {
      // Find a local channel which has enough funds, is online and belongs to a correct peer
      // empty used route means we're sending to our peer and should use it's nodeId as a target
      val target = if (rd.usedRoute.isEmpty) rd.pr.nodeId else rd.usedRoute.head.nodeId
      val channelOpt = canSend(rd.firstMsat).find(_.data.announce.nodeId == target)

      channelOpt match {
        case Some(targetChannel) => targetChannel process rd
        case None => sendEither(useRoutesLeft(rd), noRouteLeft)
      }
    }

    def sendEither(foeRD: FullOrEmptyRD, noRouteLeft: RoutingData => Unit) = foeRD match {
      case Right(rdValidPaymentRoutePresent) => send(rdValidPaymentRoutePresent, noRouteLeft)
      case Left(rdEmptyPaymentRoute) => noRouteLeft(rdEmptyPaymentRoute)
    }
  }

  abstract class WalletKit extends AbstractKit {
    type ScriptSeq = Seq[org.bitcoinj.script.Script]
    def blockingSend(tx: Transaction): String = nonBlockingSend(tx).get.toString
    def nonBlockingSend(tx: Transaction) = peerGroup.broadcastTransaction(tx, 1).broadcast
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

    def decryptSeed(pass: String) = {
      val crypter = wallet.getKeyCrypter
      val aesKey = crypter deriveKey pass
      val chainSeed = wallet.getKeyChainSeed
      chainSeed.decrypt(crypter, pass, aesKey)
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