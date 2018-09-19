package com.lightning.walletapp

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.StartNodeView._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import com.lightning.walletapp.lnutils.olympus.CloudAct
import com.lightning.walletapp.ln.Scripts.pubKeyScript
import com.lightning.walletapp.helper.AES
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.script.ScriptBuilder
import org.bitcoinj.wallet.SendRequest
import fr.acinq.bitcoin.MilliSatoshi
import android.app.AlertDialog
import org.bitcoinj.core.Batch
import java.util.Collections
import android.os.Bundle

import android.widget.{ImageButton, TextView}
import scala.util.{Failure, Success, Try}


class LNStartFundActivity extends TimerActivity { me =>
  lazy val lnStartFundCancel = findViewById(R.id.lnStartFundCancel).asInstanceOf[ImageButton]
  lazy val lnStartFundDetails = findViewById(R.id.lnStartFundDetails).asInstanceOf[TextView]
  var whenBackPressed: Runnable = UITask(super.onBackPressed)
  override def onBackPressed = whenBackPressed.run

  def INIT(state: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_start_fund)

    app.TransData checkAndMaybeErase {
      case remoteNodeView @ RemoteNodeView(ann \ _) => proceed(None, remoteNodeView.asString(nodeFundView, "<br>"), ann)
      case hardcodedNodeView @ HardcodedNodeView(ann, _) => proceed(None, hardcodedNodeView.asString(nodeFundView, "<br>"), ann)
      case ann: NodeAnnouncement => proceed(None, HardcodedNodeView(ann, chansNumber.last).asString(nodeFundView, "<br>"), ann)
      case icr: IncomingChannelRequest => proceed(Some(icr), icr.nodeView.asString(nodeFundView, "<br>"), icr.nodeView.ann)
      case _ => finish
    }

    // Or back if resources are freed
  } else me exitTo classOf[MainActivity]

  def proceed(icrOpt: Option[IncomingChannelRequest], asString: String, ann: NodeAnnouncement) = {
    val freshChan = app.ChannelManager.createChannel(bootstrap = InitData(ann), initialListeners = Set.empty)
    lnStartFundCancel setOnClickListener onButtonTap(whenBackPressed.run)
    lnStartFundDetails setText asString.html

    class OpenListener extends ConnectionListener with ChannelListener {
      val noLossProtect = new LightningException(me getString err_ln_no_data_loss_protect)
      val peerOffline = new LightningException(me getString err_ln_peer_offline format ann.addresses.head.toString)
      override def onIncompatible(nodeId: PublicKey) = if (nodeId == ann.nodeId) onException(freshChan -> noLossProtect)
      override def onTerminalError(nodeId: PublicKey) = if (nodeId == ann.nodeId) onException(freshChan -> peerOffline)
      override def onDisconnect(nodeId: PublicKey) = if (nodeId == ann.nodeId) onException(freshChan -> peerOffline)

      override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
        case open: OpenChannel if nodeId == ann.nodeId && icrOpt.exists(_ isCorrect open) =>
          val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
          val theirUnspendableReserveSat = open.fundingSatoshis / LNParams.channelReserveToFundingRatio
          freshChan process Tuple2(LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript,
            System.currentTimeMillis, isFunder = false), open)

        case _: OpenChannel if nodeId == ann.nodeId => onException(freshChan -> new LightningException)
        case error: Error if nodeId == ann.nodeId => onException(freshChan -> error.exception)
        case msg: ChannelSetupMessage if nodeId == ann.nodeId => freshChan process msg
        case _ =>
      }

      override def onException = {
        case _ \ errorWhileOpening =>
          // Inform user, disconnect this channel, go back
          UITask(app toast errorWhileOpening.getMessage).run
          whenBackPressed.run
      }
    }

    abstract class LocalOpenListener extends OpenListener {
      override def onOperational(remotePeerCandidateNodeId: PublicKey) =
        // Remote peer has sent their Init so we ask user to provide an amount
        if (remotePeerCandidateNodeId == ann.nodeId) askLocalFundingConfirm.run

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // Peer has agreed to open a channel so now we create a real funding transaction
          // We create a funding transaction by replacing an output with a real one in a saved dummy funding transaction
          val req = cmd.batch replaceDummy pubKeyScript(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
          freshChan process CMDFunding(app.kit.sign(req).tx)

        case (_, wait: WaitFundingDoneData, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // Preliminary negotiations are complete, save channel and broadcast a fund tx
          freshChan.listeners = app.ChannelManager.operationalListeners
          app.ChannelManager.all +:= freshChan
          saveChan(wait)

          // Broadcast a funding transaction
          // Tell wallet activity to redirect to ops
          LNParams.broadcaster nullOnBecome freshChan
          app.TransData.value = FragWallet.REDIRECT
          me exitTo classOf[WalletActivity]
      }

      // Provide manual or batched amount
      def askLocalFundingConfirm: Runnable
    }

    def saveChan(some: HasCommitments) = {
      // First of all we should store this chan
      // error here will halt all further progress
      freshChan STORE some

      // Start watching a channel funding script and save a channel
      val fundingScript = some.commitments.commitInput.txOut.publicKeyScript
      app.kit.wallet.addWatchedScripts(Collections singletonList fundingScript)

      // Attempt to save a channel on the cloud right away
      val refund = RefundingData(some.announce, None, some.commitments)
      val encrypted = AES.encode(refund.toJson.toString, LNParams.cloudSecret)
      val act = CloudAct(encrypted, Seq("key" -> LNParams.cloudId.toString), "data/put")
      OlympusWrap tellClouds act
    }

    def localWalletListener = new LocalOpenListener {
      // Asks user to provide a funding amount manually

      def askLocalFundingConfirm = UITask {
        val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
        val maxCap = MilliSatoshi(math.min(app.kit.conf0Balance.value, LNParams.maxCapacity.amount) * 1000L)
        val minCap = MilliSatoshi(math.max(LNParams.broadcaster.perKwThreeSat * 3, LNParams.minCapacitySat) * 1000L)
        val rateManager = new RateManager(content) hint getString(amount_hint_newchan).format(denom withSign minCap,
          denom withSign LNParams.maxCapacity, denom withSign app.kit.conf0Balance)

        def next(msat: MilliSatoshi) = new TxProcessor {
          val dummyKey: PublicKey = randomPrivKey.publicKey
          val dummyScript = pubKeyScript(dummyKey, dummyKey)
          val pay = P2WSHData(msat, dummyScript)

          def futureProcess(unsignedRequest: SendRequest) = {
            val fee: Long = LNParams.broadcaster.perKwThreeSat
            val batch = Batch(unsignedRequest, dummyScript, null)
            val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
            val theirUnspendableReserveSat = batch.fundingAmountSat / LNParams.channelReserveToFundingRatio
            val localParams = LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript, System.currentTimeMillis, isFunder = true)
            val cmd = CMDOpenChannel(localParams, tempChanId = random getBytes 32, fee, batch, batch.fundingAmountSat, pushMsat = 0L)
            freshChan process cmd
          }

          def onTxFail(fundingError: Throwable) = {
            val bld = baseBuilder(title = messageWhenMakingTx(fundingError), null)
            mkCheckForm(alert => rm(alert)(finish), none, bld, dialog_ok, -1)
          }
        }

        def askAttempt(alert: AlertDialog) = rateManager.result match {
          case Success(ms) if ms < minCap => app toast dialog_sum_small
          case Success(ms) if ms > maxCap => app toast dialog_sum_big
          case Failure(reason) => app toast dialog_sum_empty
          case Success(ms) => rm(alert)(next(ms).start)
        }

        def useMax(alert: AlertDialog) = rateManager setSum Try(maxCap)
        val bld = baseBuilder(getString(ln_ops_start_fund_local_title).html, content)
        mkCheckFormNeutral(askAttempt, none, useMax, bld, dialog_next, dialog_cancel, dialog_max)
      }
    }

    def localBatchListener(batch: Batch) = new LocalOpenListener {
      // Uses a prebuilt batch which funds a channel and pays onchain

      def askLocalFundingConfirm = UITask {
        val text = batch asString ln_open_batch_confirm
        val title = getString(ln_ops_start_fund_local_title).html

        mkCheckForm(alert => rm(alert) {
          val fee: Long = LNParams.broadcaster.perKwThreeSat
          val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
          val theirUnspendableReserveSat = batch.fundingAmountSat / LNParams.channelReserveToFundingRatio
          val localParams = LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript, System.currentTimeMillis, isFunder = true)
          freshChan process CMDOpenChannel(localParams, tempChanId = random getBytes 32, fee, batch, batch.fundingAmountSat, pushMsat = 0L)
        }, none, baseBuilder(title, text), dialog_next, dialog_cancel)
      }
    }

    def remoteOpenListener(wsw: WSWrap) = new OpenListener {
      override def onOperational(remotePeerCandidateNodeId: PublicKey) =
        // #1 peer has provided an Init so we reassure that Funder is there
        if (remotePeerCandidateNodeId == ann.nodeId) wsw send wsw.params.start

      override def onProcessSuccess = {
        case (_, _: InitData, started: Started) if started.start == wsw.params.start =>
          // #2 funder has returned a Started which is identical to the one we have
          askExternalFundingConfirm(started).run
      }

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // #3 peer has accepted our proposal and we have all the data to request a funding tx
          val realKey = pubKeyScript(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
          wsw send PrepareFundingTx(wsw.params.userId, realKey)

        case (_, wait: WaitBroadcastRemoteData, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // #4 peer has signed our first commit so we can ask funder to broadcast a tx
          // important: first save a channel here, then ask for a funding broadcast

          saveChan(wait)
          app.ChannelManager.all +:= freshChan
          freshChan.listeners ++= app.ChannelManager.operationalListeners
          wsw send BroadcastFundingTx(wsw.params.userId, wait.txHash)

        case (_, _: WaitFundingDoneData, WAIT_FUNDING_DONE, WAIT_FUNDING_DONE) =>
          // #5 we have received a remote funding tx and may exit to ops page
          freshChan.listeners = app.ChannelManager.operationalListeners
          ExternalFunder.eliminateWSWrap(wsw, inform = false)
          app.TransData.value = FragWallet.REDIRECT
          me exitTo classOf[WalletActivity]
      }

      private def askExternalFundingConfirm(started: Started) = UITask {
        val capacity \ fundingFee = coloredIn(started.start.fundingAmount) -> coloredOut(started.fee)
        val content = getString(ex_fund_accept).format(started.start.host, capacity, fundingFee)

        mkCheckForm(alert => rm(alert) {
          val fee: Long = LNParams.broadcaster.perKwThreeSat
          val remoteFundSat = started.start.fundingAmount.amount
          val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
          val theirUnspendableReserveSat = wsw.params.start.fundingAmount.amount / LNParams.channelReserveToFundingRatio
          val localParams = LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript, System.currentTimeMillis, isFunder = true)
          freshChan process CMDOpenChannel(localParams, tempChanId = random getBytes 32, fee, batch = null, remoteFundSat, pushMsat = 0L)
        }, none, baseBuilder(getString(ln_ops_start_fund_external_title).html, content.html), dialog_next, dialog_cancel)
      }
    }

    def remoteOpenFundeeListener(icr: IncomingChannelRequest) = new OpenListener {
      override def onOperational(remoteFunderNodeId: PublicKey) = icr.requestChannel

      override def onBecome = {
        case (_, wait: WaitBroadcastRemoteData, WAIT_FOR_FUNDING, WAIT_FUNDING_DONE) =>
          // Preliminary negotiations are complete, save channel and wait for their tx
          freshChan.listeners = app.ChannelManager.operationalListeners
          app.ChannelManager.all +:= freshChan
          saveChan(wait)

          // Tell wallet activity to redirect to ops
          app.TransData.value = FragWallet.REDIRECT
          me exitTo classOf[WalletActivity]
      }
    }

    lazy val efListener = new ExternalFunderListener {
      // Exit this page once disconnected for whatever reason
      override def onMsg(msg: FundMsg) = freshChan process msg
      override def onRejection: Unit = whenBackPressed.run
    }

    lazy val openListener =
      ExternalFunder.worker -> FragLNStart.batchOpt -> icrOpt match {
        case Some(onlineWsw) \ None \ None => remoteOpenListener(onlineWsw)
        case None \ None \ Some(icr) => remoteOpenFundeeListener(icr)
        case None \ Some(batch) \ None => localBatchListener(batch)
        case _ => localWalletListener
      }

    whenBackPressed = UITask {
      freshChan.listeners -= openListener
      ConnectionManager.listeners -= openListener
      for (wsw <- ExternalFunder.worker) wsw.listeners -= efListener
      // Worker may have already been removed on some connection failure
      ConnectionManager.connections.get(ann.nodeId).foreach(_.disconnect)
      finish
    }

    freshChan.listeners += openListener
    ConnectionManager.listeners += openListener
    // Attempt to set a listener irregardles of whether it exists
    for (wsw <- ExternalFunder.worker) wsw.listeners += efListener
    ConnectionManager.connectTo(ann, notify = true)
  }
}