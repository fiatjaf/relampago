package com.lightning.walletapp

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import scala.collection.JavaConverters._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.StartNodeView._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import com.lightning.walletapp.lnutils.olympus.CloudAct
import com.lightning.walletapp.helper.AES
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.script.ScriptBuilder
import org.bitcoinj.wallet.SendRequest
import android.app.AlertDialog
import java.util.TimerTask
import android.os.Bundle

import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Satoshi, Script}
import org.bitcoinj.core.{Coin, TransactionOutput}
import android.widget.{ImageButton, TextView}
import scala.util.{Failure, Success, Try}


class LNStartFundActivity extends TimerActivity { me =>
  var whenBackPressed: Runnable = UITask(super.onBackPressed)
  lazy val lnStartFundCancel = findViewById(R.id.lnStartFundCancel).asInstanceOf[ImageButton]
  lazy val lnStartFundDetails = findViewById(R.id.lnStartFundDetails).asInstanceOf[TextView]
  override def onBackPressed = whenBackPressed.run

  def INIT(state: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_start_fund)

    app.TransData.value match {
      case remoteView @ RemoteNodeView(ann \ _) => proceed(remoteView.asString(nodeFundView, "<br>"), ann)
      case hardcodedView @ HardcodedNodeView(ann, _) => proceed(hardcodedView.asString(nodeFundView, "<br>"), ann)
      case ann: NodeAnnouncement => proceed(HardcodedNodeView(ann, chansNumber.last).asString(nodeFundView, "<br>"), ann)
      case _ => finish
    }

    app.TransData.value = null
    // Or back if resources are freed
  } else me exitTo classOf[MainActivity]

  def pubKeyScript(pub1: PublicKey, pub2: PublicKey) = {
    val multisigScript = Scripts.multiSig2of2(pub1, pub2)
    Script.write(Script pay2wsh multisigScript)
  }

  def proceed(asString: String, announce: NodeAnnouncement) = {
    val freshChan = app.ChannelManager.createChannel(Set.empty, InitData apply announce)
    lnStartFundCancel setOnClickListener onButtonTap(whenBackPressed.run)
    lnStartFundDetails setText asString.html

    lazy val openListener = new ConnectionListener with ChannelListener { self =>
      override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
        case msg: ChannelSetupMessage if nodeId == announce.nodeId => freshChan process msg
        case err: Error if nodeId == announce.nodeId => onException(freshChan -> err.exception)
        case _ =>
      }

      val noLossProtect = new LightningException(me getString err_ln_no_data_loss_protect)
      val peerOffline = new LightningException(me getString err_ln_peer_offline format announce.addresses.head.toString)
      override def onOperational(nodeId: PublicKey, their: Init) = if (nodeId == announce.nodeId) askForFunding(their).run
      override def onIncompatible(nodeId: PublicKey) = if (nodeId == announce.nodeId) onException(freshChan -> noLossProtect)
      override def onTerminalError(nodeId: PublicKey) = if (nodeId == announce.nodeId) onException(freshChan -> peerOffline)
      override def onDisconnect(nodeId: PublicKey) = if (nodeId == announce.nodeId) onException(freshChan -> peerOffline)

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // Peer has agreed to open a channel so now we create a real funding transaction
          val realKey = pubKeyScript(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
          // We create a funding transaction by replacing an output with a real one in a saved dummy funding transaction
          val realOut = new TransactionOutput(app.params, null, Coin valueOf cmd.realFundingAmountSat, realKey.getProgram)
          val withReplacedDummy = cmd.dummyRequest.tx.getOutputs.asScala.patch(cmd.outIndex, List(realOut), 1)

          cmd.dummyRequest.tx.clearOutputs
          for (out <- withReplacedDummy) cmd.dummyRequest.tx addOutput out
          freshChan process CMDFunding(app.kit.sign(cmd.dummyRequest).tx)

        case (_, wait: WaitFundingDoneData, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // Preliminary negotiations are complete, we can broadcast a funding transaction
          ConnectionManager.listeners -= self
          freshChan.listeners -= self
          freshChan STORE wait

          // Error while saving will halt any further progress
          // Pressing back at this point will not affect anything
          val refund = RefundingData(wait.announce, None, wait.commitments)
          val encrypted = AES.encode(refund.toJson.toString, LNParams.cloudSecret)

          // Save a channel backup right away, don't wait until a channel becomes operational
          // in worst case it will be saved once channel becomes OPEN if there are no tokens currently
          OlympusWrap tellClouds CloudAct(encrypted, Seq("key" -> LNParams.cloudId.toString), "data/put")
          // Make this a fully established channel by attaching operational listeners and adding it to list
          freshChan.listeners = app.ChannelManager.operationalListeners
          app.ChannelManager.all +:= freshChan

          // Broadcast a funding transaction
          // Tell wallet activity to redirect to ops
          LNParams.broadcaster nullOnBecome freshChan
          app.TransData.value = FragWallet.REDIRECT
          me exitTo classOf[WalletActivity]
      }

      override def onException = {
        case _ \ errorWhileOpening =>
          // Inform user, disconnect this channel, go back
          UITask(app toast errorWhileOpening.getMessage).run
          whenBackPressed.run
      }
    }

    def askForFunding(their: Init): TimerTask = UITask {
      val maxCap = MilliSatoshi(math.min(app.kit.conf1Balance.value, 16777215L) * 1000L)
      val minCap = MilliSatoshi(math.max(LNParams.broadcaster.perKwThreeSat, 300000L) * 1000L)
      val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
      val txt = getString(amount_hint_newchan).format(denom withSign minCap, denom withSign maxCap)
      val rateManager = new RateManager(txt, content)
      val dummyKey = randomPrivKey.publicKey

      def next(msat: MilliSatoshi) = new TxProcessor {
        val dummyScript = pubKeyScript(dummyKey, dummyKey)
        val pay = P2WSHData(msat, dummyScript)

        def futureProcess(unsignedRequest: SendRequest) = {
          val finder = new PubKeyScriptIndexFinder(unsignedRequest.tx)
          val outIndex = finder.findPubKeyScriptIndex(dummyScript, None)
          val realChannelFundingAmountSat = unsignedRequest.tx.getOutput(outIndex).getValue.getValue
          val theirUnspendableReserveSat = realChannelFundingAmountSat / LNParams.theirReserveToFundingRatio
          val finalPubKeyScript: BinaryData = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
          val localParams = LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript, System.currentTimeMillis)
          freshChan process CMDOpenChannel(localParams, tempChanId = random getBytes 32, LNParams.broadcaster.perKwThreeSat,
            pushMsat = 0L, remoteInit = their, dummyRequest = unsignedRequest, outIndex, realChannelFundingAmountSat)
        }

        def onTxFail(fundingError: Throwable) = {
          val bld = baseBuilder(messageWhenMakingTx(fundingError), null)
          mkForm(askForFunding(their).run, none, bld, dialog_ok, dialog_cancel)
        }
      }

      def askAttempt(alert: AlertDialog) = rateManager.result match {
        case Success(ms) if ms < minCap => app toast dialog_sum_small
        case Success(ms) if ms > maxCap => app toast dialog_sum_big
        case Failure(reason) => app toast dialog_sum_empty
        case Success(ms) => rm(alert)(next(ms).start)
      }

      def useMax(alert: AlertDialog) = rateManager setSum Try(maxCap)
      val bld = baseBuilder(title = getString(ln_ops_start_fund_title).html, content)
      mkCheckFormNeutral(askAttempt, none, useMax, bld, dialog_next, dialog_cancel, dialog_max)
    }

    whenBackPressed = UITask {
      freshChan.listeners -= openListener
      ConnectionManager.listeners -= openListener
      // Worker may have already been automatically removed on connection failure
      try ConnectionManager.connections(announce.nodeId).disconnect catch none
      finish
    }

    // Wire up listeners and connect
    freshChan.listeners += openListener
    ConnectionManager.listeners += openListener
    ConnectionManager connectTo announce
  }
}