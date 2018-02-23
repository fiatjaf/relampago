package com.lightning.wallet

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.ln.Channel._
import scala.collection.JavaConverters._
import com.lightning.wallet.Denomination._
import fr.acinq.bitcoin.DeterministicWallet._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import android.content.DialogInterface.BUTTON_POSITIVE
import com.lightning.wallet.lnutils.RatesSaver
import com.lightning.wallet.lnutils.CloudAct
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.script.ScriptBuilder
import com.lightning.wallet.helper.AES
import java.util.TimerTask
import android.os.Bundle

import org.bitcoinj.core.{Coin, TransactionOutput}
import fr.acinq.bitcoin.{MilliSatoshi, Script}
import android.widget.{ImageButton, TextView}
import scala.util.{Failure, Success}


class LNStartFundActivity extends TimerActivity { me =>
  var whenBackPressed: Runnable = UITask(super.onBackPressed)
  lazy val lnStartFundCancel = findViewById(R.id.lnStartFundCancel).asInstanceOf[ImageButton]
  lazy val lnStartFundDetails = findViewById(R.id.lnStartFundDetails).asInstanceOf[TextView]
  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val nodeView = getString(ln_ops_start_node_view)
  override def onBackPressed = whenBackPressed.run

  def INIT(state: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_start_fund)

    app.TransData.value match {
      case (announce: NodeAnnouncement, num: Int) =>
        // We've got this announcement from a search list
        proceed(app.plurOrZero(chansNumber, num), announce)

      case announce: NodeAnnouncement =>
        // We've got this announcement from qr
        proceed(chansNumber.last, announce)

      case _ =>
        // Nothing
        finish
    }

    app.TransData.value = null
    // Or back if resources are freed
  } else me exitTo classOf[MainActivity]

  def pubKeyScript(pub1: PublicKey, pub2: PublicKey) = {
    val multisigScript = Scripts.multiSig2of2(pub1, pub2)
    Script.write(Script pay2wsh multisigScript)
  }

  def proceed(pubChansNum: String, announce: NodeAnnouncement) = {
    val theirNodeHumanId = humanNode(announce.nodeId.toString, "<br>")
    val detailsText = nodeView.format(announce.alias, pubChansNum, s"<br>$theirNodeHumanId").html
    val freshChan = app.ChannelManager.createChannel(Set.empty, InitData apply announce)

    lazy val openListener = new ConnectionListener with ChannelListener { self =>
      override def onMessage(ann: NodeAnnouncement, msg: LightningMessage) = msg match {
        case setupMsg: ChannelSetupMessage if ann == announce => freshChan process setupMsg
        case err: Error if ann == announce => freshChan process err
        case _ =>
      }

      val peerOffline = new LightningException(me getString err_ln_peer_offline)
      override def onOperational(ann: NodeAnnouncement, their: Init) = if (ann == announce) askForFunding(their).run
      override def onTerminalError(ann: NodeAnnouncement) = if (ann == announce) onError(freshChan -> peerOffline)
      override def onDisconnect(ann: NodeAnnouncement) = if (ann == announce) onError(freshChan -> peerOffline)

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

          // Error while saving will halt any further progress here
          // Pressing back at this point but it won't affect anything
          val refund = RefundingData(wait.announce, None, wait.commitments)
          val encrypted = AES.encode(refund.toJson.toString, LNParams.cloudSecret)

          // Save a channel backup right away, don't wait until a channel becomes operational
          // in worst case it will be saved once channel becomes OPEN if there are no tokens currently
          LNParams.cloud doProcess CloudAct(encrypted, Seq("key" -> LNParams.cloudId.toString), "data/put")
          // Make this a fully established channel by attaching operational listeners and adding it to list
          freshChan.listeners = app.ChannelManager.operationalListeners
          app.ChannelManager.all +:= freshChan
          me exitTo classOf[WalletActivity]
      }

      override def onError = {
        case _ \ errorWhileOpening =>
          // If anything at all goes wrong here
          // inform user, disconnect this channel, go back
          UITask(app toast errorWhileOpening.getMessage).run
          whenBackPressed.run
      }
    }

    def askForFunding(their: Init): TimerTask = UITask {
      val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
      val dummyKey = derivePrivateKey(LNParams.extendedCloudKey, System.currentTimeMillis :: 0L :: Nil).publicKey
      val alert = mkForm(negPosBld(dialog_cancel, dialog_next), getString(ln_ops_start_fund_title).html, content)
      val rateManager = new RateManager(getString(amount_hint_newchan).format(denom withSign RatesSaver.rates.feeLive,
        denom withSign LNParams.maxChannelCapacity, denom withSign app.kit.conf1Balance), content)

      def next(fundingMsat: MilliSatoshi) = new TxProcessor {
        val dummyScriptPubKey = pubKeyScript(dummyKey, dummyKey)
        val pay = P2WSHData(fundingMsat, dummyScriptPubKey)

        def processTx(pass: String, feePerKb: Coin) =
          <(unsigned(pass, feePerKb), onTxFail) { request =>
            val outIndex = Scripts.findPubKeyScriptIndex(request.tx, dummyScriptPubKey)
            val realChannelFundingAmountSat = request.tx.getOutput(outIndex).getValue.getValue
            val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
            val theirUnspendableReserveSat = realChannelFundingAmountSat / LNParams.theirReserveToFundingRatio
            val localParams = LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript, System.currentTimeMillis)
            freshChan process CMDOpenChannel(localParams, random getBytes 32, LNParams.broadcaster.ratePerKwSat, pushMsat = 0L,
              their, request, outIndex, realChannelFundingAmountSat)
          }

        def onTxFail(fundingError: Throwable) =
          mkForm(mkChoiceDialog(me delayUI askForFunding(their), none,
            dialog_ok, dialog_cancel), messageWhenMakingTx(fundingError), null)
      }

      def askAttempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if ms < RatesSaver.rates.feeLive => app toast dialog_sum_small
        case Success(ms) if ms > LNParams.maxChannelCapacity => app toast dialog_sum_big
        case Success(ms) => rm(alert)(next(ms).chooseFee)
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(askAttempt)
    }

    whenBackPressed = UITask {
      freshChan.listeners -= openListener
      ConnectionManager.listeners -= openListener
      ConnectionManager.connections(announce).disconnect
      finish
    }

    // Wire up listeners and connect
    freshChan.listeners += openListener
    ConnectionManager.listeners += openListener
    ConnectionManager connectTo announce

    // Disconnect channel and go back once user taps a back button
    lnStartFundCancel setOnClickListener onButtonTap(whenBackPressed.run)
    lnStartFundDetails setText detailsText
  }
}