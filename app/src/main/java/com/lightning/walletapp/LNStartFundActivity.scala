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
import fr.acinq.bitcoin.DeterministicWallet._
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

import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Script}
import org.bitcoinj.core.{Coin, TransactionOutput}
import android.widget.{ImageButton, TextView}
import scala.util.{Failure, Success, Try}


class LNStartFundActivity extends TimerActivity { me =>
  var whenBackPressed: Runnable = UITask(super.onBackPressed)
  lazy val lnStartFundCancel = findViewById(R.id.lnStartFundCancel).asInstanceOf[ImageButton]
  lazy val lnStartFundDetails = findViewById(R.id.lnStartFundDetails).asInstanceOf[TextView]
  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val nodeView = getString(ln_ops_start_fund_node_view)
  override def onBackPressed = whenBackPressed.run

  def INIT(state: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_start_fund)

    app.TransData.value match {
      // We may get peer info either from built-in list of from user scanned qr code
      case RemoteNodeView(ann \ num) => proceed(app.plurOrZero(chansNumber, num), ann)
      case HardcodedNodeView(ann, hardcodedTip) => proceed(hardcodedTip, ann)
      case ann: NodeAnnouncement => proceed(chansNumber.last, ann)
      case _ => finish
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
    val detailsText = nodeView.format(announce.alias, pubChansNum, theirNodeHumanId).html
    val freshChan = app.ChannelManager.createChannel(Set.empty, InitData apply announce)

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
      // Current feerate may be higher than hard capacity so choose the currently largest value
      val minCapacity = MilliSatoshi(math.max(LNParams.broadcaster.perKwThreeSat, 250000L) * 1000L)
      val currentOnChainBalance: MilliSatoshi = app.kit.conf1Balance

      val canSend = denom withSign currentOnChainBalance
      val maxHuman = denom withSign LNParams.maxChanCapacity
      val minHuman = denom withSign minCapacity

      val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
      val rateManager = new RateManager(getString(amount_hint_newchan).format(minHuman, maxHuman, canSend), content)
      val dummyKey = derivePrivateKey(LNParams.extendedCloudKey, System.currentTimeMillis :: 0L :: Nil).publicKey

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
        case Success(ms) if ms > LNParams.maxChanCapacity => app toast dialog_sum_big
        case Success(ms) if ms < minCapacity => app toast dialog_sum_small
        case Failure(reason) => app toast dialog_sum_empty
        case Success(ms) => rm(alert)(next(ms).start)
      }

      // When balance is less than or equals max chan capacity we fill it in thus giving a hint that all can be sent into channel
      mkCheckForm(askAttempt, none, baseBuilder(getString(ln_ops_start_fund_title).html, content), dialog_next, dialog_cancel)
      if (currentOnChainBalance <= LNParams.maxChanCapacity) rateManager setSum Try(currentOnChainBalance)
    }

    whenBackPressed = UITask {
      freshChan.listeners -= openListener
      ConnectionManager.listeners -= openListener
      ConnectionManager.connections(announce.nodeId).disconnect
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