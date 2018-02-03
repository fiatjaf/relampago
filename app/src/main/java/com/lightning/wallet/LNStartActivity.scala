package com.lightning.wallet

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import com.lightning.wallet.lnutils.Connector.AnnounceChansNumVec
import android.content.DialogInterface.BUTTON_POSITIVE
import com.lightning.wallet.ln.Scripts.multiSig2of2
import com.lightning.wallet.ln.Tools.runAnd
import android.support.v7.widget.Toolbar
import org.bitcoinj.script.ScriptBuilder
import scala.collection.mutable
import fr.acinq.bitcoin.Script
import org.bitcoinj.core.Coin
import java.util.TimerTask
import android.os.Bundle

import com.lightning.wallet.lnutils.{CloudAct, PaymentInfoWrap, RatesSaver}
import android.widget.{BaseAdapter, Button, ListView, TextView}
import com.lightning.wallet.ln.Tools.{none, random, wrap}
import com.lightning.wallet.helper.{AES, ThrottledWork}
import android.view.{Menu, View, ViewGroup}
import scala.util.{Failure, Success}


class LNStartActivity extends TimerActivity with ViewSwitch with SearchBar { me =>
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val lnStartDetailsText = findViewById(R.id.lnStartDetailsText).asInstanceOf[TextView]
  lazy val lnCancel = findViewById(R.id.lnCancel).asInstanceOf[Button]
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[Toolbar]

  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val views = lnStartNodesList :: findViewById(R.id.lnStartDetails) :: Nil
  lazy val nodeView = getString(ln_ops_start_node_view)

  var whenBackPressed: Runnable = UITask(super.onBackPressed)
  private var nodes = Vector.empty[AnnounceChansNum]

  val adapter = new BaseAdapter {
    def getView(nodePosition: Int, cv: View, parent: ViewGroup) = {
      val view = getLayoutInflater.inflate(R.layout.frag_single_line, null)
      val textLine = view.findViewById(R.id.textLine).asInstanceOf[TextView]

      val announce \ connections = getItem(nodePosition)
      val humanConnects = app.plurOrZero(chansNumber, connections)
      val theirNode = humanNode(announce.nodeId.toString, "\u0020")

      // Display number of connections so users may pick a well connected nodes
      textLine setText nodeView.format(announce.alias, humanConnects, theirNode).html
      view
    }

    def getItem(position: Int) = nodes(position)
    def getItemId(position: Int) = position
    def getCount = nodes.size
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    new ThrottledWork[String, AnnounceChansNumVec] {
      def work(radixNodeAliasOrNodeIdQuery: String) = LNParams.cloud.connector findNodes radixNodeAliasOrNodeIdQuery
      def process(res: AnnounceChansNumVec) = wrap { UITask(adapter.notifyDataSetChanged).run } { nodes = res }
      def error(err: Throwable) = Tools errlog err
      me.react = addWork
    }

    // Set action bar, content view, title and subtitle text, wire up listeners
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_ln_start)
    wrap(toolbar setTitle ln_open_channel)(toolbar setSubtitle ln_select_peer)
    lnStartNodesList setOnItemClickListener onTap(onPeerSelected)
    lnStartNodesList setAdapter adapter
    react(new String)

    // Or back if resources are freed
  } else me exitTo classOf[MainActivity]

  override def onBackPressed = whenBackPressed.run
  override def onCreateOptionsMenu(menu: Menu) = runAnd(true) {
    // Can search nodes by their aliases and use a QR node scanner
    getMenuInflater.inflate(R.menu.ln_start, menu)
    setupSearch(menu)
  }

  private def onPeerSelected(pos: Int) = hideKeys {
    val announce \ connections = adapter getItem pos
    val theirNode = humanNode(announce.nodeId.toString, "<br>")
    val humanConnects = app.plurOrZero(chansNumber, connections)
    // This channel does not receive events yet so we need to add some custom listeners
    val detailsText = nodeView.format(announce.alias, humanConnects, s"<br>$theirNode").html
    val freshChan = app.ChannelManager.createChannel(mutable.Set.empty, InitData apply announce)

    lazy val socketOpenListener = new ConnectionListener {
      override def onMessage(ann: NodeAnnouncement, msg: LightningMessage) = if (ann == announce) freshChan process msg
      override def onOperational(ann: NodeAnnouncement, their: Init) = if (ann == announce) askForFunding(freshChan, their).run
      override def onDisconnect(ann: NodeAnnouncement) = if (ann == announce) chanOpenListener onError freshChan -> new LightningException
      override def onTerminalError(ann: NodeAnnouncement) = if (ann == announce) chanOpenListener onError freshChan -> new LightningException
    }

    lazy val chanOpenListener = new ChannelListener {
      // Updates UI accordingly to changes in fresh channel
      // should account for user cancelling at late stages

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // Peer has agreed to open a channel so now we ask user for a tx feerate
          askForFeerate(freshChan, cmd, accept).run

        case (_, wait: WaitFundingDoneData, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // Preliminary negotiations are complete, we can broadcast a transaction
          proceed(wait)
      }

      override def onError = {
        // If anything goes wrong

        case _ \ error =>
          // Disconnect this channel and inform user
          UITask(app toast error.getMessage).run
          cancelChannel.run
      }
    }

    def proceed(wait: WaitFundingDoneData): Unit = {
      // Remove all listeners specific to this activity
      ConnectionManager.listeners -= socketOpenListener
      freshChan.listeners -= chanOpenListener

      freshChan STORE wait
      // Error while saving will halt any further progress here
      // User may press cancel at this point but it won't affect anything
      val state = RefundingData(wait.announce, wait.commitments).toJson.toString
      val encrypted = AES.encode(state, LNParams.cloudSecret)

      // Save a channel backup right away, don't wait until a channel becomes operational
      // in worst case it will be saved once channel becomes OPEN if there are no tokens currently
      LNParams.cloud doProcess CloudAct(encrypted, Seq("key" -> LNParams.cloudId.toString), "data/put")
      // Make this a fully established channel by attaching operational listeners and adding it to list
      freshChan.listeners ++= app.ChannelManager.operationalListeners
      app.ChannelManager.all +:= freshChan
      me exitTo classOf[WalletActivity]
    }

    def cancelChannel: Runnable = UITask {
      freshChan.listeners -= chanOpenListener
      ConnectionManager.listeners -= socketOpenListener
      ConnectionManager.connections(announce).disconnect
      // Disconnect all local listeners and update UI
      whenBackPressed = UITask(super.onBackPressed)
      setVis(View.VISIBLE, View.GONE)
      getSupportActionBar.show
    }

    freshChan.listeners += chanOpenListener
    ConnectionManager.listeners += socketOpenListener
    ConnectionManager connectTo announce

    // Connect all local listeners and update UI accordingly
    lnCancel setOnClickListener onButtonTap(cancelChannel.run)
    lnStartDetailsText setText detailsText
    whenBackPressed = cancelChannel
    setVis(View.GONE, View.VISIBLE)
    getSupportActionBar.hide
  }

  // UI utilities

  def askForFunding(chan: Channel, their: Init) = UITask {
    val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), getString(ln_ops_start_fund_title).html, content)
    val rateManager = new RateManager(getString(amount_hint_newchan).format(denom withSign RatesSaver.rates.feeLive,
      denom withSign LNParams.maxChannelCapacity, denom withSign app.kit.conf1Balance), content)

    def askAttempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case Success(ms) if ms < RatesSaver.rates.feeLive => app toast dialog_sum_small
      case Success(ms) if ms > LNParams.maxChannelCapacity => app toast dialog_sum_big

      case Success(ms) => rm(alert) {
        val amountSat = ms.amount / sat2msatFactor
        val theirUnspendableReserveSat = (amountSat * LNParams.theirReserveToFundingRatio).toLong
        val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
        val localParams = LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript, System.currentTimeMillis)
        chan process CMDOpenChannel(localParams, random getBytes 32, LNParams.broadcaster.ratePerKwSat, 0, their, amountSat)
      }
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(askAttempt)
  }

  def askForFeerate(chan: Channel, cmd: CMDOpenChannel, acc: AcceptChannel): TimerTask = UITask {
    val multisigScript = multiSig2of2(cmd.localParams.fundingPrivKey.publicKey, acc.fundingPubkey)
    val scriptPubKey = Script.write(Script pay2wsh multisigScript)

    new TxProcessor {
      val funding = Coin valueOf cmd.fundingAmountSat
      val pay = P2WSHData(funding, scriptPubKey)
      chooseFee

      def processTx(pass: String, feePerKb: Coin) =
        <(makeTx(pass, feePerKb): fr.acinq.bitcoin.Transaction, onTxFail) { fundTx =>
          // Convert bitcoinj tx to bitcoin-lib format and find our output order number
          val outIndex = Scripts.findPubKeyScriptIndex(fundTx, scriptPubKey)
          chan.process(fundTx -> outIndex)
        }

      def onTxFail(err: Throwable) =
        mkForm(mkChoiceDialog(me delayUI askForFeerate(chan, cmd, acc),
          none, dialog_ok, dialog_cancel), messageWhenMakingTx(err), null)
    }
  }
}