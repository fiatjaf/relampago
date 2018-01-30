package com.lightning.wallet

import android.view._
import android.widget._
import org.bitcoinj.core._
import collection.JavaConverters._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import com.lightning.wallet.ln.Tools.{none, wrap}
import scala.util.{Failure, Success}

import android.content.DialogInterface.BUTTON_POSITIVE
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import com.lightning.wallet.ln.LNParams.minDepth
import android.support.v7.widget.Toolbar
import android.support.v4.app.Fragment
import android.content.Intent
import android.os.Bundle
import android.net.Uri


class FragBTC(val host: WalletActivity) extends Fragment with ListUpdater with ToolbarFragment { me =>
  override def onCreateView(i: LayoutInflater, vg: ViewGroup, sv: Bundle) = i.inflate(R.layout.frag_view_pager_btc, vg, false)
  lazy val txsConfs = getResources getStringArray R.array.txs_confs
  lazy val mnemonicInfoText = getString(mnemonic_info)
  lazy val feeIncoming = getString(txs_fee_incoming)
  lazy val feeDetails = getString(txs_fee_details)
  lazy val feeAbsent = getString(txs_fee_absent)
  import host.{str2View, <, onFail}

  val barMenuListener = new Toolbar.OnMenuItemClickListener {
    def onMenuItemClick(m: MenuItem) = host onOptionsItemSelected m
  }

  val txTracker = new TxTracker {
    override def coinsSent(tx: Transaction) = notifyBtcEvent(me getString tx_sent)
    override def coinsReceived(tx: Transaction) = notifyBtcEvent(me getString tx_received)
  }

  val catchListener = new BlocksListener {
    def getNextTracker(initBlocksLeft: Int) = new BlocksListener { self =>
      def onBlocksDownloaded(peer: Peer, block: Block, filteredBlock: FilteredBlock, left: Int) = {
        if (left % blocksPerDay == 0) update(app.plurOrZero(syncOps, left / blocksPerDay), Informer.CHAINSYNC)
        if (left < 1) notifyFinish
        uitask.run
      }

      def notifyFinish = {
        app.kit.peerGroup removeBlocksDownloadedEventListener self
        add(me getString info_progress_done, Informer.CHAINSYNC)
        host.timer.schedule(delete(Informer.CHAINSYNC), 5000)
      }

      // We only add a SYNC item if we have a large enough
      // lag (more than a day), otherwise no updates are visible
      private val syncOps = app.getResources getStringArray R.array.info_progress
      private val text = app.plurOrZero(syncOps, initBlocksLeft / blocksPerDay)
      if (initBlocksLeft > blocksPerDay) add(text, Informer.CHAINSYNC)
    }

    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = {
      app.kit.peerGroup addBlocksDownloadedEventListener getNextTracker(left)
      app.kit.peerGroup removeBlocksDownloadedEventListener this
    }
  }

  val constListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerConnected(peer: Peer, peerCount: Int) = update(me getString status, Informer.PEER).uitask.run
    def onPeerDisconnected(peer: Peer, peerCount: Int) = update(me getString status, Informer.PEER).uitask.run
    def status = if (app.kit.peerGroup.numConnectedPeers < 1) btc_notify_connecting else btc_notify_operational
  }

  lazy val adapter = new CutAdapter[TxWrap](72, R.layout.frag_tx_btc_line) {
    // BTC line has a wider timestamp section because there is no payment info

    def getItem(position: Int) = visibleItems(position)
    def getHolder(view: View) = new TxViewHolder(view) {

      def fillView(wrap: TxWrap) = {
        val statusImage = if (wrap.tx.getConfidence.getConfidenceType == DEAD) dead
        else if (wrap.tx.getConfidence.getDepthInBlocks >= minDepth) conf1
        else await

        val marking = if (wrap.nativeValue.isPositive) sumIn else sumOut
        val finalSum = if (wrap.nativeValue.isPositive) coin2MSat(wrap.nativeValue)
        else coin2MSat(wrap.fee map wrap.nativeValue.add getOrElse wrap.nativeValue)

        transactWhen setText when(System.currentTimeMillis, wrap.tx.getUpdateTime).html
        transactSum setText marking.format(denom formatted finalSum).html
        transactCircle setImageResource statusImage
      }
    }
  }

  override def onViewCreated(view: View, savedInstanceState: Bundle) = {
    val mnemonicWarn = view.findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
    val mnemonicInfo = Utils clickableTextField view.findViewById(R.id.mnemonicInfo)
    val itemsList = view.findViewById(R.id.itemsList).asInstanceOf[ListView]

    val lstTracker = new TxTracker {
      override def txConfirmed(tx: Transaction) = host runOnUiThread {
        // Once tx is confirmed we update title and list but not subtitle
        wrap(updTitle)(adapter.notifyDataSetChanged)
      }

      override def coinsSent(tx: Transaction) = host runOnUiThread tell(tx)
      override def coinsReceived(tx: Transaction) = host runOnUiThread tell(tx)

      def tell(wrap: TxWrap) = if (!wrap.nativeValue.isZero) {
        // Zero means this tx changes nothing in our wallet and
        // thus it is watched or completely foreign

        itemsList setVisibility View.VISIBLE
        mnemonicWarn setVisibility View.GONE
        adapter.set(wrap +: adapter.availableItems)
        adapter.notifyDataSetChanged
      }
    }

    itemsList setOnItemClickListener host.onTap { pos =>
      val lst = host.getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val outside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]

      val wrap = adapter getItem pos
      val marking = if (wrap.nativeValue.isPositive) sumIn else sumOut
      val confirms = app.plurOrZero(txsConfs, wrap.tx.getConfidence.getDepthInBlocks)
      val outputs = wrap.payDatas(wrap.nativeValue.isPositive).flatMap(_.toOption)
      val humanViews = for (payData <- outputs) yield payData.cute(marking).html

      // Wire up a popup list
      lst setAdapter new ArrayAdapter(host, R.layout.frag_top_tip, R.id.actionTip, humanViews.toArray)
      lst setOnItemClickListener host.onTap { position => outputs(position - 1).onClick }
      lst setHeaderDividersEnabled false
      lst addHeaderView detailsWrapper

      outside setOnClickListener host.onButtonTap {
        val smartbit = "https://testnet.smartbit.com.au/tx/"
        val uri = Uri.parse(smartbit + wrap.tx.getHashAsString)
        me startActivity new Intent(Intent.ACTION_VIEW, uri)
      }

      wrap.fee match {
        case _ if wrap.tx.getConfidence.getConfidenceType == DEAD =>
          host.mkForm(host negBld dialog_ok, sumOut.format(txsConfs.last).html, lst)

        case _ if wrap.nativeValue.isPositive =>
          val details = feeIncoming.format(confirms)
          host.mkForm(host negBld dialog_ok, details.html, lst)

        case Some(fee) =>
          val details = feeDetails.format(marking.format(denom withSign fee), confirms)
          host.mkForm(host negBld dialog_ok, humanFiat(details, fee).html, lst)

        case None =>
          val details = feeAbsent.format(confirms).html
          host.mkForm(host negBld dialog_ok, details, lst)
      }
    }

    toggler setOnClickListener host.onButtonTap {
      // Expand and collapse available BTC transactions

      adapter.switch
      adapter set adapter.availableItems
      adapter.notifyDataSetChanged
    }

    itemsList setAdapter adapter
    itemsList setFooterDividersEnabled false
    startListUpdates(itemsList, adapter)

    me.toolbar = view.findViewById(R.id.toolbar).asInstanceOf[Toolbar]
    me.toolbar setOnClickListener host.onButtonTap(updDenom)
    me.toolbar setOnMenuItemClickListener barMenuListener
    me.toolbar.inflateMenu(R.menu.transactions_ops)

    app.kit.wallet addCoinsSentEventListener txTracker
    app.kit.wallet addCoinsReceivedEventListener txTracker
    app.kit.peerGroup addConnectedEventListener constListener
    app.kit.peerGroup addDisconnectedEventListener constListener
    app.kit.peerGroup addBlocksDownloadedEventListener catchListener

    // Wait for transactions list
    <(nativeTransactions, onFail) { txs =>
      app.kit.wallet addCoinsSentEventListener lstTracker
      app.kit.wallet addCoinsReceivedEventListener lstTracker
      app.kit.wallet addTransactionConfidenceEventListener lstTracker
      adapter set txs

      if (txs.nonEmpty) itemsList setVisibility View.VISIBLE
      if (txs.isEmpty) mnemonicWarn setVisibility View.VISIBLE
      if (txs.isEmpty) mnemonicInfo setText mnemonicInfoText.html
    }

    // Manually update title and subtitle after we have a toolbar
    add(me getString constListener.status, Informer.PEER).uitask.run
    updTitle
  }

  def updDenom = host showDenomChooser { pos =>
    wrap(adapter.notifyDataSetChanged) { denom = denoms apply pos }
    app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
    updTitle
  }

  def updTitle = setTitle {
    val conf0 = denom.withSign(app.kit.conf1Balance)
    val gap = app.kit.conf0Balance minus app.kit.conf1Balance
    if (gap.isZero) conf0 else s"$conf0 + ${denom formatted gap}"
  }

  def notifyBtcEvent(message: String) = {
    add(message, Informer.BTCEVENT).uitask.run
    host.timer.schedule(delete(Informer.BTCEVENT), 8000)
    host runOnUiThread updTitle
  }

  def nativeTransactions = {
    val raw = app.kit.wallet.getRecentTransactions(adapter.max, false)
    raw.asScala.toVector.map(bitcoinjTx2Wrap).filterNot(_.nativeValue.isZero)
  }

  def sendBtcPopup: BtcManager = {
    val content = host.getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val alert = host.mkForm(host.negPosBld(dialog_cancel, dialog_next), me getString action_bitcoin_send, content)
    val rateManager = new RateManager(getString(amount_hint_can_send).format(denom withSign app.kit.conf1Balance), content)
    val spendManager = new BtcManager(rateManager)

    def sendAttempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case _ if spendManager.getAddress == null => app toast dialog_address_wrong
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_small

      case ok @ Success(ms) =>
        val processor = new host.TxProcessor {
          val pay = AddrData(ms, spendManager.getAddress)
          override def processTx(pass: String, feePerKb: Coin) = {
            add(me getString tx_announcing, Informer.BTCEVENT).uitask.run
            <(app.kit blockingSend makeTx(pass, feePerKb), onTxFail)(none)
          }

          override def onTxFail(err: Throwable) =
            host.mkForm(host.mkChoiceDialog(host delayUI sendBtcPopup.set(ok, pay.address),
              none, dialog_ok, dialog_cancel), messageWhenMakingTx(err), null)
        }

        // Initiate the spending sequence
        host.rm(alert)(processor.chooseFee)
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener host.onButtonTap(sendAttempt)
    spendManager
  }
}