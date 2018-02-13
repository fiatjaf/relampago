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
import android.support.v7.widget.Toolbar.OnMenuItemClickListener
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import com.lightning.wallet.ln.LNParams.minDepth
import android.support.v7.widget.Toolbar
import android.support.v4.app.Fragment
import android.content.Intent
import android.os.Bundle
import android.net.Uri


class FragBTC extends Fragment { me =>
  override def onCreateView(inflator: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inflator.inflate(R.layout.frag_view_pager_btc, vg, false)

  var worker: FragBTCWorker = _
  override def onViewCreated(view: View, savedInstanceState: Bundle) =
    worker = new FragBTCWorker(getActivity.asInstanceOf[WalletActivity], view)

  override def onResume = {
    WalletActivity.frags += me
    worker.onFragmentResume
    super.onResume
  }

  override def onDestroy = {
    WalletActivity.frags -= me
    worker.onFragmentDestroy
    super.onDestroy
  }
}

class FragBTCWorker(val host: WalletActivity, frag: View) extends ListToggler with ToolbarFragment {
  val barMenuListener = new OnMenuItemClickListener { def onMenuItemClick(m: MenuItem) = host onOptionsItemSelected m }
  import host.{getResources, getString, str2View, rm, mkForm, UITask, getLayoutInflater, negPosBld, TxProcessor}
  import host.{timer, <, mkChoiceDialog, onButtonTap, onFastTap, onTap, onFail, showDenomChooser}

  val toolbar = frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  val itemsList = frag.findViewById(R.id.itemsList).asInstanceOf[ListView]
  val mnemonicWarn = frag.findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
  val txsConfs = getResources getStringArray R.array.txs_confs
  val feeIncoming = getString(txs_fee_incoming)
  val feeDetails = getString(txs_fee_details)
  val feeAbsent = getString(txs_fee_absent)

  val adapter = new CutAdapter[TxWrap](24, R.layout.frag_tx_btc_line) {
    // BTC line has a wider timestamp section because there is no payment info
    // amount of history is low here because displaying each tx is costly

    def getItem(position: Int) = visibleItems(position)
    def getHolder(view: View) = new TxViewHolder(view) {

      def fillView(wrap: TxWrap) = {
        if (wrap.nativeValue.isNegative) {
          val amountWithoutFee = wrap.fee map wrap.nativeValue.add getOrElse wrap.nativeValue
          transactSum setText sumOut.format(denom formatted amountWithoutFee.negate).html
        } else transactSum setText sumIn.format(denom formatted wrap.nativeValue).html

        val statusImage =
          if (wrap.tx.getConfidence.getConfidenceType == DEAD) dead
          else if (wrap.tx.getConfidence.getDepthInBlocks >= minDepth) conf1
          else await

        transactWhen setText when(System.currentTimeMillis, wrap.tx.getUpdateTime).html
        transactCircle setImageResource statusImage
      }
    }
  }

  val catchListener = new BlocksListener {
    def getNextTracker(initBlocksLeft: Int) = new BlocksListener { self =>
      def onBlocksDownloaded(peer: Peer, block: Block, fb: FilteredBlock, left: Int) =
        if (left < 1) notifyFinish else if (left % blocksPerDay == 0) notifyDaysLeft(left)

      def notifyDaysLeft(blocksLeft: Int) = {
        val daysLeft = blocksLeft / blocksPerDay
        val text = app.plurOrZero(syncOps, daysLeft)
        update(text, Informer.CHAINSYNC).run
      }

      def notifyFinish = {
        app.kit.peerGroup removeBlocksDownloadedEventListener self
        add(host getString btc_progress_done, Informer.CHAINSYNC).run
        timer.schedule(delete(Informer.CHAINSYNC), 5000)
      }

      // We only add a SYNC item if we have a large enough
      // lag (more than a day), otherwise no updates are visible
      private val syncOps = app.getResources getStringArray R.array.info_progress
      private val text = app.plurOrZero(syncOps, initBlocksLeft / blocksPerDay)
      if (initBlocksLeft > blocksPerDay) add(text, Informer.CHAINSYNC).run
    }

    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = {
      app.kit.peerGroup addBlocksDownloadedEventListener getNextTracker(left)
      app.kit.peerGroup removeBlocksDownloadedEventListener this
    }
  }

  val constListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerConnected(peer: Peer, peerCount: Int) = update(host getString status, Informer.PEER).run
    def onPeerDisconnected(peer: Peer, peerCount: Int) = update(host getString status, Informer.PEER).run
    def status = if (app.kit.peerGroup.numConnectedPeers < 1) notify_connecting else btc_status_online
  }

  val itemsListListener = new TxTracker { self =>
    def whenConfirmed = wrap(updTitle)(adapter.notifyDataSetChanged)
    override def txConfirmed(tx: Transaction) = UITask(whenConfirmed).run
    override def coinsReceived(tx: Transaction) = UITask(tell apply tx).run
    override def coinsSent(tx: Transaction) = UITask(tell apply tx).run

    val tell = (wrap: TxWrap) =>
      if (!wrap.nativeValue.isZero) {
        // Zero means it changes nothing in wallet
        adapter.set(wrap +: adapter.availableItems)
        adapter.notifyDataSetChanged
        updView(false)
      }
  }

  val subtitleListener = new TxTracker {
    override def coinsSent(tx: Transaction) = notifyBtcEvent(host getString btc_sent)
    override def coinsReceived(tx: Transaction) = notifyBtcEvent(host getString btc_received)
  }

  def updView(showText: Boolean) = {
    val textVisibility = if (showText) View.VISIBLE else View.GONE
    val listVisibility = if (showText) View.GONE else View.VISIBLE
    mnemonicWarn setVisibility textVisibility
    itemsList setVisibility listVisibility
  }

  def onFragmentResume = {
    app.kit.peerGroup addConnectedEventListener constListener
    app.kit.peerGroup addDisconnectedEventListener constListener
    app.kit.peerGroup addBlocksDownloadedEventListener catchListener

    app.kit.wallet addCoinsSentEventListener itemsListListener
    app.kit.wallet addCoinsReceivedEventListener itemsListListener
    app.kit.wallet addTransactionConfidenceEventListener itemsListListener
    app.kit.wallet addCoinsReceivedEventListener subtitleListener
    app.kit.wallet addCoinsSentEventListener subtitleListener
  }

  def onFragmentDestroy = {
    app.kit.peerGroup removeConnectedEventListener constListener
    app.kit.peerGroup removeDisconnectedEventListener constListener
    app.kit.peerGroup removeBlocksDownloadedEventListener catchListener

    app.kit.wallet removeCoinsSentEventListener itemsListListener
    app.kit.wallet removeCoinsReceivedEventListener itemsListListener
    app.kit.wallet removeTransactionConfidenceEventListener itemsListListener
    app.kit.wallet removeCoinsReceivedEventListener subtitleListener
    app.kit.wallet removeCoinsSentEventListener subtitleListener
  }

  def updTitle = setTitle {
    val conf0 = app.kit.conf0Balance
    val conf1 = app.kit.conf1Balance
    val gap = conf0 minus conf1

    if (gap.isPositive) s"${denom withSign conf1} + ${denom formatted gap}"
    else if (conf0.isZero && conf1.isZero) host getString btc_wallet
    else denom withSign conf1
  }

  def notifyBtcEvent(message: String) = {
    timer.schedule(delete(Informer.BTCEVENT), 8000)
    add(message, Informer.BTCEVENT).run
    UITask(updTitle).run
  }

  def nativeTransactions = {
    val raw = app.kit.wallet.getRecentTransactions(adapter.max, false)
    raw.asScala.toVector.map(bitcoinjTx2Wrap).filterNot(_.nativeValue.isZero)
  }

  def sendBtcPopup: BtcManager = {
    val content = getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), host getString action_bitcoin_send, content)
    val rateManager = new RateManager(getString(amount_hint_can_send).format(denom withSign app.kit.conf1Balance), content)
    val spendManager = new BtcManager(rateManager)
    host.walletPager.setCurrentItem(0, false)

    def sendAttempt = rateManager.result match {
      case Failure(why) => app toast dialog_sum_empty
      case _ if spendManager.getAddress == null => app toast dialog_address_wrong
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_small

      case ok @ Success(ms) =>
        val processor = new TxProcessor {
          val pay = AddrData(ms, spendManager.getAddress)

          override def processTx(passcode: String, feePerKb: Coin) = {
            <(app.kit blockingSend makeTx(passcode, feePerKb), onTxFail)(none)
            add(getString(btc_announcing), Informer.BTCEVENT).run
          }

          override def onTxFail(err: Throwable) =
            mkForm(mkChoiceDialog(host delayUI sendBtcPopup.set(ok, pay.address),
              none, dialog_ok, dialog_cancel), messageWhenMakingTx(err), null)
        }

        // Initiate the spending sequence
        rm(alert)(processor.chooseFee)
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(sendAttempt)
    spendManager
  }

  // INIT

  itemsList setOnItemClickListener onTap { pos =>
    val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
    val outside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]

    val wrap = adapter getItem pos
    val marking = if (wrap.nativeValue.isPositive) sumIn else sumOut
    val confirms = app.plurOrZero(txsConfs, wrap.tx.getConfidence.getDepthInBlocks)
    val outputs = wrap.payDatas(wrap.nativeValue.isPositive).flatMap(_.toOption)
    val humanViews = for (payData <- outputs) yield payData.cute(marking).html

    // Wire up a popup list
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    lst setAdapter new ArrayAdapter(host, R.layout.frag_top_tip, R.id.actionTip, humanViews.toArray)
    lst setOnItemClickListener onTap { position => outputs(position - 1).onClick }
    lst setHeaderDividersEnabled false
    lst addHeaderView detailsWrapper

    outside setOnClickListener onButtonTap {
      val smartbit = "https://testnet.smartbit.com.au/tx/"
      val uri = Uri.parse(smartbit + wrap.tx.getHashAsString)
      host startActivity new Intent(Intent.ACTION_VIEW, uri)
    }

    wrap.fee match {
      case _ if wrap.tx.getConfidence.getConfidenceType == DEAD =>
        mkForm(host negBld dialog_ok, sumOut.format(txsConfs.last).html, lst)

      case _ if wrap.nativeValue.isPositive =>
        val details = feeIncoming.format(confirms)
        mkForm(host negBld dialog_ok, details.html, lst)

      case Some(fee) =>
        val details = feeDetails.format(marking.format(denom withSign fee), confirms)
        mkForm(host negBld dialog_ok, humanFiat(details, fee).html, lst)

      case None =>
        val details = feeAbsent.format(confirms).html
        mkForm(host negBld dialog_ok, details, lst)
    }
  }

  toggler setOnClickListener onFastTap {
    // Expand and collapse BTC transactions

    adapter.switch
    adapter set adapter.availableItems
    adapter.notifyDataSetChanged
  }

  itemsList setAdapter adapter
  itemsList addFooterView allTxsWrapper
  itemsList setFooterDividersEnabled false
  itemsList setOnScrollListener host.listListener

  <(nativeTransactions, onFail) { txs =>
    // Fill list with bitcoin transactions
    // and update views accordingly
    updView(txs.isEmpty)
    adapter set txs
  }

  // Manually update title and subtitle once toolbar is there
  Utils clickableTextField frag.findViewById(R.id.mnemonicInfo)
  add(host getString constListener.status, Informer.PEER).run
  toolbar setOnClickListener onFastTap(showDenomChooser)
  toolbar setOnMenuItemClickListener barMenuListener
  toolbar inflateMenu R.menu.btc
  updTitle
}