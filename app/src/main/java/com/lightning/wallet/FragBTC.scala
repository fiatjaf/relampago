package com.lightning.wallet

import android.view._
import android.widget._
import org.bitcoinj.core._
import org.bitcoinj.core.TxWrap._
import collection.JavaConverters._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import com.lightning.wallet.ln.Tools.{none, wrap, runAnd}
import scala.util.{Failure, Success}

import android.content.DialogInterface.BUTTON_POSITIVE
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import android.support.v7.widget.Toolbar.OnMenuItemClickListener
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import org.bitcoinj.wallet.SendRequest.childPaysForParent
import com.lightning.wallet.ln.LNParams.minDepth
import com.lightning.wallet.lnutils.RatesSaver
import android.support.v7.widget.Toolbar
import android.app.AlertDialog.Builder
import android.support.v4.app.Fragment
import fr.acinq.bitcoin.MilliSatoshi
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
    super.onResume
  }

  override def onDestroy = {
    WalletActivity.frags -= me
    // This may be nullified hence a null check
    if (worker != null) worker.onFragmentDestroy
    super.onDestroy
  }
}

class FragBTCWorker(val host: WalletActivity, frag: View) extends ListToggler with ToolbarFragment { me =>
  val barMenuListener = new OnMenuItemClickListener { def onMenuItemClick(m: MenuItem) = host onOptionsItemSelected m }
  import host.{getResources, getString, str2View, rm, mkForm, UITask, getLayoutInflater, negPosBld, TxProcessor}
  import host.{timer, <, mkChoiceDialog, onButtonTap, onFastTap, onTap, onFail, showDenomChooser}

  val toolbar = frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  val itemsList = frag.findViewById(R.id.itemsList).asInstanceOf[ListView]
  val mnemonicWarn = frag.findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
  val txsConfs = getResources getStringArray R.array.txs_confs

  val adapter = new CutAdapter[TxWrap](24, R.layout.frag_tx_btc_line) {
    // BTC line has a wider timestamp section because there is no payment info
    // amount of history is low here because displaying each tx is costly

    def getItem(position: Int) = visibleItems(position)
    def getHolder(view: View) = new TxViewHolder(view) {

      def fillView(wrap: TxWrap) = {
        val timestamp = when(System.currentTimeMillis, wrap.tx.getUpdateTime)
        val status = if (wrap.isDead) dead else if (wrap.depth >= minDepth) conf1 else await

        val markedPaymentSum = wrap.visibleValue.isPositive match {
          case true => sumIn.format(denom formatted wrap.visibleValue)
          case false => sumOut.format(denom formatted wrap.visibleValue)
        }

        transactCircle setImageResource status
        transactSum setText markedPaymentSum.html
        transactWhen setText timestamp.html
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
    def status = if (app.kit.peerGroup.numConnectedPeers < 1) btc_status_connecting else btc_status_online
  }

  val itemsListListener = new TxTracker {
    def whenConfirmed = wrap(updTitle)(adapter.notifyDataSetChanged)
    override def txConfirmed(tx: Transaction) = UITask(whenConfirmed).run
    override def coinsReceived(tx: Transaction) = guard(tx)
    override def coinsSent(tx: Transaction) = guard(tx)

    def guard(wrap: TxWrap): Unit = {
      if (wrap.valueDelta.isZero) return
      updateItems(wrap).run
    }

    def updateItems(wrap: TxWrap) = UITask {
      val updated = wrap +: adapter.availableItems
      // Make sure we don't have hidden transactions
      // we can get one if user applies CPFP boosting
      adapter.set(updated filterNot hiddenWrap)
      adapter.notifyDataSetChanged
      updView(showText = false)
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
    val pending = conf0 minus conf1

    if (pending.isPositive) s"${denom withSign conf1} + ${denom formatted pending}"
    else if (conf0.isZero && conf1.isZero) host getString btc_wallet
    else denom withSign conf1
  }

  def notifyBtcEvent(message: String) = {
    timer.schedule(delete(Informer.BTCEVENT), 8000)
    add(message, Informer.BTCEVENT).run
    UITask(updTitle).run
  }

  def nativeTransactions = {
    val raw = app.kit.wallet.getRecentTransactions(adapter.max, false).asScala
    raw.toVector map bitcoinjTx2Wrap filterNot hiddenWrap filterNot watchedWrap
  }

  def sendBtcPopup: BtcManager = {
    val content = getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), host getString action_bitcoin_send, content)
    val rateManager = new RateManager(getString(amount_hint_can_send).format(denom withSign app.kit.conf1Balance), content)
    val spendManager = new BtcManager(rateManager)

    def next(ms: MilliSatoshi) = new TxProcessor {
      val pay = AddrData(ms, spendManager.getAddress)

      def processTx(pass: String, feePerKb: Coin) = {
        add(host getString btc_announcing, Informer.BTCEVENT).run
        <(app.kit blockingSend unsigned(pass, feePerKb), onTxFail)(none)
      }

      def onTxFail(generationError: Throwable) =
        mkForm(mkChoiceDialog(host delayUI sendBtcPopup.set(Success(ms), pay.address),
          none, dialog_ok, dialog_cancel), messageWhenMakingTx(generationError), null)
    }

    def sendAttempt = rateManager.result match {
      case Failure(why) => app toast dialog_sum_empty
      case _ if spendManager.getAddress == null => app toast dialog_address_wrong
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_small
      case Success(ms) => rm(alert)(next(ms).chooseFee)
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(sendAttempt)
    host.walletPager.setCurrentItem(0, false)
    app.TransData.value = null
    spendManager
  }

  def boostIncoming(wrap: TxWrap) = {
    val newFee = RatesSaver.rates.feeLive divide 2
    val boost = coloredIn(wrap.valueDelta minus newFee)
    val current = coloredIn(wrap.valueDelta)

    val warning = getString(boost_details).format(current, boost)
    host.passWrap(warning.html) apply host.checkPass { pass =>
      // Try to send a CPFP tx and hide an old one
      <(doSend(pass), onError)(none)
    }

    def onError(err: Throwable) = {
      // Place an old tx back to list
      // and inform user about an error
      wrap.tx setMemo null
      onFail(err)
    }

    def doSend(pass: String) = runAnd(wrap.tx setMemo HIDE) {
      val request = childPaysForParent(app.kit.wallet, wrap.tx, newFee)
      request.aesKey = app.kit.wallet.getKeyCrypter deriveKey pass

      // Check once again if tx should be re-sent before proceeding
      if (wrap.depth < 1 && !wrap.isDead) app.kit blockingSend request
      else throw new Exception(host getString err_general)
    }
  }

  // INIT

  itemsList setOnItemClickListener onTap { pos =>
    val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
    val outside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]

    val wrap = adapter getItem pos
    val confs = app.plurOrZero(txsConfs, wrap.depth)
    val marking = if (wrap.visibleValue.isPositive) sumIn else sumOut
    val outputs = wrap.payDatas(wrap.visibleValue.isPositive).flatMap(_.toOption)
    val humanOutputs = for (paymentData <- outputs) yield paymentData.cute(marking).html
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    lst setAdapter new ArrayAdapter(host, R.layout.frag_top_tip, R.id.actionTip, humanOutputs.toArray)
    lst setOnItemClickListener onTap { pos1 => outputs(pos1 - 1).onClick /* -1 because header */ }
    lst setHeaderDividersEnabled false
    lst addHeaderView detailsWrapper

    outside setOnClickListener onButtonTap {
      val smartbit = "https://testnet.smartbit.com.au/tx/"
      val uri = Uri.parse(smartbit + wrap.tx.getHashAsString)
      host startActivity new Intent(Intent.ACTION_VIEW, uri)
    }

    val header = wrap.fee match {
      case _ if wrap.isDead => sumOut format txsConfs.last
      case _ if wrap.visibleValue.isPositive => getString(txs_fee_incoming) format confs
      case Some(fee) => humanFiat(getString(txs_fee_details).format(coloredOut(fee), confs), fee)
      case None => getString(txs_fee_absent) format confs
    }

    // See if CPFP can be applied
    val hasEnoughValue = wrap.valueDelta isGreaterThan RatesSaver.rates.feeLive
    val isStale = wrap.tx.getUpdateTime.getTime < System.currentTimeMillis - 60000
    val canBoost = wrap.depth < 1 && !wrap.isDead && isStale && hasEnoughValue

    lazy val dlg: Builder = mkChoiceDialog(none, rm(alert)(me boostIncoming wrap), dialog_ok, dialog_boost)
    lazy val alert = if (canBoost) mkForm(dlg, header.html, lst) else mkForm(host negBld dialog_ok, header.html, lst)
    alert
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
  app.kit.peerGroup addConnectedEventListener constListener
  app.kit.peerGroup addDisconnectedEventListener constListener
  app.kit.peerGroup addBlocksDownloadedEventListener catchListener

  app.kit.wallet addCoinsSentEventListener itemsListListener
  app.kit.wallet addCoinsReceivedEventListener itemsListListener
  app.kit.wallet addTransactionConfidenceEventListener itemsListListener
  app.kit.wallet addCoinsReceivedEventListener subtitleListener
  app.kit.wallet addCoinsSentEventListener subtitleListener

  <(nativeTransactions, onFail) { txs =>
    // Fill list with bitcoin transactions
    updView(showText = txs.isEmpty)
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