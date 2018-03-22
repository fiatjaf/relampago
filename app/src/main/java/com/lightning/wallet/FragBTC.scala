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
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import scala.util.{Failure, Success}

import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import android.support.v7.widget.Toolbar.OnMenuItemClickListener
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import org.bitcoinj.wallet.SendRequest.childPaysForParent
import com.lightning.wallet.ln.LNParams.minDepth
import com.lightning.wallet.lnutils.RatesSaver
import android.support.v7.widget.Toolbar
import com.lightning.wallet.ln.LNParams
import android.support.v4.app.Fragment
import org.bitcoinj.wallet.SendRequest
import fr.acinq.bitcoin.MilliSatoshi
import android.app.AlertDialog
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
  import host.{timer, <, mkCheckForm, baseBuilder, onButtonTap, onFastTap, onTap, onFail, showDenomChooser, passWrap, checkPass}
  import host.{getResources, showForm, negBuilder, getString, str2View, rm, mkForm, UITask, getLayoutInflater, TxProcessor}

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

  def peerStatus = {
    val numPeers = app.kit.peerGroup.numConnectedPeers
    // Additional measure in case if ChannelManager listener fails
    if (numPeers > 3) LNParams.broadcaster.bestHeightObtained = true
    if (numPeers > 0) btc_status_online else btc_status_connecting
  }

  val constListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerConnected(peer: Peer, peerCount: Int) = update(host getString peerStatus, Informer.PEER).run
    def onPeerDisconnected(peer: Peer, peerCount: Int) = update(host getString peerStatus, Informer.PEER).run
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
    mnemonicWarn setVisibility viewMap(showText)
    itemsList setVisibility viewMap(!showText)
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
    val zeroConf = conf0 minus conf1

    if (zeroConf.isPositive) s"${denom withSign conf1} + ${denom formatted zeroConf}"
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
    val title = getString(amount_hint_can_send).format(denom withSign app.kit.conf1Balance)
    val rateManager = new RateManager(title, content)
    val spendManager = new BtcManager(rateManager)

    def next(msat: MilliSatoshi) = new TxProcessor {
      val pay = AddrData(msat, spendManager.getAddress)
      def futureProcess(unsignedRequest: SendRequest) = {
        add(getString(btc_announcing), Informer.BTCEVENT).run
        app.kit blockingSend app.kit.sign(unsignedRequest).tx
      }

      def onTxFail(sendingError: Throwable) = mkForm(sendBtcPopup.set(Success(msat), pay.address), none,
        baseBuilder(messageWhenMakingTx(sendingError), body = null), dialog_ok, dialog_cancel)
    }

    def sendAttempt(alert: AlertDialog) = rateManager.result match {
      case _ if spendManager.getAddress == null => app toast dialog_address_wrong
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_small
      case Failure(reason) => app toast dialog_sum_empty
      case Success(ms) => rm(alert)(next(ms).start)
    }

    val bld = baseBuilder(getString(action_bitcoin_send), content)
    mkCheckForm(sendAttempt, none, bld, dialog_next, dialog_cancel)
    spendManager
  }

  def boostIncoming(wrap: TxWrap) = {
    val current = coloredIn(wrap.valueDelta)
    val increasedFee = RatesSaver.rates.feeLive divide 2
    val boost = coloredIn(wrap.valueDelta minus increasedFee)
    val userWarn = getString(boost_details).format(current, boost).html

    // Transaction hiding must always happen before replacement sending
    lazy val unsignedBoost = childPaysForParent(app.kit.wallet, wrap.tx, increasedFee)
    def doReplace = runAnd(wrap.tx setMemo HIDE)(app.kit blockingSend app.kit.sign(unsignedBoost).tx)
    def replace = if (wrap.depth < 1 && !wrap.isDead) doReplace

    def encReplace(pass: String) = {
      val crypter = app.kit.wallet.getKeyCrypter
      unsignedBoost.aesKey = crypter deriveKey pass
      replace
    }

    def onError(err: Throwable) = {
      // Make an old tx visible again
      wrap.tx setMemo null
      onFail(err)
    }

    def encReplaceFuture(pass: String) = <(encReplace(pass), onError)(none)
    if (app.kit.wallet.isEncrypted) passWrap(userWarn) apply checkPass(encReplaceFuture)
    else mkForm(ok = <(replace, onError)(none), none, baseBuilder(userWarn, null), dialog_next, dialog_cancel)
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
    val notEnoughValue = wrap.valueDelta isLessThan RatesSaver.rates.feeLive
    val tooFresh = wrap.tx.getUpdateTime.getTime > System.currentTimeMillis - 3600L * 1000
    val doNotOfferCPFP = wrap.depth > 0 || wrap.isDead || tooFresh || notEnoughValue

    if (doNotOfferCPFP) showForm(negBuilder(dialog_ok, header.html, lst).create)
    else mkForm(none, boostIncoming(wrap), baseBuilder(header.html, lst), dialog_ok, dialog_boost)
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

  add(host getString peerStatus, Informer.PEER).run
  Utils clickableTextField frag.findViewById(R.id.mnemonicInfo)
  toolbar setOnClickListener onFastTap(showDenomChooser)
  toolbar setOnMenuItemClickListener barMenuListener
  toolbar inflateMenu R.menu.btc
  updTitle
}