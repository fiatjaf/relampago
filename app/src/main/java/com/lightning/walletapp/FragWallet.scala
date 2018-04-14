package com.lightning.walletapp

import spray.json._
import android.view._
import android.widget._
import org.bitcoinj.core._
import org.bitcoinj.core.TxWrap._

import collection.JavaConverters._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.R.drawable.{await, conf1, dead}
import com.lightning.walletapp.ln.Tools.{none, random, runAnd}

import scala.util.{Failure, Success}
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import org.bitcoinj.wallet.SendRequest.childPaysForParent
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.lnutils.{PaymentTable, RatesSaver}
import com.lightning.walletapp.ln.{Channel, Hop, PaymentRequest}
import android.support.v7.widget.Toolbar
import android.support.v4.app.Fragment
import org.bitcoinj.wallet.SendRequest
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi}
import android.app.AlertDialog
import android.content.Intent
import android.os.Bundle
import android.net.Uri
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute


class FragWallet extends Fragment { me =>
  override def onCreateView(inflator: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inflator.inflate(R.layout.frag_view_pager_btc, vg, false)

  var worker: FragWalletWorker = _
  override def onViewCreated(view: View, savedInstanceState: Bundle) =
    worker = new FragWalletWorker(getActivity.asInstanceOf[WalletActivity], view)

  override def onResume = {
    // Parent now has a reference to me
    WalletActivity.walletFrag = Some(me)
    super.onResume
  }

  override def onDestroy = {
    WalletActivity.walletFrag = None
    // This may be nullified hence a null check
    if (worker != null) worker.onFragmentDestroy
    super.onDestroy
  }
}

class FragWalletWorker(val host: WalletActivity, frag: View) extends ListToggler { me =>
  val mnemonicWarn = frag.findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
  val customTitle = frag.findViewById(R.id.customTitle).asInstanceOf[TextView]
  val itemsList = frag.findViewById(R.id.itemsList).asInstanceOf[ListView]
  val toolbar = frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]

  val syncOps = app.getResources getStringArray R.array.info_progress
  val txsConfs = app.getResources getStringArray R.array.txs_confs
  val statusConnecting = app getString btc_status_connecting
  val statusOnline = app getString btc_status_online
  val btcEmpty = app getString btc_empty
  val lnEmpty = app getString ln_empty

  var currentBlocksLeft = 0
  var currentPeerCount = 0

  import host._

  // UPDATING TITLE

  val catchListener = new BlocksListener {
    def onBlocksDownloaded(sourcePeerNode: Peer, block: Block, filteredBlock: FilteredBlock, left: Int) = {
      if (left > broadcaster.blocksPerDay) app.kit.peerGroup addBlocksDownloadedEventListener getNextTracker(left)
      app.kit.peerGroup removeBlocksDownloadedEventListener this
    }

    def getNextTracker(initBlocksLeft: Int) = new BlocksListener { self =>
      def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) =
        runAnd(currentBlocksLeft = left)(updTitle)
    }
  }

  val constListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(peer: Peer, peerCount: Int) = runAnd(currentPeerCount = peerCount)(updTitle)
    def onPeerConnected(peer: Peer, peerCount: Int) = runAnd(currentPeerCount = peerCount)(updTitle)
  }

  def updTitle = {
    val btcTotalSum = app.kit.conf1Balance
    val btcFunds = if (btcTotalSum.value < 1) btcEmpty else denom withSign btcTotalSum
    val lnTotalSum = app.ChannelManager.notClosingOrRefunding.map(estimateTotalCanSend).sum
    val lnFunds = if (lnTotalSum < 1) lnEmpty else denom withSign MilliSatoshi(lnTotalSum)
    val daysLeft = currentBlocksLeft / broadcaster.blocksPerDay

    val subtitleText =
      if (currentBlocksLeft > 1) app.plurOrZero(syncOps, daysLeft)
      else if (currentPeerCount < 1) statusConnecting
      else statusOnline

    val titleText = s"""
      &#3647; <strong>$btcFunds</strong><br>
      &#9735; <strong>$lnFunds</strong><br>
      $subtitleText
    """.html

    // Update all the info in one pass
    UITask(customTitle setText titleText).run
  }

  // END UPDATING TITLE

  val adapter = new CutAdapter[TxWrap](24, R.layout.frag_tx_line) {
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

  val itemsListListener = new TxTracker {
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
  }

  def nativeTransactions = {
    val raw = app.kit.wallet.getRecentTransactions(adapter.max, false).asScala
    raw.toVector map bitcoinjTx2Wrap filterNot hiddenWrap filterNot watchedWrap
  }

  def sendBtcPopup(addr: Address): RateManager = {
    val hint = getString(amount_hint_can_send).format(denom withSign app.kit.conf1Balance)
    val form = getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val addressData = form.findViewById(R.id.addressData).asInstanceOf[TextView]
    val bld = baseBuilder(getString(action_coins_send), form)
    val rateManager = new RateManager(hint, form)

    def next(msat: MilliSatoshi) = new TxProcessor {
      def futureProcess(unsignedRequest: SendRequest) = {
        val signedRequest = app.kit.sign(unsignedRequest).tx
        app.kit blockingSend signedRequest
      }

      val pay = AddrData(msat, addr)
      def onTxFail(sendingError: Throwable) = {
        val bld = baseBuilder(messageWhenMakingTx(sendingError), null)
        mkForm(sendBtcPopup(addr), none, bld, dialog_ok, dialog_cancel)
      }
    }

    def sendAttempt(alert: AlertDialog): Unit = rateManager.result match {
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_small
      case Failure(probablyEmptySum) => app toast dialog_sum_empty
      case Success(ms) => rm(alert)(next(ms).start)
    }

    mkCheckForm(sendAttempt, none, bld, dialog_next, dialog_cancel)
    addressData setText humanFour(addr.toString)
    rateManager
  }

  def boostIncoming(wrap: TxWrap) = {
    val current = coloredIn(wrap.valueDelta)
    val increasedFee = RatesSaver.rates.feeSix divide 2
    val boost = coloredIn(wrap.valueDelta minus increasedFee)
    val userWarn = getString(boost_details).format(current, boost).html
    mkForm(ok = <(replace, onError)(none), none, baseBuilder(userWarn, null),
      dialog_ok, dialog_cancel)

    // Transaction hiding must always happen before replacement sending
    def replace = if (wrap.depth < 1 && !wrap.isDead) runAnd(wrap.tx setMemo HIDE) {
      val unsignedBoost = childPaysForParent(app.kit.wallet, wrap.tx, increasedFee)
      app.kit blockingSend app.kit.sign(unsignedBoost).tx
    }

    def onError(err: Throwable) = {
      // Make an old tx visible again
      wrap.tx setMemo null
      onFail(err)
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
    lst setHeaderDividersEnabled false
    lst addHeaderView detailsWrapper

    outside setOnClickListener onButtonTap {
      val smartbit = "https://smartbit.com.au/tx/"
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
    val notEnoughValue = wrap.valueDelta isLessThan RatesSaver.rates.feeSix
    val tooFresh = wrap.tx.getUpdateTime.getTime > System.currentTimeMillis - 1800L * 1000
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
  app.kit.peerGroup addConnectedEventListener constListener
  app.kit.peerGroup addDisconnectedEventListener constListener
  app.kit.peerGroup addBlocksDownloadedEventListener catchListener

  app.kit.wallet addCoinsSentEventListener itemsListListener
  app.kit.wallet addCoinsReceivedEventListener itemsListListener
  app.kit.wallet addTransactionConfidenceEventListener itemsListListener

  <(nativeTransactions, onFail) { txs =>
    // Fill list with bitcoin transactions
    updView(showText = txs.isEmpty)
    adapter set txs
  }

  Utils clickableTextField frag.findViewById(R.id.mnemonicInfo)
  toolbar setOnClickListener onFastTap(showDenomChooser)
  host setSupportActionBar toolbar
  updTitle

  def showQR(pr: PaymentRequest) = {
    host goTo classOf[RequestActivity]
    app.TransData.value = pr
  }

  def ifOperational(next: Vector[Channel] => Unit) = {
    val operational = app.ChannelManager.notClosingOrRefunding filter isOperational
    if (operational.isEmpty) app toast ln_status_none else next(operational)
  }

  def makePaymentRequest = ifOperational { operationalChannels =>
    // Get channels which contain assisted routes which can be used to receive a payment
    val chansWithRoutes: Map[Channel, PaymentRoute] = operationalChannels.flatMap(channelAndHop).toMap
    if (chansWithRoutes.isEmpty) showForm(negTextBuilder(dialog_ok, host getString err_ln_6_confs).create)
    else {

      // Check that enough room has been made in selected channels
      val maxCanReceive = MilliSatoshi(chansWithRoutes.keys.map(estimateCanReceive).max)
      val reserveUnspent = host getString err_ln_reserve_unspent format coloredOut(maxCanReceive)
      if (maxCanReceive < minHtlcValue) showForm(negTextBuilder(dialog_ok, reserveUnspent.html).create)
      else {

        val content = getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
        val hint = getString(amount_hint_can_receive).format(denom withSign maxCanReceive)
        val desc = content.findViewById(R.id.inputDescription).asInstanceOf[EditText]
        val rateManager = new RateManager(hint, content)

        def makeRequest(sum: MilliSatoshi, preimage: BinaryData) = {
          // Once again filter out those channels which can received a supplied amount
          val routes = chansWithRoutes.filterKeys(channel => estimateCanReceive(channel) >= sum.amount).values.toVector
          val pr = PaymentRequest(chainHash, Some(sum), Crypto sha256 preimage, nodePrivateKey, desc.getText.toString.trim, None, routes)
          val rd = emptyRD(pr, sum.amount)

          db.change(PaymentTable.newVirtualSql, params = rd.queryText, rd.paymentHashString)
          db.change(PaymentTable.newSql, pr.toJson, preimage, 1, HIDDEN, System.currentTimeMillis,
            pr.description, rd.paymentHashString, sum.amount, 0L, 0L)

          showQR(pr)
        }

        def recAttempt(alert: AlertDialog) = rateManager.result match {
          case Success(ms) if maxCanReceive < ms => app toast dialog_sum_big
          case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small
          case Failure(reason) => app toast dialog_sum_empty

          case Success(ms) => rm(alert) {
            // Requests without amount are not allowed for now
            <(makeRequest(ms, random getBytes 32), onFail)(none)
            app toast dialog_pr_making
          }
        }

        val bld = baseBuilder(getString(action_ln_details), content)
        mkCheckForm(recAttempt, none, bld, dialog_ok, dialog_cancel)
      }
    }
  }
}