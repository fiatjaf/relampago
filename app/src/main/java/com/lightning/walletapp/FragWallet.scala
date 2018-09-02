package com.lightning.walletapp

import spray.json._
import android.view._
import android.widget._
import org.bitcoinj.core._
import collection.JavaConverters._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.FragWallet._
import com.lightning.walletapp.R.drawable._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import android.os.{Bundle, Handler}
import scala.util.{Failure, Success, Try}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import android.database.{ContentObserver, Cursor}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi}
import com.lightning.walletapp.helper.{ReactLoader, RichCursor}
import org.bitcoinj.core.Transaction.{MIN_NONDUST_OUTPUT => MIN}
import com.lightning.walletapp.ln.Tools.{none, random, runAnd, wrap}
import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import android.support.v4.app.LoaderManager.LoaderCallbacks
import com.lightning.walletapp.lnutils.IconGetter.isTablet
import org.bitcoinj.wallet.SendRequest.childPaysForParent
import android.support.v4.content.Loader
import android.support.v7.widget.Toolbar
import org.bitcoinj.script.ScriptPattern
import android.support.v4.app.Fragment
import android.app.AlertDialog
import android.content.Intent
import android.net.Uri


object FragWallet {
  var worker: FragWalletWorker = _
  val REDIRECT = "goToLnOpsActivity"
}

class FragWallet extends Fragment {
  override def onCreateView(inf: LayoutInflater, vg: ViewGroup, bn: Bundle) = inf.inflate(R.layout.frag_view_pager_btc, vg, false)
  override def onViewCreated(view: View, state: Bundle) = worker = new FragWalletWorker(getActivity.asInstanceOf[WalletActivity], view)
  override def onDestroy = wrap(super.onDestroy)(worker.onFragmentDestroy)
  override def onResume = wrap(super.onResume)(worker.host.checkTransData)
}

class FragWalletWorker(val host: WalletActivity, frag: View) extends SearchBar with HumanTimeDisplay { me =>
  import host.{UITask, onButtonTap, showForm, negBuilder, baseBuilder, negTextBuilder, onFastTap, str2View, onTap}
  import host.{onFail, TxProcessor, getSupportLoaderManager, mkCheckForm, rm, mkCheckFormNeutral, <, showDenomChooser}

  val mnemonicWarn = frag.findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
  val customTitle = frag.findViewById(R.id.customTitle).asInstanceOf[TextView]
  val itemsList = frag.findViewById(R.id.itemsList).asInstanceOf[ListView]
  val toolbar = frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]

  val allTxsWrapper = host.getLayoutInflater.inflate(R.layout.frag_toggler, null)
  val toggler = allTxsWrapper.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  val imageMap = Array(await, await, conf1, dead, frozen)

  val paymentStates = app.getResources getStringArray R.array.ln_payment_states
  val expiryLeft = app.getResources getStringArray R.array.ln_status_expiry
  val syncOps = app.getResources getStringArray R.array.info_progress
  val txsConfs = app.getResources getStringArray R.array.txs_confs
  val lnTitleOutNoFee = app getString ln_outgoing_title_no_fee
  val statusOperational = app getString btc_status_operational
  val statusConnecting = app getString btc_status_connecting
  val lnTitleOut = app getString ln_outgoing_title
  val lnTitleIn = app getString ln_incoming_title
  val btcEmpty = app getString btc_empty
  val lnEmpty = app getString ln_empty
  val lnProof = app getString ln_proof

  // LISTENERS

  val blocksTitleListener = new BlocksListener {
    def onBlocksDownloaded(peer: Peer, block: Block, filteredBlock: FilteredBlock, left: Int) =
      if (left < 1) updPaymentList.run else if (left % broadcaster.blocksPerDay == 0) UITask(updTitle).run
  }

  val peersListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(connectedPeer: Peer, currentPeerCount: Int) = UITask(updTitle).run
    def onPeerConnected(connectedPeer: Peer, currentPeerCount: Int) = UITask(updTitle).run
  }

  val txsListener = new TxTracker {
    // isGreaterThan check because as of now both listeners are fired on incoming and outgoing txs
    def onCoinsSent(w: Wallet, txj: Transaction, a: Coin, b: Coin) = if (a isGreaterThan b) updBtcItems
    def onCoinsReceived(w: Wallet, txj: Transaction, a: Coin, b: Coin) = if (b isGreaterThan a) updBtcItems
    override def txConfirmed(txj: Transaction) = UITask(adapter.notifyDataSetChanged).run
  }

  val chanListener = new ChannelListener {
    // should be removed once frag is destroyed
    // prevent remote error spamming by using trigger
    private[this] var firstTimeError = true

    override def onProcessSuccess = {
      case (chan, data: HasCommitments, remoteError: wire.Error) if firstTimeError => UITask {
        val bld = baseBuilder(chan.data.announce.toString.html, remoteError.exception.getMessage)
        def close(alert: AlertDialog) = rm(alert)(chan process app.ChannelManager.CMDLocalShutdown)
        mkCheckFormNeutral(alert => rm(alert)(none), none, close, bld, dialog_ok, -1, ln_chan_force)
        firstTimeError = false
      }.run
    }

    override def onException = {
      case _ \ CMDAddImpossible(rd, code) =>
        // Remove this payment from unsent when failed
        PaymentInfoWrap.unsentPayments -= rd.pr.paymentHash
        val bld = negTextBuilder(dialog_ok, app getString code)
        UITask(host showForm bld.create).run
        PaymentInfoWrap failOnUI rd

      case chan \ HTLCExpiryException(_, htlc) =>
        val paymentHash = htlc.add.paymentHash.toString
        // Inform user about situation instead of force-closing automatically
        val bld = negTextBuilder(dialog_ok, app.getString(err_ln_expired).format(paymentHash).html)
        UITask(host showForm bld.setCustomTitle(chan.data.announce.toString.html).create).run

      case _ \ internal =>
        // Internal error has happened, show stack trace
        val stackTrace = UncaughtHandler toText internal
        val bld = negTextBuilder(dialog_ok, stackTrace)
        UITask(host showForm bld.create).run
    }
  }

  new LoaderCallbacks[Cursor] { self =>
    private[this] var lastQuery = new String
    private[this] val observer = new ContentObserver(new Handler) {
      override def onChange(fromMe: Boolean) = if (!fromMe) react(lastQuery)
    }

    def onCreateLoader(id: Int, bn: Bundle) = new ReactLoader[PaymentInfo](host) {
      val consume = (vec: InfoVec) => runAnd(lnItems = vec map LNWrap)(updPaymentList.run)
      def getCursor = if (lastQuery.isEmpty) bag.byRecent else bag.byQuery(lastQuery)
      def createItem(richCursor: RichCursor) = bag toPaymentInfo richCursor
    }

    type LoaderCursor = Loader[Cursor]
    type InfoVec = Vector[PaymentInfo]
    def onLoaderReset(loader: LoaderCursor) = none
    def onLoadFinished(loader: LoaderCursor, c: Cursor) = none
    me.react = vs => runAnd(lastQuery = vs)(getSupportLoaderManager.restartLoader(1, null, self).forceLoad)
    host.getContentResolver.registerContentObserver(db sqlPath PaymentTable.table, true, observer)
  }

  toggler setOnClickListener onFastTap {
    val newImg = if (currentCut > minLinesNum) ic_explode_24dp else ic_implode_24dp
    currentCut = if (currentCut > minLinesNum) minLinesNum else allItems.size
    toggler setImageResource newImg
    adapter.notifyDataSetChanged
  }

  // UPDATING TITLE

  def updTitle = {
    val btcTotalSum = app.kit.conf0Balance
    val lnTotalSum = app.ChannelManager.notClosingOrRefunding.map(estimateCanSend).sum
    val lnFunds = if (lnTotalSum < 1) lnEmpty else denom withSign MilliSatoshi(lnTotalSum)
    val btcFunds = if (btcTotalSum.isZero) btcEmpty else denom withSign btcTotalSum

    val subtitleText =
      if (app.kit.peerGroup.numConnectedPeers < 1) statusConnecting
      else if (app.ChannelManager.currentBlocksLeft < broadcaster.blocksPerDay) statusOperational
      else app.plurOrZero(syncOps, app.ChannelManager.currentBlocksLeft / broadcaster.blocksPerDay)

    customTitle setText s"""
      <img src="btc"/><strong>$btcFunds</strong><br>
      <img src="ln"/><strong>$lnFunds</strong><br>
      $subtitleText<img src="none"/>""".html
  }

  override def setupSearch(menu: Menu) = {
    // Expand payment list if search is active
    // hide payment description if it's not

    super.setupSearch(menu)
    searchView addOnAttachStateChangeListener new View.OnAttachStateChangeListener {
      def onViewDetachedFromWindow(searchViewLenseItem: View): Unit = updPaymentList.run
      def onViewAttachedToWindow(searchViewLenseItem: View): Unit = none
    }
  }

  // DISPLAYING ITEMS LIST

  val minLinesNum = 5
  var currentCut = minLinesNum
  var lnItems = Vector.empty[LNWrap]
  var btcItems = Vector.empty[BTCWrap]
  var allItems = Vector.empty[ItemWrap]

  def updPaymentList = {
    val delayedWraps = app.ChannelManager.delayedPublishes map ShowDelayedWrap
    val tempItems = if (isSearching) lnItems else delayedWraps ++ btcItems ++ lnItems
    allItems = tempItems.sortBy(_.getDate)(Ordering[java.util.Date].reverse) take 48

    UITask {
      allTxsWrapper setVisibility viewMap(allItems.size > minLinesNum)
      mnemonicWarn setVisibility viewMap(allItems.isEmpty)
      itemsList setVisibility viewMap(allItems.nonEmpty)
      adapter.notifyDataSetChanged
      updTitle
    }
  }

  val adapter = new BaseAdapter {
    def getCount = math.min(allItems.size, currentCut)
    def getItem(position: Int) = allItems(position)
    def getItemId(position: Int) = position

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val resource = if (isTablet) R.layout.frag_tx_line_tablet else R.layout.frag_tx_line
      val view = if (null == savedView) host.getLayoutInflater.inflate(resource, null) else savedView
      val holder = if (null == view.getTag) new ViewHolder(view) else view.getTag.asInstanceOf[ViewHolder]
      allItems(position) fillView holder
      view
    }
  }

  class ViewHolder(view: View) {
    val transactCircle = view.findViewById(R.id.transactCircle).asInstanceOf[ImageView]
    val transactWhen = view.findViewById(R.id.transactWhen).asInstanceOf[TextView]
    val transactWhat = view.findViewById(R.id.transactWhat).asInstanceOf[TextView]
    val transactSum = view.findViewById(R.id.transactSum).asInstanceOf[TextView]
    view setTag this
  }

  abstract class ItemWrap {
    def fillView(v: ViewHolder): Unit
    def getDate: java.util.Date
    def generatePopup: Unit
  }

  case class ShowDelayedWrap(stat: ShowDelayed) extends ItemWrap {
    val getDate = new java.util.Date(System.currentTimeMillis + stat.delay)
    val humanSum = sumIn.format(denom formatted stat.amount)

    def humanWhen = {
      val now = System.currentTimeMillis
      val blocksAsMsecs = now + 600000L * stat.delay
      val future = new java.util.Date(blocksAsMsecs)
      when(now, future)
    }

    def fillView(holder: ViewHolder) = {
      holder.transactSum setText s"<img src='btc'/>$humanSum".html
      holder.transactWhat setVisibility viewMap(isTablet)
      holder.transactCircle setImageResource await
      holder.transactWhat setText btc_refunding
      holder.transactWhen setText humanWhen
    }

    def generatePopup = {
      val inFiat = msatInFiatHuman(stat.amount)
      val base = app.getString(btc_pending_title)
      val paidFeePercent = stat.fee.amount / (stat.amount.amount / 100D)
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val viewTxOutside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]
      val viewShareBody = detailsWrapper.findViewById(R.id.viewShareBody).asInstanceOf[Button]
      val title = base.format(humanWhen, humanSum, inFiat, coloredOut(stat.fee), paidFeePercent)
      showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

      viewTxOutside setOnClickListener onButtonTap {
        val parentCommitTxid = stat.commitTx.txid.toString
        val uri = s"https://smartbit.com.au/tx/$parentCommitTxid"
        host startActivity new Intent(Intent.ACTION_VIEW, Uri parse uri)
      }

      viewShareBody setOnClickListener onButtonTap {
        val rawTx = fr.acinq.bitcoin.Transaction write stat.txn
        host share rawTx.toString
      }
    }
  }

  case class LNWrap(info: PaymentInfo) extends ItemWrap {
    val getDate: java.util.Date = new java.util.Date(info.stamp)

    def fillView(holder: ViewHolder) = {
      val marking = if (info.incoming == 1) sumIn else sumOut
      val humanSum = marking.format(denom formatted info.firstSum)

      holder.transactSum setText s"<img src='ln'/>$humanSum".html
      holder.transactCircle setImageResource imageMap(info.actualStatus)
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactWhat setVisibility viewMap(isTablet || isSearching)
      holder.transactWhat setText getDescription(info.description).html
    }

    def generatePopup = {
      val inFiat = msatInFiatHuman(info.firstSum)
      val noRetry = if (info.pr.isFresh) dialog_retry else -1
      val rd = emptyRD(info.pr, info.firstMsat, useCache = false)
      val humanStatus = s"<strong>${paymentStates apply info.actualStatus}</strong>"
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_ln_details, null)
      val paymentDetails = detailsWrapper.findViewById(R.id.paymentDetails).asInstanceOf[TextView]
      val paymentProof = detailsWrapper.findViewById(R.id.paymentProof).asInstanceOf[Button]
      val paymentDebug = detailsWrapper.findViewById(R.id.paymentDebug).asInstanceOf[Button]
      val paymentHash = detailsWrapper.findViewById(R.id.paymentHash).asInstanceOf[Button]
      paymentHash setOnClickListener onButtonTap(host share rd.paymentHashString)
      paymentDetails setText getDescription(info.description).html

      if (info.actualStatus == SUCCESS) {
        paymentHash setVisibility View.GONE
        paymentProof setVisibility View.VISIBLE
        paymentProof setOnClickListener onButtonTap {
          val serialized = PaymentRequest.write(pr = info.pr)
          host share lnProof.format(serialized, info.preimage.toString)
        }
      }

      for {
        responses <- PaymentInfo.errors.get(rd.pr.paymentHash)
        history = responses.reverse.map(_.toString) mkString "\n==\n"
        rd1 <- PaymentInfoWrap.inFlightPayments.get(rd.pr.paymentHash)
        routingPath = for (usedPaymentHop <- rd1.usedRoute) yield usedPaymentHop.humanDetails
        receiver = s"Payee: ${rd1.pr.nodeId.toString}, Expiry: ${rd1.pr.adjustedMinFinalCltvExpiry} blocks"
        fullDebugData = ("Your wallet" +: routingPath :+ receiver mkString "\n-->\n") + s"\n\n$history"
        _ = paymentDebug setOnClickListener onButtonTap(host share fullDebugData)
      } paymentDebug setVisibility View.VISIBLE

      def outgoingTitle = {
        val fee = MilliSatoshi(info.lastMsat - info.firstMsat)
        val paidFeePercent = fee.amount / (info.firstMsat / 100D)
        val title = lnTitleOut.format(humanStatus, coloredOut(info.firstSum), inFiat, coloredOut(fee), paidFeePercent)
        val expiryBlocksLeftPart = app.plurOrZero(expiryLeft, info.lastExpiry - broadcaster.currentHeight)
        if (info.actualStatus == WAITING) s"$expiryBlocksLeftPart<br>$title" else title
      }

      info.incoming -> onChainRunnable(rd.pr) match {
        case 0 \ Some(runnable) if info.lastMsat == 0 && info.lastExpiry == 0 && info.actualStatus == FAILURE =>
          // Payment was failed without even trying because wallet is offline or no suitable payment routes were found
          val bld = baseBuilder(lnTitleOutNoFee.format(humanStatus, coloredOut(info.firstSum), inFiat).html, detailsWrapper)
          mkCheckFormNeutral(alert => rm(alert)(none), none, alert => rm(alert)(runnable.run), bld, dialog_ok, -1, dialog_pay_onchain)

        case 0 \ _ if info.lastMsat == 0 && info.lastExpiry == 0 =>
          // Payment has not been tried yet because an onchain wallet is still offline
          val title = lnTitleOutNoFee.format(humanStatus, coloredOut(info.firstSum), inFiat)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

        case 0 \ Some(runnable) =>
          val bld = baseBuilder(outgoingTitle.html, detailsWrapper)
          def useOnchain(alert: AlertDialog) = rm(alert)(runnable.run)
          // Offer a fallback onchain address if payment was not successfull
          if (info.actualStatus != FAILURE) showForm(negBuilder(dialog_ok, outgoingTitle.html, detailsWrapper).create)
          else mkCheckFormNeutral(alert => rm(alert)(none), doSend(rd), useOnchain, bld, dialog_ok, noRetry, dialog_pay_onchain)

        case 0 \ None =>
          val bld = baseBuilder(outgoingTitle.html, detailsWrapper)
          // Only allow user to retry this payment while using excluded nodes and channels but not an onchain option
          if (info.actualStatus != FAILURE) showForm(negBuilder(dialog_ok, outgoingTitle.html, detailsWrapper).create)
          else mkCheckForm(alert => rm(alert)(none), doSend(rd), bld, dialog_ok, noRetry)

        case 1 \ _ =>
          // This is an incoming payment
          val title = lnTitleIn.format(humanStatus, coloredIn(info.firstSum), inFiat)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)
      }
    }
  }

  case class BTCWrap(wrap: TxWrap) extends ItemWrap {
    val getDate: java.util.Date = wrap.tx.getUpdateTime
    private[this] def txDepth = wrap.tx.getConfidence.getDepthInBlocks
    private[this] def txDead = DEAD == wrap.tx.getConfidence.getConfidenceType

    def fillView(holder: ViewHolder) = {
      val humanSum = wrap.visibleValue.isPositive match {
        case true => sumIn.format(denom formatted wrap.visibleValue)
        case false => sumOut.format(denom formatted -wrap.visibleValue)
      }

      val status = if (txDead) dead else if (txDepth >= minDepth) conf1 else await
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactSum setText s"<img src='btc'/>$humanSum".html
      holder.transactWhat setText wrap.tx.getHashAsString
      holder.transactWhat setVisibility viewMap(isTablet)
      holder.transactCircle setImageResource status
    }

    def generatePopup = {
      val confs = app.plurOrZero(txsConfs, txDepth)
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val viewTxOutside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]
      val viewShareBody = detailsWrapper.findViewById(R.id.viewShareBody).asInstanceOf[Button]
      val lst = host.getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]

      val color = if (wrap.visibleValue.isPositive) coloredIn else coloredOut
      val humanOutputs = wrap directedScriptPubKeysWithValueTry wrap.visibleValue.isPositive collect {
        case Success(chanFunding \ value) if chanFunding.isSentToP2WSH => P2WSHData(value, chanFunding).destination(coloredChan).html
        case Success(pks \ value) if !ScriptPattern.isOpReturn(pks) => AddrData(value, pks getToAddress app.params).destination(color).html
      }

      val views = new ArrayAdapter(host, R.layout.frag_top_tip, R.id.titleTip,
        humanOutputs.toArray) { override def isEnabled(pos: Int) = false }

      lst setHeaderDividersEnabled false
      lst addHeaderView detailsWrapper
      lst setAdapter views
      lst setDivider null

      viewTxOutside setOnClickListener onButtonTap {
        val uri = s"https://smartbit.com.au/tx/" + wrap.tx.getHashAsString
        host startActivity new Intent(Intent.ACTION_VIEW, Uri parse uri)
      }

      viewShareBody setOnClickListener onButtonTap {
        val rawTransaction = wrap.tx.unsafeBitcoinSerialize
        host share BinaryData(rawTransaction).toString
      }

      val header = wrap.fee match {
        case _ if wrap.visibleValue.isPositive =>
          // This is an incoming tx, do not show a fee
          val inFiat = msatInFiatHuman(wrap.visibleValue)
          val humanAmount = coloredIn(wrap.visibleValue)
          val base = app.getString(btc_incoming_title)
          base.format(confs, humanAmount, inFiat)

        case Some(fee) =>
          // This is an outgoing tx with fee
          val base = app.getString(btc_outgoing_title)
          val inFiat = msatInFiatHuman(-wrap.visibleValue)
          val humanAmount = coloredOut(-wrap.visibleValue)
          val paidFeePercent = fee.value / (-wrap.visibleValue.value / 100D)
          base.format(confs, humanAmount, inFiat, coloredOut(fee), paidFeePercent)

        case None =>
          // Should never happen but whatever
          val inFiat = msatInFiatHuman(-wrap.visibleValue)
          val humanAmount = coloredOut(-wrap.visibleValue)
          val base = app.getString(btc_outgoing_title_no_fee)
          base.format(confs, humanAmount, inFiat)
      }

      // Check if CPFP can be applied: enough value to handle the fee, not dead yet
      if (wrap.valueDelta.isLessThan(RatesSaver.rates.feeSix) || txDepth > 0) showForm(negBuilder(dialog_ok, header.html, lst).create)
      else mkCheckForm(alert => rm(alert)(none), boostIncoming(wrap), baseBuilder(header.html, lst), dialog_ok, dialog_boost)
    }
  }

  // WORKER EVENT HANDLERS

  def onFragmentDestroy = {
    for (c <- app.ChannelManager.all) c.listeners -= chanListener
    app.kit.wallet removeTransactionConfidenceEventListener txsListener
    app.kit.peerGroup removeBlocksDownloadedEventListener blocksTitleListener
    app.kit.peerGroup removeDisconnectedEventListener peersListener
    app.kit.peerGroup removeConnectedEventListener peersListener
    app.kit.wallet removeCoinsReceivedEventListener txsListener
    app.kit.wallet removeCoinsSentEventListener txsListener
  }

  // LN SEND / RECEIVE

  def receive(chansWithRoutes: Map[Channel, PaymentRoute], maxCanReceive: MilliSatoshi) = {
    val content = host.getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
    val hint = app.getString(amount_hint_can_receive).format(denom withSign maxCanReceive)
    val rateManager = new RateManager(hint, content)

    def makeRequest(sum: MilliSatoshi, preimg: BinaryData) = {
      val onChainFallback = Some(app.kit.currentAddress.toString)
      val info = content.findViewById(R.id.inputDescription).asInstanceOf[EditText].getText.toString.trim
      val routes = chansWithRoutes.filterKeys(chan => estimateCanReceiveCapped(chan) >= sum.amount).values.toVector
      val pr = PaymentRequest(chainHash, Some(sum), Crypto sha256 preimg, nodePrivateKey, info, onChainFallback, routes)
      val rd = emptyRD(pr, sum.amount, useCache = true)

      db.change(PaymentTable.newVirtualSql, params = rd.queryText, rd.paymentHashString)
      db.change(PaymentTable.newSql, pr.toJson, preimg, 1, HIDDEN, System.currentTimeMillis,
        pr.description, rd.paymentHashString, sum.amount, 0L, 0L, NOCHANID)

      app.TransData.value = pr
      host goTo classOf[RequestActivity]
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

    def useMax(alert: AlertDialog) = rateManager setSum Try(maxCanReceive)
    val bld = baseBuilder(title = app.getString(ln_receive_title).html, content)
    mkCheckFormNeutral(recAttempt, none, useMax, bld, dialog_ok, dialog_cancel, dialog_max)
  }

  def sendPayment(pr: PaymentRequest) =
    if (PaymentRequest.prefixes(chainHash) != pr.prefix) app toast err_general
    else if (pr.nodeId == nodePublicKey) app toast err_self_payment
    else if (!pr.isFresh) app toast dialog_pr_expired else {

      // This fetches normal channels which MAY be offline currently, this is fine
      val openingChannels = app.ChannelManager.notClosingOrRefunding.filter(isOpening)
      val operationalChannels = app.ChannelManager.notClosingOrRefunding.filter(isOperational)
      if (operationalChannels.isEmpty && openingChannels.nonEmpty) onFail(app getString err_ln_still_opening)
      else if (operationalChannels.isEmpty) app toast ln_no_open_chans else {

        val maxCanSendUncapped = operationalChannels.map(estimateCanSend).max
        val maxCanSend = MilliSatoshi apply math.min(maxCanSendUncapped, LNParams.maxHtlcValueMsat)
        val content = host.getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
        val hint = app.getString(amount_hint_can_send).format(denom withSign maxCanSend)
        val rateManager = new RateManager(hint, content)

        def sendAttempt(alert: AlertDialog) = rateManager.result match {
          case Success(ms) if maxCanSend < ms => app toast dialog_sum_big
          case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small
          case Success(ms) if pr.amount.exists(_ * 2 < ms) => app toast dialog_sum_big
          case Success(ms) if pr.amount.exists(_ > ms) => app toast dialog_sum_small
          case Failure(reason) => app toast dialog_sum_empty

          case Success(ms) => rm(alert) {
            // Custom amount may be higher than requested
            me doSend emptyRD(pr, ms.amount, useCache = true)
          }
        }

        val runnableOpt = onChainRunnable(pr)
        val description = getDescription(pr.description)
        val bld = baseBuilder(app.getString(ln_send_title).format(description).html, content)
        def useOnchain(alert: AlertDialog) = rm(alert) { for (runnable <- runnableOpt) runnable.run }
        if (pr.amount.forall(_ <= maxCanSend) || runnableOpt.isEmpty) mkCheckForm(sendAttempt, none, bld, dialog_pay, dialog_cancel)
        else mkCheckFormNeutral(sendAttempt, none, useOnchain, bld, dialog_pay, dialog_cancel, dialog_pay_onchain)
        for (askedSum <- pr.amount) rateManager setSum Try(askedSum)
      }
    }

  def doSend(rd: RoutingData) =
    app.ChannelManager checkIfSendable rd match {
      case Right(rd1) => PaymentInfoWrap addPendingPayment rd1
      case Left(sanityCheckErrorMsg) => onFail(sanityCheckErrorMsg)
    }

  // BTC SEND / BOOST

  def onChainRunnable(pr: PaymentRequest) =
    for (adr <- pr.fallbackAddress) yield UITask {
      val fallback = Address.fromString(app.params, adr)
      val tryMSat = Try(pr.amount.get)

      sendBtcPopup(fallback) { txj =>
        // Hide offchain payment once onchain is sent
        PaymentInfoWrap.updStatus(HIDDEN, pr.paymentHash)
        PaymentInfoWrap.uiNotify
      } setSum tryMSat
    }

  def sendBtcPopup(addr: Address)(and: Transaction => Unit): RateManager = {
    val form = host.getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val hint = app.getString(amount_hint_can_send).format(denom withSign app.kit.conf1Balance)
    val addressData = form.findViewById(R.id.addressData).asInstanceOf[TextView]
    val rateManager = new RateManager(hint, form)

    def next(ms: MilliSatoshi) = new TxProcessor {
      def futureProcess(unsignedRequest: SendRequest) = {
        val signedSpendTx = app.kit.sign(unsignedRequest).tx
        and(app.kit blockSend signedSpendTx)
      }

      val pay = AddrData(ms, addr)
      def retry = sendBtcPopup(addr)(and)
      def onTxFail(sendingError: Throwable) = {
        val bld = baseBuilder(title = messageWhenMakingTx(sendingError), null)
        mkCheckForm(alert => rm(alert)(retry), none, bld, dialog_retry, dialog_cancel)
      }
    }

    def sendAttempt(alert: AlertDialog): Unit = rateManager.result match {
      case Success(ms) if MIN isGreaterThan ms => app toast dialog_sum_small
      case Failure(probablyEmptySum) => app toast dialog_sum_empty
      case Success(ms) => rm(alert)(next(ms).start)
    }

    val bld = baseBuilder(app.getString(btc_send_title).html, form)
    mkCheckForm(sendAttempt, none, bld, dialog_next, dialog_cancel)
    addressData setText humanSix(addr.toString)
    rateManager
  }

  def boostIncoming(wrap: TxWrap) = {
    val current = coloredIn(wrap.valueDelta)
    val newFee = RatesSaver.rates.feeSix div 2
    val boost = coloredIn(wrap.valueDelta minus newFee)
    // Unlike normal transaction this one uses a whole half of current feeSix
    val userWarn = baseBuilder(app.getString(boost_details).format(current, boost).html, null)
    mkCheckForm(alert => rm(alert)(none), <(replace, onError)(none), userWarn, dialog_cancel, dialog_boost)

    def replace: Unit = {
      if (wrap.tx.getConfidence.getDepthInBlocks > 0) return
      if (DEAD == wrap.tx.getConfidence.getConfidenceType) return
      wrap.makeHidden

      // Parent transaction hiding must happen before child is broadcasted
      val unsigned = childPaysForParent(app.kit.wallet, wrap.tx, newFee)
      app.kit blockSend app.kit.sign(unsigned).tx
    }

    def onError(err: Throwable) = {
      // Make an old tx visible again
      wrap.tx setMemo null
      onFail(err)
    }
  }

  def updBtcItems = {
    val rawTxs = app.kit.wallet.getRecentTransactions(24, false)
    val wraps = for (txnj <- rawTxs.asScala.toVector) yield new TxWrap(txnj)
    btcItems = for (wrap <- wraps if wrap.isVisible) yield BTCWrap(wrap)
    updPaymentList.run
  }

  host setSupportActionBar toolbar
  Utils clickableTextField frag.findViewById(R.id.mnemonicInfo)
  toolbar setOnClickListener onFastTap { if (!isSearching) showDenomChooser }
  // Only update a minimized payments list to eliminate possible performance slowdowns
  host.timer.schedule(if (currentCut <= minLinesNum) adapter.notifyDataSetChanged, 10000, 10000)
  itemsList setOnItemClickListener onTap { position => adapter.getItem(position).generatePopup }
  itemsList setFooterDividersEnabled false
  itemsList addFooterView allTxsWrapper
  itemsList setAdapter adapter

  for (c <- app.ChannelManager.all) c.listeners += chanListener
  app.kit.wallet addTransactionConfidenceEventListener txsListener
  app.kit.peerGroup addBlocksDownloadedEventListener blocksTitleListener
  app.kit.peerGroup addDisconnectedEventListener peersListener
  app.kit.peerGroup addConnectedEventListener peersListener
  app.kit.wallet addCoinsReceivedEventListener txsListener
  app.kit.wallet addCoinsSentEventListener txsListener
  react(new String)
  updBtcItems
}