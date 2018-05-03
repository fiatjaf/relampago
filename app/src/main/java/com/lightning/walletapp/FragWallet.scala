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
import android.database.{ContentObserver, Cursor}
import com.lightning.walletapp.helper.{ReactLoader, RichCursor}
import org.bitcoinj.core.Transaction.{MIN_NONDUST_OUTPUT => MIN}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Satoshi}
import com.lightning.walletapp.ln.Tools.{none, random, runAnd, wrap}
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import android.support.v4.app.LoaderManager.LoaderCallbacks
import org.bitcoinj.wallet.SendRequest.childPaysForParent
import android.support.v4.content.Loader
import android.support.v7.widget.Toolbar
import android.support.v4.app.Fragment
import org.bitcoinj.wallet.SendRequest
import android.app.AlertDialog
import android.content.Intent
import java.util.TimerTask
import android.net.Uri


object FragWallet {
  var currentPeerCount = 0
  var currentBlocksLeft = 0
  var worker: FragWalletWorker = _
  val REDIRECT = "goToLnOpsActivity"
}

class FragWallet extends Fragment {
  override def onCreateView(inf: LayoutInflater, vg: ViewGroup, bn: Bundle) = inf.inflate(R.layout.frag_view_pager_btc, vg, false)
  override def onViewCreated(view: View, state: Bundle) = worker = new FragWalletWorker(getActivity.asInstanceOf[WalletActivity], view)
  override def onDestroy = wrap(super.onDestroy)(worker.onFragmentDestroy)
  override def onResume = wrap(super.onResume)(worker.onFragmentResume)
}

class FragWalletWorker(val host: WalletActivity, frag: View) extends SearchBar with HumanTimeDisplay { me =>
  import host.{UITask, onButtonTap, mkForm, showForm, negBuilder, baseBuilder, negTextBuilder, onFastTap, str2View}
  import host.{onFail, TxProcessor, getSupportLoaderManager, rm, mkCheckForm, <, onTap, showDenomChooser}
  def getDescription(rawText: String) = if (rawText.isEmpty) s"<i>$noDesc</i>" else rawText take 140

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
  val statusConnecting = app getString btc_status_connecting
  val statusOnline = app getString btc_status_online
  val lnTitleOut = app getString ln_outgoing_title
  val lnTitleIn = app getString ln_incoming_title
  val noDesc = app getString ln_no_description
  val btcEmpty = app getString btc_empty
  val lnEmpty = app getString ln_empty

  // UPDATING TITLE

  val catchListener = new BlocksListener {
    def onBlocksDownloaded(sourcePeerNode: Peer, block: Block, filteredBlock: FilteredBlock, left: Int) = {
      if (left > broadcaster.blocksPerDay) app.kit.peerGroup addBlocksDownloadedEventListener getNextTracker(left)
      app.kit.peerGroup removeBlocksDownloadedEventListener this
    }

    def getNextTracker(initBlocksLeft: Int) = new BlocksListener {
      def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = {
        // Runtime optimization: avoid calling update more than once per 144 blocks
        if (left % broadcaster.blocksPerDay == 0) updTitle.run
        currentBlocksLeft = left
      }
    }
  }

  val constListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(peer: Peer, peerCount: Int) = runAnd(currentPeerCount = peerCount)(updTitle.run)
    def onPeerConnected(peer: Peer, peerCount: Int) = runAnd(currentPeerCount = peerCount)(updTitle.run)
  }

  def updTitle = {
    val btcTotalSum = app.kit.conf0Balance
    val lnTotalSum = app.ChannelManager.notClosingOrRefunding.map(estimateTotalCanSend).sum
    val lnFunds = if (lnTotalSum < 1) lnEmpty else denom withSign MilliSatoshi(lnTotalSum)
    val btcFunds = if (btcTotalSum.isZero) btcEmpty else denom withSign btcTotalSum
    val daysLeft = currentBlocksLeft / broadcaster.blocksPerDay

    val subtitleText =
      if (currentBlocksLeft > 6) app.plurOrZero(syncOps, daysLeft)
      else if (currentPeerCount < 1) statusConnecting
      else statusOnline

    UITask(customTitle setText s"""
      &#3647; <strong>$btcFunds</strong><br>
      &#9735; <strong>$lnFunds</strong><br>
      $subtitleText""".html)
  }

  // END UPDATING TITLE

  override def setupSearch(menu: Menu) = {
    // Expand payment list if search is active
    // hide payment description if it's not

    super.setupSearch(menu)
    searchView addOnAttachStateChangeListener new View.OnAttachStateChangeListener {
      def onViewDetachedFromWindow(searchViewItem: View) = adapter.notifyDataSetChanged
      def onViewAttachedToWindow(searchViewItem: View) = adapter.notifyDataSetChanged
    }
  }

  // DISPLAYING ITEMS LIST

  val minLinesNum = 4
  var currentCut = minLinesNum
  var lnItems = Vector.empty[LNWrap]
  var btcItems = Vector.empty[BTCWrap]
  var allItems = Vector.empty[ItemWrap]

  val adapter = new BaseAdapter {
    def getCount = math.min(allItems.size, currentCut)
    def getItem(position: Int) = allItems(position)
    def getItemId(position: Int) = position

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val view = if (null == savedView) host.getLayoutInflater.inflate(R.layout.frag_tx_line, null) else savedView
      val holder = if (null == view.getTag) new ViewHolder(view) else view.getTag.asInstanceOf[ViewHolder]
      allItems(position) fillView holder
      view
    }
  }

  val itemsListListener = new TxTracker {
    override def coinsReceived(txj: Transaction) = guard(txj)
    override def coinsSent(txj: Transaction) = guard(txj)
    override def txConfirmed(txj: Transaction) = {
      // Update title amounts, mark as confirmed
      UITask(adapter.notifyDataSetChanged).run
      Vibrator.vibrate
      updTitle.run
    }

    def guard(txj: Transaction): Unit = {
      val transactionWrap = new TxWrap(txj)
      if (transactionWrap.valueDelta.isZero) return
      val btcs = BTCWrap(transactionWrap) +: btcItems
      btcItems = btcs.filter(_.wrap.isVisible)
      updList(btcItems ++ lnItems).run
      updTitle.run
    }
  }

  val updList: Vector[ItemWrap] => TimerTask = items => {
    // Orders BTC and LN payments by their date and updates UI accordingly
    allItems = items.sortBy(_.getDate)(Ordering[java.util.Date].reverse)
    val showToggle = allItems.size > minLinesNum

    UITask {
      allTxsWrapper setVisibility viewMap(showToggle)
      mnemonicWarn setVisibility viewMap(allItems.isEmpty)
      itemsList setVisibility viewMap(allItems.nonEmpty)
      adapter.notifyDataSetChanged
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

  case class LNWrap(info: PaymentInfo) extends ItemWrap {
    val getDate: java.util.Date = new java.util.Date(info.stamp)

    def fillView(holder: ViewHolder) = {
      val humanSum = info.incoming match {
        case 1 => sumIn.format(denom formatted info.firstSum)
        case 0 => sumOut.format(denom formatted info.firstSum)
      }

      val description = getDescription(info.description)
      val humanHash = humanFour(info.hash.toUpperCase take 24)
      val humanSumDetails = s"<font color=#999999>$humanHash</font><br>$description"
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactCircle setImageResource imageMap(info.actualStatus)
      holder.transactWhat setVisibility viewMap(isSearching)
      holder.transactSum setText s"&#9735; $humanSum".html
      holder.transactWhat setText humanSumDetails.html
    }

    def generatePopup = {
      val inFiat = msatInFiatHuman(info.firstSum)
      val rd = emptyRD(info.pr, info.firstMsat, useCache = false)
      val humanStatus = s"<strong>${paymentStates apply info.actualStatus}</strong>"
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_ln_details, null)
      val paymentDetails = detailsWrapper.findViewById(R.id.paymentDetails).asInstanceOf[TextView]
      val paymentProof = detailsWrapper.findViewById(R.id.paymentProof).asInstanceOf[Button]
      val paymentHash = detailsWrapper.findViewById(R.id.paymentHash).asInstanceOf[Button]
      paymentHash setOnClickListener onButtonTap(host share rd.paymentHashString)
      paymentDetails setText getDescription(info.description).html

      def makeProof =
        // Signed request along with a preimage constitute a proof
        app.getString(ln_proof).format(PaymentRequest write info.pr,
          rd.paymentHashString, info.preimage.toString)

      if (info.actualStatus == SUCCESS) {
        paymentHash setVisibility View.GONE
        paymentProof setVisibility View.VISIBLE
        val listener = onButtonTap(host share makeProof)
        paymentProof setOnClickListener listener
      }

      info.incoming match {
        case 0 if info.lastMsat == 0 && info.lastExpiry == 0 =>
          // Payment has not been sent yet because on-chain wallet is still offline
          val title = lnTitleOutNoFee.format(humanStatus, coloredOut(info.firstSum), inFiat)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

        case 0 =>
          val fee = MilliSatoshi(info.lastMsat - info.firstMsat)
          val feeAsPercent = fee.amount / (info.firstMsat / 100D)
          val title = lnTitleOut.format(humanStatus, coloredOut(info.firstSum), coloredOut(fee), feeAsPercent, inFiat)
          val titleWithExpiry = app.plurOrZero(expiryLeft, info.lastExpiry - broadcaster.currentHeight) + "<br>" + title
          val title1 = if (info.actualStatus == WAITING) titleWithExpiry else title

          // Allow user to retry this payment using excluded nodes and channels when it is a failure and pr is not expired yet
          if (info.actualStatus != FAILURE || !info.pr.isFresh) showForm(negBuilder(dialog_ok, title1.html, detailsWrapper).create)
          else mkForm(doSend(rd), none, baseBuilder(title1.html, detailsWrapper), dialog_retry, dialog_cancel)

        case 1 =>
          val title = lnTitleIn.format(humanStatus, coloredIn(info.firstSum), inFiat)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)
      }
    }
  }

  case class BTCWrap(wrap: TxWrap) extends ItemWrap {
    val getDate: java.util.Date = wrap.tx.getUpdateTime

    def fillView(holder: ViewHolder) = {
      val humanSum = wrap.visibleValue.isPositive match {
        case true => sumIn.format(denom formatted wrap.visibleValue)
        case false => sumOut.format(denom formatted -wrap.visibleValue)
      }

      val status = if (wrap.isDead) dead else if (wrap.depth >= minDepth) conf1 else await
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactSum setText s"&#3647; $humanSum".html
      holder.transactCircle setImageResource status
      holder.transactWhat setVisibility View.GONE
    }

    def generatePopup = {
      val confs = app.plurOrZero(txsConfs, wrap.depth)
      val marking = if (wrap.visibleValue.isPositive) sumIn else sumOut
      val outputs = wrap.payDatas(wrap.visibleValue.isPositive).flatMap(_.toOption)
      val humanOutputs = for (pay <- outputs) yield marking.format(pay.destination).html
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val viewTxOutside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]
      val lst = host.getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
      val views = new ArrayAdapter(host, R.layout.frag_top_tip, R.id.titleTip, humanOutputs.toArray)

      lst setHeaderDividersEnabled false
      lst addHeaderView detailsWrapper
      lst setAdapter views

      viewTxOutside setOnClickListener onButtonTap {
        val uri = s"https://testnet.smartbit.com.au/tx/" + wrap.tx.getHashAsString
        host startActivity new Intent(Intent.ACTION_VIEW, Uri parse uri)
      }

      val header = wrap.fee match {
        case _ if wrap.isDead => sumOut format txsConfs.last

        case _ if wrap.visibleValue.isPositive =>
          val amount: MilliSatoshi = wrap.visibleValue
          val inFiat: String = msatInFiatHuman(ms = amount)
          val incomingTitle = app.getString(btc_incoming_title)
          incomingTitle.format(confs, coloredIn(amount), inFiat)

        case None =>
          val amount = Satoshi(-wrap.visibleValue.value)
          val inFiat: String = msatInFiatHuman(ms = amount)
          val titleNoFee = app.getString(btc_outgoing_title_no_fee)
          titleNoFee.format(confs, coloredOut(amount), inFiat)

        case Some(fee) =>
          val amount = Satoshi(-wrap.visibleValue.value)
          val inFiat: String = msatInFiatHuman(ms = amount)
          val feePercent = fee.value / (amount.amount / 100D)
          val titleWithFee = app.getString(btc_outgoing_title)
          titleWithFee.format(confs, coloredOut(amount),
            coloredOut(fee), feePercent, inFiat)
      }

      // See if CPFP can be applied
      val notEnough = wrap.valueDelta isLessThan RatesSaver.rates.feeSix
      val tooFresh = wrap.tx.getUpdateTime.getTime > System.currentTimeMillis - 1800L * 1000
      if (wrap.depth > 0 || tooFresh || notEnough) showForm(negBuilder(dialog_ok, header.html, lst).create)
      else mkForm(none, boostIncoming(wrap), baseBuilder(header.html, lst), dialog_ok, dialog_boost)
    }
  }

  toggler setOnClickListener onFastTap {
    val newImg = if (currentCut == minLinesNum) ic_expand_less_black_24dp else ic_expand_more_black_24dp
    currentCut = if (currentCut == minLinesNum) allItems.size else minLinesNum
    toggler setImageResource newImg
    adapter.notifyDataSetChanged
  }

  new LoaderCallbacks[Cursor] { self =>
    def onLoadFinished(loader: LoaderCursor, c: Cursor) = none
    def onLoaderReset(loader: LoaderCursor) = none
    type LoaderCursor = Loader[Cursor]
    type InfoVec = Vector[PaymentInfo]

    private[this] var lastQuery = new String
    private[this] val observer = new ContentObserver(new Handler) {
      override def onChange(fromMe: Boolean) = if (!fromMe) react(lastQuery)
    }

    def recentPays = new ReactLoader[PaymentInfo](host) {
      val consume = (vec: InfoVec) => runAnd(lnItems = vec map LNWrap)(updList(btcItems ++ lnItems).run)
      def createItem(richCursor: RichCursor) = bag toPaymentInfo richCursor
      def getCursor = bag.byRecent
    }

    def searchPays = new ReactLoader[PaymentInfo](host) {
      val consume = (vec: InfoVec) => runAnd(lnItems = vec map LNWrap)(updList(lnItems).run)
      def createItem(rCursor: RichCursor) = bag toPaymentInfo rCursor
      def getCursor = bag byQuery lastQuery
    }

    def onCreateLoader(loaderId: Int, bundle: Bundle) = if (lastQuery.isEmpty) recentPays else searchPays
    me.react = vs => runAnd(lastQuery = vs)(getSupportLoaderManager.restartLoader(1, null, self).forceLoad)
    host.getContentResolver.registerContentObserver(db sqlPath PaymentTable.table, true, observer)
  }

  // END DISPLAYING ITEMS LIST

  def onFragmentResume = {
    for (c <- app.ChannelManager.all) c.listeners += chanListener
    // Calling host when this fragment is definitely created
    runAnd(updTitle.run)(host checkTransData null)
  }

  def onFragmentDestroy = {
    for (c <- app.ChannelManager.all) c.listeners -= chanListener
    app.kit.wallet removeCoinsSentEventListener itemsListListener
    app.kit.wallet removeCoinsReceivedEventListener itemsListListener
    app.kit.wallet removeTransactionConfidenceEventListener itemsListListener

    app.kit.peerGroup removeConnectedEventListener constListener
    app.kit.peerGroup removeDisconnectedEventListener constListener
    app.kit.peerGroup removeBlocksDownloadedEventListener catchListener
  }

  // LN STUFF

  val chanListener = new ChannelListener {
    // Should be removed on activity destroyed

    override def onError = {
      // Commit tx fee + channel reserve forbid sending of this payment
      // inform user with all the details laid out as cleanly as possible
      case _ \ CMDReserveOverflow(rpi, missingSat, reserveSat) =>

        val message = host getString err_ln_fee_overflow
        val reserve = coloredIn apply Satoshi(reserveSat)
        val missing = coloredOut apply Satoshi(missingSat)
        val sending = coloredOut apply MilliSatoshi(rpi.firstMsat)
        onFail(message.format(reserve, sending, missing).html)

      case _ \ CMDAddImpossible(_, code) =>
        onFail(host getString code)

      case chan \ error =>
        val content = UncaughtHandler toText error
        val dlg = negTextBuilder(dialog_ok, content)
        UITask(host showForm dlg.create).run
    }

    override def onBecome = {
      // Update UI on all changes
      case state => updTitle.run
    }
  }

  def showQR(pr: PaymentRequest) = {
    host goTo classOf[RequestActivity]
    app.TransData.value = pr
  }

  def ifOperational(next: Vector[Channel] => Unit) = {
    // This fetches normal channels which MAY be offline and this is intentional
    val operational = app.ChannelManager.notClosingOrRefunding.filter(isOperational)
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

        val content = host.getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
        val hint = app.getString(amount_hint_can_receive).format(denom withSign maxCanReceive)
        val rateManager = new RateManager(hint, content)

        def makeRequest(sum: MilliSatoshi, preimage: BinaryData) = {
          // Once again filter out those channels which can receive a supplied amount
          val description = content.findViewById(R.id.inputDescription).asInstanceOf[EditText].getText.toString.trim
          val routes = chansWithRoutes.filterKeys(channel => estimateCanReceive(channel) >= sum.amount).values.toVector
          val pr = PaymentRequest(chainHash, Some(sum), Crypto sha256 preimage, nodePrivateKey, description, None, routes)
          val rd = emptyRD(pr, sum.amount, useCache = true)

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

        val bld = baseBuilder(app.getString(ln_receive_title).html, content)
        mkCheckForm(recAttempt, none, bld, dialog_ok, dialog_cancel)
      }
    }
  }

  def sendPayment(pr: PaymentRequest) = ifOperational { operationalChannels =>
    if (PaymentRequest.prefixes(chainHash) != pr.prefix) app toast err_general
    else if (pr.nodeId == nodePublicKey) app toast err_general
    else if (!pr.isFresh) app toast dialog_pr_expired
    else {

      val description = me getDescription pr.description
      val title = app.getString(ln_send_title).format(description)
      val maxCanSend = MilliSatoshi(operationalChannels.map(estimateCanSend).max)
      val hint = app.getString(amount_hint_can_send).format(denom withSign maxCanSend)
      val content = host.getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
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

      val bld = baseBuilder(title.html, content)
      mkCheckForm(sendAttempt, none, bld, dialog_pay, dialog_cancel)
      for (amountMsat <- pr.amount) rateManager setSum Try(amountMsat)
    }
  }

  def doSend(rd: RoutingData) =
    app.ChannelManager checkIfSendable rd match {
      case Right(okRD) => PaymentInfoWrap addPendingPayment okRD
      case Left(sanityCheckErrorMsg) => onFail(sanityCheckErrorMsg)
    }

  // END LN STUFF

  // BTC SEND AND BOOST

  def sendBtcPopup(addr: Address): RateManager = {
    val form = host.getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val hint = app.getString(amount_hint_can_send).format(denom withSign app.kit.conf1Balance)
    val addressData = form.findViewById(R.id.addressData).asInstanceOf[TextView]
    val rateManager = new RateManager(hint, form)

    def next(ms: MilliSatoshi) = new TxProcessor {
      def futureProcess(unsignedRequest: SendRequest) =
        app.kit blockingSend app.kit.sign(unsignedRequest).tx

      val pay = AddrData(ms, addr)
      def onTxFail(sendingError: Throwable) = {
        val bld = baseBuilder(messageWhenMakingTx(sendingError), null)
        mkForm(sendBtcPopup(addr), none, bld, dialog_ok, dialog_cancel)
      }
    }

    def sendAttempt(alert: AlertDialog): Unit = rateManager.result match {
      case Success(ms) if MIN isGreaterThan ms => app toast dialog_sum_small
      case Failure(probablyEmptySum) => app toast dialog_sum_empty
      case Success(ms) => rm(alert)(next(ms).start)
    }

    val bld = baseBuilder(app.getString(btc_send_title).html, form)
    mkCheckForm(sendAttempt, none, bld, dialog_next, dialog_cancel)
    addressData setText humanFour(addr.toString)
    rateManager
  }

  def boostIncoming(wrap: TxWrap) = {
    val current = coloredIn(wrap.valueDelta)
    val newFee = RatesSaver.rates.feeSix div 2
    val boost = coloredIn(wrap.valueDelta minus newFee)
    // Unlike normal transaction this one uses a whole half of current feeSix
    val userWarn = baseBuilder(app.getString(boost_details).format(current, boost).html, null)
    mkForm(ok = <(replace, onError)(none), none, userWarn, dialog_ok, dialog_cancel)

    def replace = if (wrap.depth < 1 && !wrap.isDead) runAnd(wrap.makeHidden) {
      // Parent transaction hiding must always happen before child is broadcasted
      val unsigned = childPaysForParent(app.kit.wallet, wrap.tx, newFee)
      app.kit blockingSend app.kit.sign(unsigned).tx
    }

    def onError(err: Throwable) = {
      // Make an old tx visible again
      wrap.tx setMemo null
      onFail(err)
    }
  }

  // END BTC SEND AND BOOST

  host setSupportActionBar toolbar
  toolbar setOnClickListener onFastTap { if (!isSearching) showDenomChooser }
  itemsList setOnItemClickListener onTap { pos => adapter.getItem(pos).generatePopup }
  itemsList setFooterDividersEnabled false
  itemsList addFooterView allTxsWrapper
  itemsList setAdapter adapter

  app.kit.wallet addCoinsSentEventListener itemsListListener
  app.kit.wallet addCoinsReceivedEventListener itemsListListener
  app.kit.wallet addTransactionConfidenceEventListener itemsListListener

  app.kit.peerGroup addConnectedEventListener constListener
  app.kit.peerGroup addDisconnectedEventListener constListener
  app.kit.peerGroup addBlocksDownloadedEventListener catchListener
  host.timer.schedule(adapter.notifyDataSetChanged, 10000, 10000)
  Utils clickableTextField frag.findViewById(R.id.mnemonicInfo)
  react(new String)

  <(fun = {
    val rawTxs = app.kit.wallet.getRecentTransactions(PaymentTable.limit, false)
    val wraps = for (txnj <- rawTxs.asScala.toVector) yield new TxWrap(txnj)
    btcItems = for (wrap <- wraps if wrap.isVisible) yield BTCWrap(wrap)
    updList(btcItems ++ lnItems).run
  }, onFail)(none)
}