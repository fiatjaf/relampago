package com.lightning.walletapp

import spray.json._
import android.view._
import android.widget._
import org.bitcoinj.core._
import collection.JavaConverters._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.R.drawable._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import java.util.{Date, TimerTask}
import android.os.{Bundle, Handler}
import scala.util.{Failure, Success, Try}
import android.database.{ContentObserver, Cursor}
import com.lightning.walletapp.ln.Tools.{none, random, runAnd}
import com.lightning.walletapp.helper.{ReactLoader, RichCursor}
import com.lightning.walletapp.lnutils.{PaymentTable, RatesSaver}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Satoshi}

import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
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
import android.net.Uri


class FragWallet extends Fragment { me =>
  override def onCreateView(inflator: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inflator.inflate(R.layout.frag_view_pager_btc, vg, false)

  override def onViewCreated(view: View, savedInstanceState: Bundle) =
    WalletActivity.worker = new FragWalletWorker(getActivity.asInstanceOf[WalletActivity], view)

  override def onResume = {
    // Save global reference to current worker
    WalletActivity.worker.onFragmentResume
    super.onResume
  }

  override def onDestroy = {
    // This may be nullified hence a try/catch enclosure
    try WalletActivity.worker.onFragmentDestroy catch none
    super.onDestroy
  }
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
  val statusConnecting = app getString btc_status_connecting
  val statusOnline = app getString btc_status_online
  val noDesc = app getString ln_no_description
  val btcEmpty = app getString btc_empty
  val lnEmpty = app getString ln_empty

  // UPDATING TITLE

  var currentPeerCount = 0
  var currentBlocksLeft = 0

  val catchListener = new BlocksListener {
    def onBlocksDownloaded(sourcePeerNode: Peer, block: Block, filteredBlock: FilteredBlock, left: Int) = {
      if (left > broadcaster.blocksPerDay) app.kit.peerGroup addBlocksDownloadedEventListener getNextTracker(left)
      app.kit.peerGroup removeBlocksDownloadedEventListener this
    }

    def getNextTracker(initBlocksLeft: Int) = new BlocksListener { self =>
      def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) =
        runAnd(currentBlocksLeft = left)(updTitle.run)
    }
  }

  val constListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(peer: Peer, peerCount: Int) = runAnd(currentPeerCount = peerCount)(updTitle.run)
    def onPeerConnected(peer: Peer, peerCount: Int) = runAnd(currentPeerCount = peerCount)(updTitle.run)
  }

  def updTitle = {
    val btcTotalSum = app.kit.conf1Balance
    val btcFunds = if (btcTotalSum.value < 1) btcEmpty else denom withSign btcTotalSum
    val lnTotalSum = app.ChannelManager.notClosingOrRefunding.map(estimateTotalCanSend).sum
    val lnFunds = if (lnTotalSum < 1) lnEmpty else denom withSign MilliSatoshi(lnTotalSum)
    val daysLeft = currentBlocksLeft / broadcaster.blocksPerDay

    val subtitleText =
      if (currentBlocksLeft > 6) app.plurOrZero(syncOps, daysLeft)
      else if (currentPeerCount < 1) statusConnecting
      else statusOnline

    val titleText = s"""
      <font color=#FF9900>&#3647;</font> <strong>$btcFunds</strong><br>
      <font color=#0099FE>&#9735;</font> <strong>$lnFunds</strong><br>
      $subtitleText
    """.html

    // Update all the info in one pass
    UITask(customTitle setText titleText)
  }

  // END UPDATING TITLE

  // SEARCH BAR

  var showLNDetails = false
  override def setupSearch(menu: Menu) = {
    // Expand payment list if search is active
    // hide payment description if it's not

    super.setupSearch(menu)
    searchView addOnAttachStateChangeListener new View.OnAttachStateChangeListener {
      def onViewDetachedFromWindow(arg: View) = runAnd(showLNDetails = false)(adapter.notifyDataSetChanged)
      def onViewAttachedToWindow(arg: View) = runAnd(showLNDetails = true)(adapter.notifyDataSetChanged)
    }
  }

  // END SEARCH BAR

  // DISPLAYING ITEMS LIST

  val minLinesNum = 4
  var currentCut = minLinesNum
  var items = Vector.empty[ItemWrap]

  val adapter = new BaseAdapter {
    def getCount = math.min(items.size, currentCut)
    def getItem(position: Int) = items(position)
    def getItemId(position: Int) = position

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val view = if (null == savedView) host.getLayoutInflater.inflate(R.layout.frag_tx_line, null) else savedView
      val holder = if (null == view.getTag) new ViewHolder(view) else view.getTag.asInstanceOf[ViewHolder]
      items(position) fillView holder
      view
    }
  }

  val itemsListListener = new TxTracker {
    override def coinsReceived(txj: Transaction) = guard(txj)
    override def coinsSent(txj: Transaction) = guard(txj)

    override def txConfirmed(txj: Transaction) = {
      // Update amounts in title, mark as confirmed
      UITask(adapter.notifyDataSetChanged).run
      runAnd(Vibr.vibrate)(updTitle.run)
    }

    def guard(txj: Transaction) = {
      val transactionWrap = new TxWrap(txj)
      if (!transactionWrap.valueDelta.isZero) {
        // Zero valueDelta means this tx is foreign
        val signleBTCWrap = BTCWrap(transactionWrap)
        addItems(Vector apply signleBTCWrap).run
      }
    }
  }

  val addItems: Vector[ItemWrap] => TimerTask = fresh => {
    val visibleItems = for (item <- fresh ++ items if item.canShow) yield item
    val distinctItems = visibleItems.groupBy(_.key).values.map(_.head).toVector
    items = distinctItems.sortBy(_.getDate)(Ordering[Date].reverse)

    UITask {
      allTxsWrapper setVisibility viewMap(items.size > minLinesNum)
      mnemonicWarn setVisibility viewMap(items.isEmpty)
      itemsList setVisibility viewMap(items.nonEmpty)
      adapter.notifyDataSetChanged
    }
  }

  def nativeBTCWraps = {
    val rawTransactions = app.kit.wallet.getRecentTransactions(PaymentTable.limit, false)
    val rawWraps = for (txj <- rawTransactions.asScala.toVector) yield new TxWrap(txj)
    for (wrap <- rawWraps if !wrap.valueDelta.isZero) yield BTCWrap(wrap)
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
    def canShow: Boolean
    def key: String
  }

  case class LNWrap(info: PaymentInfo) extends ItemWrap {
    def canShow: Boolean = info.status != HIDDEN
    val getDate: Date = new Date(info.stamp)
    val key = info.hash

    def fillView(holder: ViewHolder) = {
      val humanSum = info.incoming match {
        case 1 => sumIn.format(denom formatted info.firstSum)
        case 0 => sumOut.format(denom formatted info.firstSum)
      }

      val description = getDescription(info.description)
      val humanHash = humanFour(info.hash.toUpperCase take 24)
      val humanSumDetails = s"<font color=#999999>$humanHash</font><br>$description"
      holder.transactSum setText s"<font color=#0099FE>&#9735;</font> $humanSum".html
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactCircle setImageResource imageMap(info.actualStatus)
      holder.transactWhat setVisibility viewMap(showLNDetails)
      holder.transactWhat setText humanSumDetails.html
    }

    def generatePopup = {
      val rd = emptyRDFromInfo(info)
      val description = me getDescription info.description
      val humanStatus = s"<strong>${paymentStates apply info.actualStatus}</strong>"
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_ln_details, null)
      val paymentDetails = detailsWrapper.findViewById(R.id.paymentDetails).asInstanceOf[TextView]
      val paymentProof = detailsWrapper.findViewById(R.id.paymentProof).asInstanceOf[Button]
      val paymentHash = detailsWrapper.findViewById(R.id.paymentHash).asInstanceOf[Button]
      paymentHash setOnClickListener onButtonTap(host share rd.paymentHashString)

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
        // Different display specifics
        // which depend on payment direction

        case 1 =>
          val title = host getString ln_incoming_title format humanStatus
          val humanIn = humanFiat(coloredIn(info.firstSum), info.firstSum)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)
          paymentDetails setText s"$description<br><br>$humanIn".html

        case 0 =>
          val fee = MilliSatoshi(info.lastMsat - info.firstMsat)
          val humanOut = humanFiat(coloredOut(info.firstSum), info.firstSum)
          val expiry = app.plurOrZero(expiryLeft, info.lastExpiry - broadcaster.currentHeight)
          val title = humanFiat(app.getString(ln_outgoing_title).format(coloredOut(fee), humanStatus), fee)
          val title1 = if (info.actualStatus == WAITING) s"$expiry<br>$title" else title
          paymentDetails setText s"$description<br><br>$humanOut".html

          // Allow user to retry this payment using excluded nodes and channels when it is a failure and pr is not expired yet
          if (info.actualStatus != FAILURE || !info.pr.isFresh) showForm(negBuilder(dialog_ok, title1.html, detailsWrapper).create)
          else mkForm(doSend(rd), none, baseBuilder(title1.html, detailsWrapper), dialog_retry, dialog_cancel)
      }
    }
  }

  case class BTCWrap(wrap: TxWrap) extends ItemWrap {
    def canShow: Boolean = wrap.tx.getMemo != wrap.HIDE
    val getDate: Date = wrap.tx.getUpdateTime
    val key = wrap.tx.getHashAsString

    def fillView(holder: ViewHolder) = {
      val humanSum = wrap.visibleValue.isPositive match {
        case true => sumIn.format(denom formatted wrap.visibleValue)
        case false => sumOut.format(denom formatted -wrap.visibleValue)
      }

      val status = if (wrap.isDead) dead else if (wrap.depth >= minDepth) conf1 else await
      holder.transactSum setText s"<font color=#FF9900>&#3647;</font> $humanSum".html
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactCircle setImageResource status
    }

    def generatePopup = {
      val confs = app.plurOrZero(txsConfs, wrap.depth)
      val marking = if (wrap.visibleValue.isPositive) sumIn else sumOut
      val outputs = wrap.payDatas(wrap.visibleValue.isPositive).flatMap(_.toOption)
      val humanOutputs = for (paymentData <- outputs) yield paymentData.cute(marking).html
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val viewTxOutside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]
      val lst = host.getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
      val views = new ArrayAdapter(host, R.layout.frag_top_tip, R.id.actionTip, humanOutputs.toArray)

      lst setHeaderDividersEnabled false
      lst addHeaderView detailsWrapper
      lst setAdapter views

      viewTxOutside setOnClickListener onButtonTap {
        val uri = Uri parse s"https://smartbit.com.au/tx/$key"
        host startActivity new Intent(Intent.ACTION_VIEW, uri)
      }

      val header = wrap.fee match {
        case _ if wrap.isDead => sumOut format txsConfs.last
        case _ if wrap.visibleValue.isPositive => host getString txs_fee_incoming format confs
        case Some(fee) => humanFiat(app.getString(txs_fee_details).format(coloredOut(fee), confs), fee)
        case None => host getString txs_fee_absent format confs
      }

      // See if CPFP can be applied
      val notEnoughValue = wrap.valueDelta isLessThan RatesSaver.rates.feeSix
      val tooFresh = wrap.tx.getUpdateTime.getTime > System.currentTimeMillis - 1800L * 1000
      val doNotOfferCPFP = wrap.depth > 0 || wrap.isDead || tooFresh || notEnoughValue

      if (doNotOfferCPFP) showForm(negBuilder(dialog_ok, header.html, lst).create)
      else mkForm(none, boostIncoming(wrap), baseBuilder(header.html, lst), dialog_ok, dialog_boost)
    }
  }

  toggler setOnClickListener onFastTap {
    val newImg = if (currentCut == minLinesNum) ic_expand_less_black_24dp else ic_expand_more_black_24dp
    currentCut = if (currentCut == minLinesNum) items.size else minLinesNum
    toggler setImageResource newImg
    adapter.notifyDataSetChanged
  }

  // Load LN payments right away
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
      val consume = (infos: InfoVec) => addItems(infos map LNWrap).run
      def createItem(rCursor: RichCursor) = bag toPaymentInfo rCursor
      def getCursor = bag.byRecent
    }

    def searchPays = new ReactLoader[PaymentInfo](host) {
      val consume = (infos: InfoVec) => addItems(infos map LNWrap).run
      def createItem(rCursor: RichCursor) = bag toPaymentInfo rCursor
      def getCursor = bag byQuery lastQuery
    }

    def onCreateLoader(loaderId: Int, bundle: Bundle) = if (lastQuery.isEmpty) recentPays else searchPays
    me.react = vs => runAnd(lastQuery = vs)(getSupportLoaderManager.restartLoader(1, null, self).forceLoad)
    host.getContentResolver.registerContentObserver(db sqlPath PaymentTable.table, true, observer)
  }

  // Load recent Bitcoin transactions right away
  <(nativeBTCWraps, onFail)(wraps => addItems(wraps).run)

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
      case _ \ CMDReserveExcept(rpi, missingSat, reserveSat) =>

        val message = host getString err_ln_fee_overflow
        val reserve = coloredIn apply Satoshi(reserveSat)
        val missing = coloredOut apply Satoshi(missingSat)
        val sending = coloredOut apply MilliSatoshi(rpi.firstMsat)
        onFail(message.format(reserve, sending, missing).html)

      case _ \ CMDAddExcept(_, code) =>
        // Display detailed description
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

        val content = host.getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
        val hint = app.getString(amount_hint_can_receive).format(denom withSign maxCanReceive)
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

        val popupTitle = s"<font color=#0099FE>${app getString ln_receive_title}</font>".html
        mkCheckForm(recAttempt, none, baseBuilder(popupTitle, content), dialog_ok, dialog_cancel)
      }
    }
  }

  def sendPayment(pr: PaymentRequest) = ifOperational { operationalChannels =>
    if (PaymentRequest.prefixes(chainHash) != pr.prefix) app toast err_general
    else if (pr.nodeId == nodePublicKey) app toast err_general
    else if (!pr.isFresh) app toast dialog_pr_expired
    else {

      val title = app getString ln_send_title
      val description = me getDescription pr.description
      val maxCanSend = MilliSatoshi(operationalChannels.map(estimateCanSend).max)
      val popupTitle = s"<font color=#0099FE>$title</font><br><br><small>$description</small>"
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
          // Outgoing payment needs to have an amount
          // custom amount may be higher than requested
          me doSend emptyRD(pr, ms.amount)
        }
      }

      val bld = baseBuilder(popupTitle.html, content)
      mkCheckForm(sendAttempt, none, bld, dialog_pay, dialog_cancel)
      for (amountMsat <- pr.amount) rateManager setSum Try(amountMsat)
    }
  }

  def doSend(rd: RoutingData) = {
    val request = app.ChannelManager.withRoutesAndOnionRD(rd, useCache = true)
    def noRoutes(emptyRoutes: RoutingData) = onFail(host getString err_ln_no_route)
    request.foreach(foeRD => app.ChannelManager.sendEither(foeRD, noRoutes), onFail)
  }

  // END LN STUFF

  // BTC SEND AND BOOST

  def sendBtcPopup(addr: Address): RateManager = {
    val form = host.getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val hint = app.getString(amount_hint_can_send).format(denom withSign app.kit.conf1Balance)
    val bld = baseBuilder(s"<font color=#FF9900>${app getString btc_send_title}</font>".html, form)
    val addressData = form.findViewById(R.id.addressData).asInstanceOf[TextView]
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

  // INIT

  host setSupportActionBar toolbar
  toolbar setOnClickListener onFastTap { if (!showLNDetails) showDenomChooser }
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
}