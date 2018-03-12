package com.lightning.wallet

import spray.json._
import android.view._
import android.widget._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.helper._
import com.lightning.wallet.lnutils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._

import scala.util.{Failure, Success, Try}
import com.lightning.wallet.ln.Tools.{none, random, runAnd, wrap}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Satoshi}
import com.lightning.wallet.R.drawable.{await, conf1, dead, frozen}
import android.content.DialogInterface.BUTTON_POSITIVE
import com.lightning.wallet.ln.PaymentRequest.write
import android.support.v7.widget.Toolbar
import com.lightning.wallet.ln.Channel
import android.support.v4.app.Fragment
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Transaction
import android.os.Bundle
import java.util.Date


class FragLN extends Fragment { me =>
  override def onCreateView(inflator: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inflator.inflate(R.layout.frag_view_pager_ln, vg, false)

  var worker: FragLNWorker = _
  override def onViewCreated(view: View, savedInstanceState: Bundle) =
    worker = new FragLNWorker(getActivity.asInstanceOf[WalletActivity], view)

  override def onResume = {
    WalletActivity.frags += me
    worker.onFragmentResume
    super.onResume
  }

  override def onDestroy = {
    WalletActivity.frags -= me
    // This may be nullified hence a null check
    if (worker != null) worker.onFragmentDestroy
    super.onDestroy
  }
}

class FragLNWorker(val host: WalletActivity, frag: View) extends ListToggler with ToolbarFragment with SearchBar { me =>
  import host.{getResources, getString, onFail, UITask, getSupportLoaderManager, str2View, timer, getLayoutInflater, onTap}
  import host.{rm, <, onButtonTap, onFastTap, mkForm, negBld, negPosBld, mkChoiceDialog}

  val itemsList = frag.findViewById(R.id.itemsList).asInstanceOf[ListView]
  val toolbar = frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  val lnChanDetails = frag.findViewById(R.id.lnChanDetails)
  val lnAddChannel = frag.findViewById(R.id.lnAddChannel)
  val lnDivider = frag.findViewById(R.id.lnDivider)

  val lnStatus = getResources getStringArray R.array.ln_status_online
  val paymentStatesMap = getResources getStringArray R.array.ln_payment_states
  val expiryLeft = getResources getStringArray R.array.ln_status_expiry
  val imageMap = Array(await, await, conf1, dead, frozen)
  val lnChanWarn = frag.findViewById(R.id.lnChanWarn)
  val noDesc = host getString ln_no_description

  var showDescription = false
  val adapter = new CutAdapter[PaymentInfo](PaymentTable.limit, R.layout.frag_tx_ln_line) {
    // LN line has smaller timestamps because payment info, also limit of rows is reduced
    // which is fine because unlike Bitcoin all the LN payments can be found via search

    def getItem(position: Int) = visibleItems(position)
    def getHolder(paymentView: View) = new TxViewHolder(paymentView) {
      val transactWhat = paymentView.findViewById(R.id.transactWhat).asInstanceOf[TextView]

      def fillView(info: PaymentInfo) = {
        val timestamp = new Date(info.stamp)
        val markedPayment = info.incoming match {
          case 1 => sumIn.format(denom formatted info.firstSum)
          case _ => sumOut.format(denom formatted -info.firstSum)
        }

        val fastId = humanFour(info.hash.toUpperCase take 8)
        transactSum setText s"$markedPayment <font color=#999999>$fastId</font>".html
        transactWhen setText when(System.currentTimeMillis, timestamp).html
        transactCircle setImageResource imageMap(info.actualStatus)
        transactWhat setText getDescription(info.description).html
        transactWhat setVisibility viewMap(showDescription)
      }
    }
  }

  val chanListener = new ChannelListener {
    // Should be removed on activity destroyed

    override def onError = {
      // Commit tx fee + channel reserve forbid sending of this payment
      // inform user with all the details laid out as cleanly as possible
      case _ \ CMDReserveExcept(rpi, missingSat, reserveSat) =>

        val message = getString(err_ln_fee_overflow)
        val reserve = coloredIn apply Satoshi(reserveSat)
        val missing = coloredOut apply Satoshi(missingSat)
        val sending = coloredOut apply MilliSatoshi(rpi.firstMsat)
        onFail(message.format(reserve, sending, missing).html)

      case _ \ CMDAddExcept(_, code) =>
        // Display detailed description
        onFail(host getString code)
    }

    override def onBecome = {
      // Updates local UI on every state change
      case _ => updTitleSubtitleAndButtons
    }
  }

  val subtitleListener = new TxTracker {
    override def coinsSent(tx: Transaction) = notifyBtcEvent(host getString btc_sent)
    override def coinsReceived(tx: Transaction) = notifyBtcEvent(host getString btc_received)
  }

  override def setupSearch(menu: Menu) = {
    // Expand payment list if search is active
    // hide payment description if it's not

    super.setupSearch(menu)
    searchView addOnAttachStateChangeListener new View.OnAttachStateChangeListener {
      def onViewDetachedFromWindow(arg: View) = runAnd(showDescription = false)(adapter.notifyDataSetChanged)
      def onViewAttachedToWindow(arg: View) = runAnd(showDescription = true)(adapter.notifyDataSetChanged)
    }
  }

  def getDescription(txt: String) =
    if (txt.isEmpty) s"<i>$noDesc</i>"
    else txt take 140

  def onFragmentResume = {
    for (chan <- app.ChannelManager.all) chan.listeners += chanListener
    // We may have opening channels here so get their funding tx broadcasted
    for (chan <- app.ChannelManager.notClosing) broadcaster nullOnBecome chan
    wrap(host.checkTransData)(updTitleSubtitleAndButtons)
  }

  def onFragmentDestroy = {
    app.kit.wallet removeCoinsSentEventListener subtitleListener
    app.kit.wallet removeCoinsReceivedEventListener subtitleListener
    for (chan <- app.ChannelManager.all) chan.listeners -= chanListener
  }

  def updTitleSubtitleAndButtons = {
    val activeChannels = app.ChannelManager.notClosingOrRefunding
    val online = activeChannels.count(_.state != Channel.OFFLINE)
    val openingChannelExist = activeChannels exists isOpening
    val funds = activeChannels.map(estimateTotalCanSend).sum
    val total = activeChannels.size

    val subtitle =
      if (total == 0) getString(ln_status_none)
      else if (online == 0) app.plurOrZero(lnStatus, online)
      else if (online == total) app.plurOrZero(lnStatus, online)
      else lnStatus.last.format(online, total)

    val title =
      if (funds == 0L) getString(ln_wallet)
      else denom withSign MilliSatoshi(funds)

    val updateWarningAndTitle = UITask {
      lnDivider setVisibility viewMap(total == 0 || openingChannelExist)
      lnChanDetails setVisibility viewMap(openingChannelExist)
      lnAddChannel setVisibility viewMap(total == 0)
      me setTitle title
    }

    update(subtitle, Informer.LNSTATE).run
    updateWarningAndTitle.run
  }

  def notifyBtcEvent(message: String) = {
    timer.schedule(delete(Informer.BTCEVENT), 8000)
    add(text = message, Informer.BTCEVENT).run
  }

  def showQR(pr: PaymentRequest) = {
    host goTo classOf[RequestActivity]
    app.TransData.value = pr
  }

  def ifOperational(next: Vector[Channel] => Unit) = {
    val operational = app.ChannelManager.notClosingOrRefunding filter isOperational
    if (operational.isEmpty) app toast ln_status_none else next(operational)
  }

  def sendPayment(pr: PaymentRequest) = ifOperational { operationalChannels =>
    // We only can proceed if an operational (normal and online) channel exists,
    // this is not a payment to itself and this payment request has not expired
    if (pr.nodeId == nodePublicKey) app toast err_general
    else if (!pr.isFresh) app toast dialog_pr_expired
    else {

      val maxCanSend = MilliSatoshi(operationalChannels.map(estimateCanSend).max)
      val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
      val popupTitle = getString(ln_send_title).format(me getDescription pr.description)
      val alert = mkForm(negPosBld(dialog_cancel, dialog_pay), popupTitle.html, content)
      val hint = getString(amount_hint_can_send).format(denom withSign maxCanSend)
      val rateManager = new RateManager(hint, content)

      def sendAttempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if maxCanSend < ms => app toast dialog_sum_big
        case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small
        case Success(ms) if pr.amount.exists(_ * 2 < ms) => app toast dialog_sum_big
        case Success(ms) if pr.amount.exists(_ > ms) => app toast dialog_sum_small
        case _ if !broadcaster.bestHeightObtained => app toast dialog_chain_behind

        case Success(ms) => rm(alert) {
          // Outgoing payment needs to have an amount
          // and this amount may be higher than requested
          val rpi = RuntimePaymentInfo(emptyRD, pr, ms.amount)
          me doSend rpi
        }
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(sendAttempt)
      for (sum <- pr.amount) rateManager setSum Try(sum)
    }
  }

  def doSend(rpi: RuntimePaymentInfo) =
    host.placeLoader match { case indeterminateLoader =>
      val request = app.ChannelManager.withRoutesAndOnionRPI(rpi)
      val request1 = request.doOnTerminate(host removeLoader indeterminateLoader)
      def noRoutes(rpi: RuntimePaymentInfo) = onFail(host getString err_ln_no_route)
      request1.foreach(app.ChannelManager.sendEither(_, noRoutes), onFail)
    }

  def makePaymentRequest = ifOperational { operationalChannels =>
    val chansWithRoutes = operationalChannels.flatMap(channelAndHop).toMap
    // Lazy because chansWithRoutes may contain no channels in which case max would throw
    lazy val maxCanReceive = MilliSatoshi(chansWithRoutes.keys.map(estimateCanReceive).max)
    lazy val reserveUnspent = getString(err_ln_reserve) format coloredOut(-maxCanReceive)

    if (chansWithRoutes.isEmpty) mkForm(negBld(dialog_ok), getString(err_ln_6_confs), null)
    else if (maxCanReceive.amount < 0L) mkForm(negBld(dialog_ok), reserveUnspent.html, null)
    else {

      val content = getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
      val inputDescription = content.findViewById(R.id.inputDescription).asInstanceOf[EditText]
      val alert = mkForm(negPosBld(dialog_cancel, dialog_ok), getString(action_ln_receive), content)
      val hint = getString(amount_hint_can_receive).format(denom withSign maxCanReceive)
      val rateManager = new RateManager(hint, content)

      def makeRequest(sum: MilliSatoshi, preimage: BinaryData) = {
        val routes = chansWithRoutes.filterKeys(chan => estimateCanReceive(chan) >= sum.amount).values.toVector
        val rpi = RuntimePaymentInfo(emptyRD, PaymentRequest(chainHash, Some(sum), Crypto sha256 preimage,
          nodePrivateKey, inputDescription.getText.toString.trim, None, routes), sum.amount)

        db.change(PaymentTable.newVirtualSql, rpi.searchText, rpi.paymentHashString)
        db.change(PaymentTable.newSql, rpi.paymentHashString, preimage, 1, rpi.firstMsat,
          HIDDEN, System.currentTimeMillis, rpi.pr.description, rpi.pr.toJson, rpi.rd.toJson)

        showQR(rpi.pr)
      }

      def recAttempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if maxCanReceive < ms => app toast dialog_sum_big
        case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small

        case Success(ms) => rm(alert) {
          // Requests without amount are not allowed for now
          <(makeRequest(ms, random getBytes 32), onFail)(none)
          app toast dialog_pr_making
        }
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(recAttempt)
    }
  }

  // INIT

  new ReactCallback(host) { self =>
    val observeTablePath = db sqlPath PaymentTable.table
    private[this] var lastQuery = new String
    type InfoVec = Vector[PaymentInfo]

    def updView(pays: InfoVec, showText: Boolean) = UITask {
      wrap(adapter.notifyDataSetChanged)(adapter set pays)
      lnChanWarn setVisibility viewMap(showText)
      itemsList setVisibility viewMap(!showText)
    }

    def recentPays = new ReactLoader[PaymentInfo](host) {
      val consume = (pays: InfoVec) => updView(pays, pays.isEmpty).run
      def createItem(rCursor: RichCursor) = bag toPaymentInfo rCursor
      def getCursor = bag.byRecent
    }

    def searchPays = new ReactLoader[PaymentInfo](host) {
      val consume = (pays: InfoVec) => updView(pays, showText = false).run
      def createItem(rCursor: RichCursor) = bag toPaymentInfo rCursor
      def getCursor = bag byQuery lastQuery
    }

    // Recent payment history and paymenr search combined
    me.react = vs => runAnd(lastQuery = vs)(getSupportLoaderManager.restartLoader(1, null, self).forceLoad)
    def onCreateLoader(loaderId: Int, bundle: Bundle) = if (lastQuery.isEmpty) recentPays else searchPays
  }

  itemsList setOnItemClickListener onTap { pos =>
    val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_tx_ln_details, null)
    val paymentDetails = detailsWrapper.findViewById(R.id.paymentDetails).asInstanceOf[TextView]
    val paymentProof = detailsWrapper.findViewById(R.id.paymentProof).asInstanceOf[Button]
    val paymentHash = detailsWrapper.findViewById(R.id.paymentHash).asInstanceOf[Button]

    val info = adapter getItem pos
    val description = me getDescription info.description
    val rpi = RuntimePaymentInfo(info.rd, info.pr, info.firstMsat)
    val humanStatus = s"<strong>${paymentStatesMap apply info.actualStatus}</strong>"
    paymentHash setOnClickListener onButtonTap(host share rpi.paymentHashString)

    if (info.actualStatus == SUCCESS) {
      paymentHash setVisibility View.GONE
      paymentProof setVisibility View.VISIBLE
      paymentProof setOnClickListener onButtonTap {
        host share getString(ln_proof).format(write(info.pr),
          rpi.paymentHashString, info.preimage.toString)
      }
    }

    if (info.incoming == 1) {
      val title = getString(ln_incoming_title).format(humanStatus)
      val humanIn = humanFiat(coloredIn(info.firstSum), info.firstSum)
      paymentDetails setText s"$description<br><br>$humanIn".html
      mkForm(negBld(dialog_ok), title.html, detailsWrapper)

    } else {
      val fee = MilliSatoshi(info.rd.lastMsat - info.firstMsat)
      val humanOut = humanFiat(coloredOut(info.firstSum), info.firstSum)
      paymentDetails setText s"$description<br><br>$humanOut".html

      // Will show title with expiry if payment is in-flight so user can make estimations
      val expiry = app.plurOrZero(expiryLeft, info.rd.lastExpiry - broadcaster.currentHeight)
      val title = humanFiat(getString(ln_outgoing_title).format(coloredOut(fee), humanStatus), fee)
      val title1 = if (info.actualStatus == WAITING) s"$expiry<br>$title" else title

      // Allow user to retry this payment using excluded nodes and channels if it is a failure and pr is not expired
      if (info.actualStatus != FAILURE || !info.pr.isFresh) mkForm(negBld(dialog_ok), title1.html, detailsWrapper)
      else mkForm(mkChoiceDialog(none, doSend(rpi), dialog_ok, dialog_retry), title1.html, detailsWrapper)
    }
  }

  toggler setOnClickListener onFastTap {
    // Expand and collapse lightning payments

    adapter.switch
    adapter set adapter.availableItems
    adapter.notifyDataSetChanged
  }

  itemsList setAdapter adapter
  itemsList addFooterView allTxsWrapper
  itemsList setFooterDividersEnabled false
  itemsList setOnScrollListener host.listListener
  app.kit.wallet addCoinsSentEventListener subtitleListener
  app.kit.wallet addCoinsReceivedEventListener subtitleListener

  toolbar setOnClickListener onFastTap {
    // Only display chooser if search is off
    if (!showDescription) host.showDenomChooser
  }

  // LN page toolbar will be an action bar
  add(getString(ln_status_none), Informer.LNSTATE).run
  Utils clickableTextField frag.findViewById(R.id.lnChanInfo)
  host setSupportActionBar toolbar
  react(new String)
}