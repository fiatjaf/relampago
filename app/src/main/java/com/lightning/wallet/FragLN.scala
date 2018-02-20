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
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.ImplicitConversions._

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
    worker.onFragmentDestroy
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

  val paymentStatesMap = getResources getStringArray R.array.ln_payment_states
  val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  val viewMap = Map(true -> View.VISIBLE, false -> View.GONE)
  val imageMap = Array(await, await, conf1, dead, frozen)
  val lnChanWarn = frag.findViewById(R.id.lnChanWarn)

  val adapter = new CutAdapter[PaymentInfo](PaymentTable.limit, R.layout.frag_tx_ln_line) {
    // LN line has smaller timestamps because payment info, also limit of rows is reduced
    // which is fine because unlike Bitcoin all the LN payments can be found via search

    def getItem(position: Int) = visibleItems(position)
    def getHolder(view: View) = new TxViewHolder(view) {

      def fillView(info: PaymentInfo) = {
        val timestamp = new Date(info.stamp)
        val marker = if (info.incoming == 1) sumIn else sumOut
        val markedPaymentSum = marker.format(denom formatted info.firstSum)
        transactWhen setText when(System.currentTimeMillis, timestamp).html
        transactSum setText s"$markedPaymentSum\u00A0${info.text}".html
        transactCircle setImageResource imageMap(info.actualStatus)
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
      case _ => updTitleSubtitleAbdButtons
    }
  }

  val subtitleListener = new TxTracker {
    override def coinsSent(tx: Transaction) = notifyBtcEvent(host getString btc_sent)
    override def coinsReceived(tx: Transaction) = notifyBtcEvent(host getString btc_received)
  }

  def onFragmentResume = {
    app.kit.wallet addCoinsSentEventListener subtitleListener
    app.kit.wallet addCoinsReceivedEventListener subtitleListener
    for (chan <- app.ChannelManager.all) chan.listeners += chanListener
    wrap(host.checkTransData)(updTitleSubtitleAbdButtons)
  }

  def onFragmentDestroy = {
    app.kit.wallet removeCoinsSentEventListener subtitleListener
    app.kit.wallet removeCoinsReceivedEventListener subtitleListener
    for (chan <- app.ChannelManager.all) chan.listeners -= chanListener
  }

  def updTitleSubtitleAbdButtons = {
    val activeChannels = app.ChannelManager.notClosingOrRefunding
    val onlineChannels = activeChannels.count(_.state != Channel.OFFLINE)
    val openingChannelExist = activeChannels exists isOpening
    val funds = activeChannels.map(myBalanceMsat).sum
    val total = activeChannels.size

    val subtitle =
      if (total == 0) getString(ln_status_none)
      else if (onlineChannels == 0) getString(ln_status_connecting)
      else if (onlineChannels == total) getString(ln_status_online)
      else getString(ln_status_mix).format(onlineChannels, total)

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

  def sendPayment(pr: PaymentRequest) = ifOperational { operational =>
    if (pr.isFresh) withFreshPaymentRequest else app toast ln_status_pr_expired
    host.walletPager.setCurrentItem(1, false)

    def withFreshPaymentRequest = {
      val maxCanSend = MilliSatoshi(operational.map(estimateCanSend).max)
      val popupTitle = getString(ln_send_title).format(me getDescription pr)
      val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
      val alert = mkForm(negPosBld(dialog_cancel, dialog_pay), popupTitle.html, content)
      val hint = getString(amount_hint_can_send).format(denom withSign maxCanSend)
      val rateManager = new RateManager(hint, content)

      def sendAttempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if maxCanSend < ms => app toast dialog_sum_big
        case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small
        case Success(ms) if pr.amount.exists(_ * 2 < ms) => app toast dialog_sum_big
        case Success(ms) if pr.amount.exists(_ > ms) => app toast dialog_sum_small

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
      request1.foreach(foeRPI => app.ChannelManager.sendEither(foeRPI, none), onFail)
    }

  def makePaymentRequest = ifOperational { operational =>
    val goodChannels \ extraRoutes = operational.flatMap(StorageWrap.getUpd).unzip
    if (extraRoutes.isEmpty) mkForm(negBld(dialog_ok), getString(err_ln_6_confs), null)
    else withUpdates(goodChannels.map(estimateCanReceive).max)

    def withUpdates(maxReceive: Long) = {
      val maxCanReceive = MilliSatoshi(maxReceive)
      val reserveUnspent = getString(err_ln_reserve) format coloredOut(-maxCanReceive)
      if (maxCanReceive.amount < 0L) mkForm(negBld(dialog_ok), reserveUnspent.html, null)
      else withEnoughReserve(maxCanReceive)
    }

    def withEnoughReserve(maxReceive: MilliSatoshi) = {
      val content = getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
      val inputDescription = content.findViewById(R.id.inputDescription).asInstanceOf[EditText]
      val alert = mkForm(negPosBld(dialog_cancel, dialog_ok), getString(action_ln_receive), content)
      val hint = getString(amount_hint_can_receive).format(denom withSign maxReceive)
      val rateManager = new RateManager(hint, content)

      def makeRequest(requestedSum: MilliSatoshi, preimage: BinaryData) = {
        val rpi = RuntimePaymentInfo(emptyRD, PaymentRequest(chainHash, Some(requestedSum),
          Crypto sha256 preimage, nodePrivateKey, inputDescription.getText.toString.trim,
          None, extraRoutes), requestedSum.amount)

        db.change(PaymentTable.newVirtualSql, rpi.searchText, rpi.paymentHashString)
        db.change(PaymentTable.newSql, rpi.paymentHashString, preimage, 1, rpi.firstMsat,
          HIDDEN, System.currentTimeMillis.toString, rpi.text, rpi.pr.toJson, rpi.rd.toJson)

        // Show to user
        showQR(rpi.pr)
      }

      def recAttempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if maxReceive < ms => app toast dialog_sum_big
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

  def getDescription(pr: PaymentRequest) = pr.description match {
    case Right(description) if description.nonEmpty => description take 140
    case Left(descriptionHash) => s"<i>${descriptionHash.toString}</i>"
    case _ => s"<i>${host getString ln_no_description}</i>"
  }

  // INIT

  new ReactCallback(host) { self =>
    val observeTablePath = db sqlPath PaymentTable.table
    private[this] var lastQuery = new String
    type InfoVec = Vector[PaymentInfo]

    def updView(pays: InfoVec, showText: Boolean) = UITask {
      val textVisibility = if (showText) View.VISIBLE else View.GONE
      val listVisibility = if (showText) View.GONE else View.VISIBLE
      wrap(adapter.notifyDataSetChanged)(adapter set pays)
      lnChanWarn setVisibility textVisibility
      itemsList setVisibility listVisibility
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
      val humanIn = humanFiat(prefix = coloredIn(info.firstSum), info.firstSum)
      paymentDetails setText s"${me getDescription info.pr}<br><br>$humanIn".html
      // Can show a QR again if this is not a success yet AND payment request has not expired yet
      if (info.actualStatus == SUCCESS || !info.pr.isFresh) mkForm(negBld(dialog_ok), title.html, detailsWrapper)
      else mkForm(mkChoiceDialog(none, showQR(info.pr), dialog_ok, dialog_retry), title.html, detailsWrapper)

    } else {
      val feeAmount = MilliSatoshi(info.rd.lastMsat - info.firstMsat)
      val humanOut = humanFiat(prefix = coloredOut(info.firstSum), info.firstSum)
      paymentDetails setText s"${me getDescription info.pr}<br><br>$humanOut".html

      // Will show title with expiry if payment is in-flight so user can make estimations
      val expiryHuman = app.plurOrZero(blocksLeft, info.rd.lastExpiry - broadcaster.currentHeight)
      val title = humanFiat(getString(ln_outgoing_title).format(coloredOut(feeAmount), humanStatus), feeAmount)
      val title1 = if (info.actualStatus == WAITING) s"${host getString ln_expiry} $expiryHuman<br>$title" else title
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

  // LN page toolbar will be an action bar
  add(getString(ln_status_none), Informer.LNSTATE).run
  Utils clickableTextField frag.findViewById(R.id.lnChanInfo)
  toolbar setOnClickListener onFastTap(host.showDenomChooser)
  host setSupportActionBar toolbar
  react(new String)
}