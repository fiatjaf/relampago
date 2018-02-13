package com.lightning.wallet

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

import fr.acinq.bitcoin.{MilliSatoshi, Satoshi}
import com.lightning.wallet.ln.Tools.{none, wrap, runAnd}
import com.lightning.wallet.R.drawable.{await, conf1, dead}
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
  import host.{onButtonTap, onFastTap, mkForm, negBld, mkChoiceDialog}

  val paymentStatesMap = getResources getStringArray R.array.ln_payment_states
  val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  val itemsList = frag.findViewById(R.id.itemsList).asInstanceOf[ListView]
  val toolbar = frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  val lnChanWarn = frag.findViewById(R.id.lnChanWarn)
  val imageMap = Array(await, await, conf1, dead)

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
    // Updates local UI according to changes in a channel
    // should always be removed when activity is stopped

    override def onError = {
      // Commit tx fee + channel reserve forbid sending of this payment
      // inform user with all the details laid out as cleanly as possible
      case _ \ CMDReserveExcept(rpi, missingSat, reserveSat) =>

        val message = getString(err_ln_fee_overflow)
        val reserve = coloredIn apply Satoshi(reserveSat)
        val missing = coloredOut apply Satoshi(missingSat)
        val sending = coloredOut apply MilliSatoshi(rpi.firstMsat)
        onFail(message.format(reserve, sending, missing).html)

      // Show detailed description to user
      case _ \ CMDAddExcept(_, code) =>
        onFail(host getString code)
    }

    override def onBecome = {
      // Update toolbar every time
      case _ => updTitleAndSubtitle
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
    wrap(host.checkTransData)(updTitleAndSubtitle)
  }

  def onFragmentDestroy = {
    app.kit.wallet removeCoinsSentEventListener subtitleListener
    app.kit.wallet removeCoinsReceivedEventListener subtitleListener
    for (chan <- app.ChannelManager.all) chan.listeners -= chanListener
  }

  def updTitleAndSubtitle = {
    val totalChannels = app.ChannelManager.notClosingOrRefunding
    val online = totalChannels.count(_.state != Channel.OFFLINE)
    val funds = totalChannels.map(myBalanceMsat).sum
    val total = totalChannels.size

    val subtitle =
      if (total == 0) getString(ln_notify_none)
      else if (online == 0) getString(notify_connecting)
      else if (online == total) getString(ln_status_online)
      else getString(ln_status_mix).format(online, total)

    val title =
      if (funds == 0L) getString(ln_wallet)
      else denom withSign MilliSatoshi(funds)

    update(subtitle, Informer.LNSTATE).run
    UITask(me setTitle title).run
  }

  def notifyBtcEvent(message: String) = {
    timer.schedule(delete(Informer.BTCEVENT), 8000)
    add(text = message, Informer.BTCEVENT).run
  }

  def showQR(pr: PaymentRequest) = {
    host goTo classOf[RequestActivity]
    app.TransData.value = pr
  }

  def getDescription(pr: PaymentRequest) = pr.description match {
    case Right(description) if description.nonEmpty => description take 140
    case Left(descriptionHash) => s"<i>${descriptionHash.toString}</i>"
    case _ => s"<i>${host getString ln_no_description}</i>"
  }

  def makePaymentRequest = {

  }

  def sendPayment(pr: PaymentRequest) = {

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
    val humanStatus = s"<strong>${paymentStatesMap apply info.actualStatus}</strong>"
    paymentHash setOnClickListener onButtonTap(app setBuffer info.pr.hash.toString)

    if (info.actualStatus == SUCCESS) {
      paymentHash setVisibility View.GONE
      paymentProof setVisibility View.VISIBLE
      paymentProof setOnClickListener onButtonTap {
        app setBuffer getString(ln_proof).format(write(info.pr),
          info.pr.hash.toString, info.preimage.toString)
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
      val title1 = humanFiat(getString(ln_outgoing_title).format(coloredOut(feeAmount), humanStatus), feeAmount)
      val title2 = if (info.actualStatus == WAITING) s"${host getString ln_expiry} $expiryHuman<br>$title1" else title1
      mkForm(negBld(dialog_ok), title2.html, detailsWrapper)
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
  add(getString(ln_notify_none), Informer.LNSTATE).run
  Utils clickableTextField frag.findViewById(R.id.lnChanInfo)
  toolbar setOnClickListener onFastTap(host.showDenomChooser)
  host setSupportActionBar toolbar
  react(new String)
}