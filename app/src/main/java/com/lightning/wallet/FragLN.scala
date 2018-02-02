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
import com.lightning.wallet.ln.Tools.{none, random, wrap, runAnd}
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Satoshi}
import scala.util.{Failure, Success, Try}

import com.lightning.wallet.Denomination.sat2msatFactor
import android.content.DialogInterface.BUTTON_POSITIVE
import com.lightning.wallet.lnutils.JsonHttpUtils.to
import com.lightning.wallet.ln.wire.ChannelUpdate
import android.support.v7.widget.Toolbar
import android.support.v4.app.Fragment
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.Crypto.sha256
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
    // Save itself in registry
    WalletActivity.frags += me
    worker.reWireChannelOrNone.run
    // Inversively call parent check
    worker.host.checkTransData
    app.TransData.value = null
    super.onResume
  }

  override def onDestroy = {
    WalletActivity.frags -= me
    worker.onFragmentDestroy.run
    super.onDestroy
  }
}

class FragLNWorker(val host: WalletActivity, frag: View) extends ListUpdater with ToolbarFragment with SearchBar { me =>
  import host.{getResources, rm, getString, onFail, UITask, getSupportLoaderManager, str2View, timer, getLayoutInflater, onTap}
  import host.{onButtonTap, <, mkForm, negBld, negPosBld, mkChoiceDialog}

  val toolbar = frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  val paymentStatesMap = getResources getStringArray R.array.ln_payment_states
  val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  val imageMap = Array(await, await, conf1, dead)

  val itemsList = frag.findViewById(R.id.itemsList).asInstanceOf[ListView]
  val viewChannelInfo = frag.findViewById(R.id.viewChannelInfo)
  val openNewChannel = frag.findViewById(R.id.openNewChannel)
  val actionDivider = frag.findViewById(R.id.actionDivider)
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

  val chanListener = new ChannelListener { self =>
    // Updates local UI according to changes in a channel
    // should always be removed when activity is stopped

    override def onError = {
      case _ \ CMDReserveExcept(CMDPlainAddHtlc(rpi), missingSat, reserveSat) =>
        // Current commit tx fee and channel reserve forbid sending of this payment
        // inform user with all the details laid out as cleanly as possible

        val message = getString(err_ln_fee_overflow)
        val reserve = coloredIn apply Satoshi(reserveSat)
        val missing = coloredOut apply Satoshi(missingSat)
        val sending = coloredOut apply MilliSatoshi(rpi.firstMsat)
        onFail(message.format(reserve, sending, missing).html)

      case _ \ CMDAddExcept(_: CMDPlainAddHtlc, code) =>
        // Let user know why payment could not be added
        onFail(host getString code)
    }

    override def onBecome = {
      case (_, _, SYNC | WAIT_FUNDING_DONE | OPEN, _) => reWireChannelOrNone.run
      case (_, _, null, SYNC) => update(host getString ln_notify_connecting, Informer.LNSTATE).run
      case (_, _, null, OPEN) => update(host getString ln_notify_operational, Informer.LNSTATE).run
    }
  }

  val subtitleListener = new TxTracker {
    override def coinsSent(tx: Transaction) = notifyBtcEvent(host getString tx_sent)
    override def coinsReceived(tx: Transaction) = notifyBtcEvent(host getString tx_received)
  }

  val onFragmentDestroy = UITask {
    app.kit.wallet removeCoinsSentEventListener subtitleListener
    app.kit.wallet removeCoinsReceivedEventListener subtitleListener
    for (chan <- app.ChannelManager.all) chan.listeners -= chanListener
  }

  val reWireChannelOrNone: Runnable = UITask {
    app.ChannelManager.all.find(_.isOperational) map whenOpen getOrElse {
      app.ChannelManager.all.find(_.isOpening) map whenOpening getOrElse whenNone
    }
  }

  var sendPayment: PaymentRequest => Unit = none
  var makePaymentRequest: Runnable = UITask(none)

  def whenOpen(chan: Channel) = {
    // Make all triggers operational
    makePaymentRequest = host UITask {
      // Somewhat counterintuitive: localParams.channelReserveSat is THEIR unspendable reseve
      // peer's balance can't go below their unspendable channel reserve so it should be taken into account here
      val canReceive = chan(c => c.localCommit.spec.toRemoteMsat - c.localParams.channelReserveSat * sat2msatFactor)
      val maxMsat = MilliSatoshi apply math.min(canReceive getOrElse 0L, maxHtlcValue.amount)

      chan(_.channelId.toString) foreach { chanIdKey =>
        StorageWrap get chanIdKey map to[ChannelUpdate] match {
          // Can issue requests if a saved ChannelUpdate is present

          case Success(update) =>
            val content = getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
            val inputDescription = content.findViewById(R.id.inputDescription).asInstanceOf[EditText]
            val canReceiveHint = if (maxMsat.amount < 0L) coloredOut(maxMsat) else denom withSign maxMsat
            val rateManager = new RateManager(getString(amount_hint_can_receive).format(canReceiveHint), content)
            val alert = mkForm(negPosBld(dialog_cancel, dialog_ok), getString(ln_receive), content)

            def makeRequest(sum: MilliSatoshi, preimage: BinaryData) = {
              val extraRoute = Vector(update toHop chan.data.announce.nodeId)
              val rpi = emptyRPI apply PaymentRequest(chainHash, Some(sum), sha256(preimage),
                nodePrivateKey, inputDescription.getText.toString.trim, None, extraRoute)

              qr(rpi.pr)
              // Save incoming request to a database
              db.change(PaymentTable.newVirtualSql, rpi.searchText, rpi.pr.paymentHash)
              db.change(PaymentTable.newSql, rpi.pr.paymentHash, preimage, 1, rpi.firstMsat,
                HIDDEN, System.currentTimeMillis, rpi.text, rpi.pr.toJson, rpi.rd.toJson)
            }

            def recAttempt = rateManager.result match {
              case Failure(_) => app toast dialog_sum_empty
              case Success(ms) if maxMsat < ms => app toast dialog_sum_big
              case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small

              case Success(ms) => rm(alert) {
                // Requests without amount are not allowed for now
                <(makeRequest(ms, random getBytes 32), onFail)(none)
                app toast ln_pr_make
              }
            }

            val ok = alert getButton BUTTON_POSITIVE
            ok setOnClickListener onButtonTap(recAttempt)

          case _ =>
            // Peer node has not sent us a ChannelUpdate
            // likely because a funding transaction is not yet deep enough
            mkForm(negBld(dialog_ok), getString(err_ln_cant_ask), null)
        }
      }
    }

    sendPayment = pr => {
      // Somewhat counterintuitive: remoteParams.channelReserveSatoshis is OUR unspendable reseve
      // we can not calculate an exact commitTx fee + HTLC fees in advance so just use a minChannelMargin
      // which also guarantees a user has some substantial amount to be refunded once a channel is nearly exhausted
      val canSend0 = chan(c => c.localCommit.spec.toLocalMsat - c.remoteParams.channelReserveSatoshis * sat2msatFactor)
      val canSend1 = canSend0.map(_ - RatesSaver.rates.feeLive.value * sat2msatFactor / 2) getOrElse 0L
      val maxMsat = MilliSatoshi apply math.min(canSend1, maxHtlcValue.amount)

      val title = getString(ln_send_title).format(me getDescription pr)
      val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
      val alert = mkForm(negPosBld(dialog_cancel, dialog_pay), title.html, content)
      val hint = getString(amount_hint_can_send).format(denom withSign maxMsat)
      val rateManager = new RateManager(hint, content)
      host.walletPager.setCurrentItem(1, true)

      def sendAttempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if maxMsat < ms => app toast dialog_sum_big
        case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small
        case Success(ms) if pr.amount.exists(_ * 2 < ms) => app toast dialog_sum_big
        case Success(ms) if pr.amount.exists(_ > ms) => app toast dialog_sum_small

        case Success(ms) => rm(alert) {
          // Outgoing payment needs to have an amount
          // and this amount may be higher than requested
          val rpi = RuntimePaymentInfo(emptyRD, pr, ms.amount)
          app.ChannelManager.getOutPaymentObs(rpi).foreach(onNext = {
            // Must account for a special fail when no routes are found
            case Some(updatedRPI) => chan process CMDPlainAddHtlc(updatedRPI)
            case None => onFail(host getString err_ln_no_route)
          }, onFail)
        }
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(sendAttempt)
      for (sum <- pr.amount) rateManager setSum Try(sum)
    }

    // Reload listener to get subtitle updated
    me setTitle denom.withSign(me getBalance chan)
    chan.listeners += chanListener
    chanListener nullOnBecome chan

    // Update interface buttons
    viewChannelInfo setVisibility View.GONE
    openNewChannel setVisibility View.GONE
    actionDivider setVisibility View.GONE
  }

  def whenOpening(chan: Channel) = {
    // Reset all triggers except denomination
    sendPayment = pr => app toast ln_notify_opening
    makePaymentRequest = UITask(app toast ln_notify_opening)
    update(getString(ln_notify_opening), Informer.LNSTATE).run
    me setTitle denom.withSign(me getBalance chan)

    // Broadcast a funding tx
    chan.listeners += chanListener
    broadcaster nullOnBecome chan

    // Update interface buttons
    viewChannelInfo setVisibility View.VISIBLE
    actionDivider setVisibility View.VISIBLE
    openNewChannel setVisibility View.GONE
  }

  def whenNone = {
    // Reset all triggers
    sendPayment = pr => app toast ln_notify_none
    makePaymentRequest = UITask(app toast ln_notify_none)
    update(getString(ln_notify_none), Informer.LNSTATE).run
    me setTitle getString(ln_wallet)

    // Update interface buttons
    viewChannelInfo setVisibility View.GONE
    actionDivider setVisibility View.VISIBLE
    openNewChannel setVisibility View.VISIBLE
  }

  def getBalance(chan: Channel) = MilliSatoshi {
    chan(_.localCommit.spec.toLocalMsat) getOrElse 0L
  }

  def notifyBtcEvent(message: String) = {
    // In LN we only temporairly update a subtitle
    timer.schedule(delete(Informer.BTCEVENT), 8000)
    add(message, Informer.BTCEVENT).run
  }

  def qr(pr: PaymentRequest) = {
    host goTo classOf[RequestActivity]
    app.TransData.value = pr
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
    }.run

    def recentPays = new ReactLoader[PaymentInfo](host) {
      val consume = (pays: InfoVec) => updView(pays, pays.isEmpty)
      def createItem(rCursor: RichCursor) = bag toPaymentInfo rCursor
      def getCursor = bag.byRecent
    }

    def searchPays = new ReactLoader[PaymentInfo](host) {
      val consume = (pays: InfoVec) => updView(pays, showText = false)
      def createItem(rCursor: RichCursor) = bag toPaymentInfo rCursor
      def getCursor = bag byQuery lastQuery
    }

    // Recent payment history and paymenr search combined
    me.react = vs => runAnd(lastQuery = vs)(getSupportLoaderManager.restartLoader(1, null, self).forceLoad)
    def onCreateLoader(id: Int, bundle: Bundle) = if (lastQuery.isEmpty) recentPays else searchPays
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
        val serializedPaymentRequest = PaymentRequest write info.pr
        app setBuffer getString(ln_proof).format(serializedPaymentRequest,
          info.pr.hash.toString, info.preimage.toString)
      }
    }

    if (info.incoming == 1) {
      val title = getString(ln_incoming_title).format(humanStatus)
      val humanIn = humanFiat(prefix = coloredIn(info.firstSum), info.firstSum)
      paymentDetails setText s"${me getDescription info.pr}<br><br>$humanIn".html

      // Can show a QR again if this is not a success yet AND payment request has not expired yet
      if (info.actualStatus == SUCCESS || !info.pr.isFresh) mkForm(negBld(dialog_ok), title.html, detailsWrapper)
      else mkForm(mkChoiceDialog(none, qr(info.pr), dialog_ok, dialog_retry), title.html, detailsWrapper)

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

  toggler setOnClickListener onButtonTap {
    // Expand and collapse available LN payments

    adapter.switch
    adapter set adapter.availableItems
    adapter.notifyDataSetChanged
  }

  itemsList setAdapter adapter
  itemsList setFooterDividersEnabled false
  startListUpdates(itemsList, adapter)

  // LN page toolbar is going to be WalletActicity action bar
  toolbar setOnClickListener host.onButtonTap(host.showDenomChooser)
  add(getString(ln_notify_none), Informer.LNSTATE).run
  host setSupportActionBar toolbar

  app.kit.wallet addCoinsSentEventListener subtitleListener
  app.kit.wallet addCoinsReceivedEventListener subtitleListener
  Utils clickableTextField frag.findViewById(R.id.lnChanInfo)
  me.react(new String)
}