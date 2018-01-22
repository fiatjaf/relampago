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
import com.lightning.wallet.Denomination._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._

import fr.castorflex.android.smoothprogressbar.SmoothProgressDrawable
import fr.castorflex.android.smoothprogressbar.SmoothProgressBar
import android.support.v7.widget.SearchView.OnQueryTextListener
import com.lightning.wallet.Denomination.sat2msatFactor
import android.content.Context.LAYOUT_INFLATER_SERVICE
import android.content.DialogInterface.BUTTON_POSITIVE
import org.ndeftools.util.activity.NfcReaderActivity
import com.lightning.wallet.lnutils.JsonHttpUtils.to
import com.lightning.wallet.ln.wire.ChannelUpdate
import android.support.v4.view.MenuItemCompat
import android.view.ViewGroup.LayoutParams
import com.lightning.wallet.Utils.app
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.core.Address
import scala.language.postfixOps
import org.ndeftools.Message
import android.os.Bundle
import java.util.Date

import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Satoshi}
import com.lightning.wallet.ln.Tools.{none, random, runAnd, wrap}
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import scala.util.{Failure, Success, Try}


trait SearchBar { me =>
  import android.support.v7.widget.SearchView
  protected[this] var searchItem: MenuItem = _
  protected[this] var search: SearchView = _

  private[this] val lst = new OnQueryTextListener {
    def onQueryTextSubmit(queryText: String) = true
    def onQueryTextChange(queryText: String) =
      runAnd(true)(me react queryText)
  }

  def react(query: String)
  def setupSearch(menu: Menu) = {
    searchItem = menu findItem R.id.action_search
    val view = MenuItemCompat getActionView searchItem
    search = view.asInstanceOf[SearchView]
    search setOnQueryTextListener lst
  }
}

trait DataReader extends NfcReaderActivity {
  def readEmptyNdefMessage = app toast nfc_error
  def readNonNdefMessage = app toast nfc_error
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none
  def checkTransData: Unit

  def readNdefMessage(msg: Message) = try {
    val asText = readFirstTextNdefMessage(msg)
    app.TransData recordValue asText
    checkTransData

  } catch { case _: Throwable =>
    // Could not process a message
    app toast nfc_error
  }
}

class LNActivity extends DataReader with ToolbarActivity with ListUpdater with SearchBar { me =>
  lazy val layoutInflater = app.getSystemService(LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
  lazy val container = findViewById(R.id.container).asInstanceOf[RelativeLayout]
  lazy val lnChanInfo = Utils clickableTextField findViewById(R.id.lnChanInfo)
  lazy val viewChannelInfo = findViewById(R.id.viewChannelInfo)
  lazy val openNewChannel = findViewById(R.id.openNewChannel)
  lazy val actionDivider = findViewById(R.id.actionDivider)

  lazy val viewParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT)
  lazy val paymentStatesMap = getResources getStringArray R.array.ln_payment_states
  lazy val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  lazy val paymentsViewProvider = new PaymentsViewProvider
  lazy val imgMap = Array(await, await, conf1, dead)

  lazy val adapter = new CutAdapter[PaymentInfo](PaymentTable.limit, R.layout.frag_tx_ln_line) {
    // LN line has a narrower timestamp section because payment info, also limit of rows is reduced
    // which is fine because unlike Bitcoin all the LN payments can always be found via search

    def getItem(position: Int) = visibleItems(position)
    def getHolder(view: View) = new TxViewHolder(view) {

      def fillView(info: PaymentInfo) = {
        val timestamp = new Date(info.stamp)
        val markedPaymentSum = info.incoming match {
          case 1 => sumIn.format(denom formatted info.firstSum)
          case _ => sumOut.format(denom formatted -info.firstSum)
        }

        transactWhen setText when(System.currentTimeMillis, timestamp).html
        transactSum setText s"$markedPaymentSum\u00A0${info.text}".html
        transactCircle setImageResource imgMap(info.actualStatus)
      }
    }
  }

  val chanListener = new ChannelListener { self =>
    // Updates local UI according to changes in channel
    // Should always be removed when activity is stopped

    override def onError = {
      case _ \ CMDReserveExcept(CMDPlainAddHtlc(rpi), missingSat, reserveSat) =>
        // Current commit tx fee and channel reserve forbid sending of this payment
        // Inform user with all the details laid out as cleanly as possible

        val message = me getString err_ln_fee_overflow
        val reserve = coloredIn apply Satoshi(reserveSat)
        val missing = coloredOut apply Satoshi(missingSat)
        val sending = coloredOut apply MilliSatoshi(rpi.firstMsat)
        onFail(message.format(reserve, sending, missing).html)

      case _ \ CMDAddExcept(_: CMDPlainAddHtlc, code) =>
        // Let user know why payment could not be added
        onFail(me getString code)
    }

    override def onBecome = {
      case (_, _, SYNC | WAIT_FUNDING_DONE | NORMAL, _) => me runOnUiThread reWireChannel
      case (_, _, null, SYNC) => update(me getString ln_notify_connecting, Informer.LNSTATE).flash.run
      case (_, _, null, NORMAL) => update(me getString ln_notify_operational, Informer.LNSTATE).flash.run
    }
  }

  var makePaymentRequest: Runnable = _
  var sendPayment: PaymentRequest => Unit = _
  def react(qs: String) = paymentsViewProvider reload qs
  def notifySubTitle(subtitle: String, infoType: Int) = {
    // Title will updated separately so just update subtitle
    timer.schedule(delete(infoType), 8000)
    add(subtitle, infoType).flash.run
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    // Set action bar, main view content, wire up list events, update title later
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_ln)
    add(me getString ln_notify_none, Informer.LNSTATE)
    wrap(me setDetecting true)(me initNfc state)
    me startListUpdates adapter

    list setOnItemClickListener onTap { pos =>
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
          val serialized = PaymentRequest write info.pr
          app setBuffer getString(ln_proof).format(serialized,
            info.pr.hash.toString, info.preimage.toString)
        }
      }

      if (info.incoming == 1) {
        val title = getString(ln_incoming_title).format(humanStatus)
        val humanIn = humanFiat(prefix = coloredIn(info.firstSum), info.firstSum)
        paymentDetails setText s"${me getDescription info.pr}<br><br>$humanIn".html

        // Can only show a QR again if this is not a success yet AND payment request has not expired yet
        if (info.actualStatus == SUCCESS || !info.pr.isFresh) mkForm(me negBld dialog_ok, title.html, detailsWrapper)
        else mkForm(mkChoiceDialog(none, qr(info.pr), dialog_ok, dialog_retry), title.html, detailsWrapper)

      } else {
        val feeAmount = MilliSatoshi(info.rd.lastMsat - info.firstMsat)
        val humanOut = humanFiat(prefix = coloredOut(info.firstSum), info.firstSum)
        paymentDetails setText s"${me getDescription info.pr}<br><br>$humanOut".html

        // Will show title with expiry if payment is in-flight so user can make estimations
        val expiryHuman = app.plurOrZero(blocksLeft, info.rd.lastExpiry - broadcaster.currentHeight)
        val title1 = humanFiat(getString(ln_outgoing_title).format(coloredOut(feeAmount), humanStatus), feeAmount)
        val title2 = if (info.actualStatus == WAITING) s"${me getString ln_expiry} $expiryHuman<br>$title1" else title1
        mkForm(me negBld dialog_ok, title2.html, detailsWrapper)
      }
    }

    list setAdapter adapter
    list setFooterDividersEnabled false
    paymentsViewProvider reload new String
    app.kit.wallet addCoinsSentEventListener txTracker
    app.kit.wallet addCoinsReceivedEventListener txTracker
  } else me exitTo classOf[MainActivity]

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_normal_ops, menu)
    setupSearch(menu)
    true
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeCoinsSentEventListener txTracker
    app.kit.wallet removeCoinsReceivedEventListener txTracker
    stopDetecting
  }

  override def onStop = wrap(super.onStop) {
    // We can not show a progress bar anymore so discard it
    // We should remove a listener, just iterate over all of them
    app.ChannelManager.getOutPaymentObs = app.ChannelManager.outPaymentObs
    for (chan <- app.ChannelManager.all) chan.listeners -= chanListener
  }

  override def onResume = wrap(super.onResume) {
    app.prefs.edit.putBoolean(AbstractKit.LANDING_LN, true).commit
    // Searches for suitable channel and wires it up accordingly
    reWireChannel

    app.ChannelManager.getOutPaymentObs = rpi => {
      // Stopping animation immediately does not work sometimes so we use a guarded timer here once again
      // On entering this activity we show a progress bar animation, a call may happen outside of this activity
      val progressBar = layoutInflater.inflate(R.layout.frag_progress_bar, null).asInstanceOf[SmoothProgressBar]
      def delayedStop = try timer.schedule(anyToRunnable(progressBar.progressiveStop), 250) catch none
      val drawable = progressBar.getIndeterminateDrawable.asInstanceOf[SmoothProgressDrawable]

      drawable setCallbacks new SmoothProgressDrawable.Callbacks {
        // In some cases timer may already be disabled and will throw here because activity has been killed
        def onStop = try timer.schedule(anyToRunnable(container removeView progressBar), 250) catch none
        def onStart = try drawable.setColors(getResources getIntArray R.array.bar_colors) catch none
      }

      me runOnUiThread container.addView(progressBar, viewParams)
      app.ChannelManager.outPaymentObs(rpi).doOnTerminate(delayedStop)
    }
  }

  override def onOptionsItemSelected(menu: MenuItem) = {
    if (menu.getItemId == R.id.actionSettings) mkSetsForm
    true
  }

  def qr(pr: PaymentRequest) = {
    me goTo classOf[RequestActivity]
    app.TransData.value = pr
  }

  def getBalance(chan: Channel) = MilliSatoshi {
    chan(_.localCommit.spec.toLocalMsat) getOrElse 0L
  }

  def updDenom(chan: Channel) = showDenominationChooser { pos =>
    wrap(adapter.notifyDataSetChanged) { denom = denoms apply pos }
    app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
    me setTitle denom.withSign(me getBalance chan)
  }

  def reWireChannel: Unit =
    app.ChannelManager.all.find(_.isOperational) map whenOpen getOrElse {
      app.ChannelManager.all.find(_.isOpening) map whenOpening getOrElse whenNone
    }

  def whenOpen(chan: Channel) = {
    makePaymentRequest = anyToRunnable {
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
            val alert = mkForm(negPosBld(dialog_cancel, dialog_ok), me getString ln_receive, content)
            val canReceiveHint = if (maxMsat.amount < 0L) coloredOut(maxMsat) else denom withSign maxMsat
            val rateManager = new RateManager(getString(amount_hint_can_receive).format(canReceiveHint), content)

            def makeRequest(sum: MilliSatoshi, r: BinaryData) = {
              val extraRoute = Vector(update toHop chan.data.announce.nodeId)
              val rpi = emptyRPI apply PaymentRequest(chainHash, Some(sum), Crypto sha256 r,
                nodePrivateKey, inputDescription.getText.toString.trim, None, extraRoute)

              qr(rpi.pr)
              // Save incoming request to a database
              db.change(PaymentTable.newVirtualSql, rpi.searchText, rpi.pr.paymentHash)
              db.change(PaymentTable.newSql, rpi.pr.paymentHash, r, 1, rpi.firstMsat, HIDDEN,
                System.currentTimeMillis, rpi.text, rpi.pr.toJson, rpi.rd.toJson)
            }

            def recAttempt = rateManager.result match {
              case Failure(_) => app toast dialog_sum_empty
              case Success(ms) if maxMsat < ms => app toast dialog_sum_big
              case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small

              case Success(ms) => rm(alert) {
                notifySubTitle(me getString ln_pr_make, Informer.LNPAYMENT)
                <(makeRequest(ms, random getBytes 32), onFail)(none)
              }
            }

            val ok = alert getButton BUTTON_POSITIVE
            ok setOnClickListener onButtonTap(recAttempt)

          case _ =>
            // Peer node has not sent us a ChannelUpdate
            // most likely because a funding tx is not yet deep enough
            mkForm(me negBld dialog_ok, me getString err_ln_cant_ask, null)
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
            case None => onFail(me getString err_ln_no_route)
          }, onFail)
        }
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(sendAttempt)
      for (sum <- pr.amount) rateManager setSum Try(sum)
    }

    // Reload to update subtitle from listener
    toolbar setOnClickListener onButtonTap(me updDenom chan)
    me setTitle denom.withSign(me getBalance chan)
    chan.listeners += chanListener
    chanListener nullOnBecome chan
    checkTransData

    // Update interface buttons
    viewChannelInfo setVisibility View.GONE
    openNewChannel setVisibility View.GONE
    actionDivider setVisibility View.GONE
  }

  def whenOpening(chan: Channel) = {
    // Reset all triggers except denomination
    sendPayment = pr => app toast ln_notify_opening
    makePaymentRequest = anyToRunnable(app toast ln_notify_opening)
    toolbar setOnClickListener onButtonTap(me updDenom chan)

    // Set title to current balance and notify channel is being opened
    update(me getString ln_notify_opening, Informer.LNSTATE).flash.run
    me setTitle denom.withSign(me getBalance chan)
    chan.listeners += chanListener

    // Update interface buttons
    viewChannelInfo setVisibility View.VISIBLE
    actionDivider setVisibility View.VISIBLE
    openNewChannel setVisibility View.GONE
  }

  def whenNone = {
    // Reset all triggers to toasts
    sendPayment = pr => app toast ln_notify_none
    makePaymentRequest = anyToRunnable(app toast ln_notify_none)
    toolbar setOnClickListener onButtonTap(app toast ln_notify_none)

    // Set base title and notify there is no channel in a subtitle
    update(me getString ln_notify_none, Informer.LNSTATE).flash.run
    setTitle(me getString ln_wallet)

    // Update interface buttons
    viewChannelInfo setVisibility View.GONE
    actionDivider setVisibility View.VISIBLE
    openNewChannel setVisibility View.VISIBLE
  }

  // DATA READING AND BUTTON ACTIONS

  def checkTransData = app.TransData.value match {
    case uri: BitcoinURI => me goTo classOf[BtcActivity]
    case adr: Address => me goTo classOf[BtcActivity]

    case pr: PaymentRequest =>
      if (pr.isFresh) sendPayment(pr)
      else onFail(me getString err_ln_old)
      app.TransData.value = null

    case otherwise =>
      app.TransData.value = null
  }

  def goReceive(top: View) = makePaymentRequest.run
  def goQR(top: View) = me goTo classOf[ScanActivity]
  def goBitcoin(top: View) = me goTo classOf[BtcActivity]
  def goLNOps(top: View) = me goTo classOf[LNOpsActivity]

  def goLNStart(top: View) = {
    val minRequired = RatesSaver.rates.feeLive multiply 2
    lazy val required = sumIn.format(denom withSign minRequired)
    lazy val balance = sumIn.format(denom withSign app.kit.conf1Balance)
    lazy val notEnough = getString(err_ln_not_enough_funds).format(balance, required)
    if (app.kit.conf1Balance isGreaterThan minRequired) me exitTo classOf[LNStartActivity]
    else mkForm(me negBld dialog_ok, notEnough.html, null)
  }

  def toggle(v: View) = {
    // Expand or collapse all txs
    // adapter contains all history

    adapter.switch
    adapter set adapter.availableItems
    adapter.notifyDataSetChanged
  }

  // Payment history and search results loader
  class PaymentsViewProvider extends ReactCallback(me) { self =>
    def onCreateLoader(id: Int, bundle: Bundle) = if (lastQuery.isEmpty) recentPays else searchPays
    def reload(txt: String) = runAnd(lastQuery = txt)(getSupportLoaderManager.restartLoader(1, null, self).forceLoad)
    val observeTablePath = db sqlPath PaymentTable.table
    var lastQuery = new String

    type InfoVec = Vector[PaymentInfo]
    def searchPays = new ReactLoader[PaymentInfo](me) {
      val consume = (vec: InfoVec) => me runOnUiThread update(vec)
      def createItem(rc: RichCursor) = bag toPaymentInfo rc
      def getCursor = bag.byQuery(lastQuery)

      def update(pays: InfoVec) = {
        wrap(adapter.notifyDataSetChanged)(adapter set pays)
        if (pays.nonEmpty) lnChanInfo setVisibility View.GONE
        if (pays.nonEmpty) list setVisibility View.VISIBLE
      }
    }

    def recentPays = new ReactLoader[PaymentInfo](me) {
      val consume = (vec: InfoVec) => me runOnUiThread update(vec)
      def createItem(rc: RichCursor) = bag toPaymentInfo rc
      def getCursor = bag.byRecent

      def update(pays: InfoVec) = {
        wrap(adapter.notifyDataSetChanged)(adapter set pays)
        if (pays.isEmpty) lnChanInfo setVisibility View.VISIBLE
        if (pays.isEmpty) list setVisibility View.GONE
      }
    }
  }

  def getDescription(pr: PaymentRequest) = pr.description match {
    case Right(description) if description.nonEmpty => description take 140
    case Left(descriptionHash) => s"<i>${descriptionHash.toString}</i>"
    case _ => s"<i>${me getString ln_no_description}</i>"
  }
}