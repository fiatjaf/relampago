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

import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Satoshi}
import com.lightning.wallet.ln.wire.{ChannelUpdate, CommitSig}
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import com.lightning.wallet.ln.Tools.{none, random}
import com.lightning.wallet.ln.Tools.{runAnd, wrap}
import scala.util.{Failure, Success, Try}

import fr.castorflex.android.smoothprogressbar.SmoothProgressDrawable
import fr.castorflex.android.smoothprogressbar.SmoothProgressBar
import android.support.v7.widget.SearchView.OnQueryTextListener
import com.lightning.wallet.Denomination.sat2msatFactor
import android.content.Context.LAYOUT_INFLATER_SERVICE
import android.content.DialogInterface.BUTTON_POSITIVE
import org.ndeftools.util.activity.NfcReaderActivity
import com.lightning.wallet.lnutils.JsonHttpUtils.to
import android.support.v4.view.MenuItemCompat
import android.view.ViewGroup.LayoutParams
import android.app.AlertDialog.Builder
import com.lightning.wallet.Utils.app
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.core.Address
import scala.language.postfixOps
import android.app.AlertDialog
import org.ndeftools.Message
import android.os.Bundle
import java.util.Date


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
  lazy val viewParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT)
  lazy val container = findViewById(R.id.container).asInstanceOf[RelativeLayout]

  lazy val paymentStatesMap = getResources getStringArray R.array.ln_payment_states
  lazy val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  lazy val paymentsViewProvider = new PaymentsViewProvider
  val imgMap = Array(await, await, conf1, dead)

  lazy val adapter = new CutAdapter[PaymentInfo](PaymentTable.limit, R.layout.frag_tx_ln_line) {
    // LN line has a narrower timestamp section because payment info, also limit of rows is reduced

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

  val chanListener = new ChannelListener {
    // Updates local UI according to changes in channel
    // Should be removed when activity is stopped

    override def onError = {
      case _ \ CMDReserveExcept(CMDPlainAddHtlc(rpi), missingSat, reserveSat) =>
        // Current commit tx fee and channel reserve forbid sending of this payment
        // Inform user with all the details laid out as cleanly as possible

        val message = me getString err_ln_fee_overflow
        val reserve = coloredIn apply Satoshi(reserveSat)
        val missing = coloredOut apply Satoshi(missingSat)
        val sending = coloredOut apply MilliSatoshi(rpi.firstMsat)
        onFail(error = message.format(reserve, sending, missing).html)

      case _ \ CMDAddExcept(_: CMDPlainAddHtlc, code) =>
        // Let user know why payment could not be added
        onFail(me getString code)
    }

    override def onBecome = {
      case (chan, _, _, _) if !chan.isOperational => evacuate
      case (_, _: NormalData, _, SYNC) => update(me getString ln_notify_connecting, Informer.LNSTATE).flash.run
      case (_, _: NormalData, _, NORMAL) => update(me getString ln_notify_operational, Informer.LNSTATE).flash.run
    }

    override def onProcess = {
      case (chan, norm: NormalData, _: CommitSig)
        // GUARD: Update UI once we have some fulfilled HTLCs
        if norm.commitments.localCommit.spec.fulfilled.nonEmpty =>
        notifySubTitle(me getString ln_done, Informer.LNPAYMENT)
        me updTitle chan
    }
  }

  private[this] var sendPayment: PaymentRequest => Unit = none
  private[this] var makePaymentRequest = anyToRunnable(none)
  private[this] var whenStop = anyToRunnable(super.onStop)
  private[this] var getChannelInfo = anyToRunnable(none)
  override def onStop = whenStop.run

  def evacuate = me exitTo classOf[LNOpsActivity]
  def react(qs: String) = paymentsViewProvider reload qs
  def notifySubTitle(subtitle: String, infoType: Int) = {
    // Title will updated separately so just update subtitle
    timer.schedule(delete(infoType), 8000)
    add(subtitle, infoType).flash.run
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    // Set action bar, main view content, wire up list events, update title later
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_ln)
    add(me getString ln_notify_connecting, Informer.LNSTATE)
    wrap(me setDetecting true)(me initNfc state)
    me startListUpdates adapter

    list setAdapter adapter
    list setFooterDividersEnabled false
    paymentsViewProvider reload new String
    app.kit.wallet addCoinsSentEventListener txTracker
    app.kit.wallet addCoinsReceivedEventListener txTracker
  } else me exitTo classOf[MainActivity]

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeCoinsSentEventListener txTracker
    app.kit.wallet removeCoinsReceivedEventListener txTracker
    stopDetecting
  }

  override def onCreateOptionsMenu(menu: Menu) = runAnd(true) {
    getMenuInflater.inflate(R.menu.ln_normal_ops, menu)
    setupSearch(menu)
  }

  override def onResume = wrap(super.onResume) {
    app.prefs.edit.putBoolean(AbstractKit.LANDING_LN, true).commit
    val chanOpt = app.ChannelManager.all.find(_.isOperational)
    chanOpt map manageActive getOrElse evacuate
  }

  // APP MENU

  override def onOptionsItemSelected(menu: MenuItem) = runAnd(true) {
    if (menu.getItemId == R.id.actionCloseChannel) getChannelInfo.run
    else if (menu.getItemId == R.id.actionSettings) mkSetsForm
  }

  def getBalance(chan: Channel): MilliSatoshi = {
    val canSend = chan(_.localCommit.spec.toLocalMsat)
    MilliSatoshi(canSend getOrElse 0L)
  }

  def updTitle(chan: Channel) = animateTitle {
    // A channel always has some balance everytime
    denom withSign getBalance(chan)
  }

  def onPaymentError(er: Throwable) = er.getMessage match {
    case FROMBLACKLISTED => onFail(me getString err_ln_black)
    case techDetails => onFail(techDetails)
  }

  def expiryTitle(expiry: Long, title: String) = {
    val relativeLeft = expiry - broadcaster.currentHeight
    val leftHuman = app.plurOrZero(blocksLeft, relativeLeft)
    s"${me getString ln_expiry} $leftHuman<br>$title"
  }

  def qr(pr: PaymentRequest) = {
    me goTo classOf[RequestActivity]
    app.TransData.value = pr
  }

  def manageActive(chan: Channel) = {
    val pay: RuntimePaymentInfo => Unit = rpi =>
      app.ChannelManager.getOutPaymentObs(rpi).foreach(onNext = {
        // Must account for a special fail when no routes are found
        case Some(updatedRPI) => chan process CMDPlainAddHtlc(updatedRPI)
        case None => onFail(me getString err_ln_no_route)
      }, onPaymentError)

    toolbar setOnClickListener onButtonTap {
      showDenominationChooser(me getBalance chan) { pos =>
        app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit

        denom = denoms apply pos
        adapter.notifyDataSetChanged
        updTitle(chan)
      }
    }

    list setOnItemClickListener onTap { pos =>
      val info: PaymentInfo = adapter getItem pos
      val description = me getDescription info.pr
      val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_tx_ln_details, null)
      val paymentDetails = detailsWrapper.findViewById(R.id.paymentDetails).asInstanceOf[TextView]
      val paymentProof = detailsWrapper.findViewById(R.id.paymentProof).asInstanceOf[Button]
      val paymentHash = detailsWrapper.findViewById(R.id.paymentHash).asInstanceOf[Button]
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
        val humanIn = humanFiat(coloredIn(info.firstSum), info.firstSum)
        val canRetryQR = info.actualStatus != SUCCESS && info.pr.isFresh
        val bld = mkChoiceDialog(none, qr(info.pr), dialog_ok, dialog_retry)
        val bld1 = if (canRetryQR) bld else me negBld dialog_ok

        // Can show a QR again if payment request has not expired yet
        paymentDetails setText s"$description<br><br>$humanIn".html
        mkForm(bld1, title.html, detailsWrapper)
      } else {
        val feeAmount = MilliSatoshi(info.rd.lastMsat - info.firstMsat)
        val humanOut = humanFiat(coloredOut(info.firstSum), info.firstSum)
        val bld = mkChoiceDialog(none, pay(info.runtime), dialog_ok, dialog_retry)
        val bld1 = if (info.actualStatus != SUCCESS) bld else me negBld dialog_ok

        // Will show title with expiry countdown if payment is in-flight
        val title = humanFiat(getString(ln_outgoing_title).format(coloredOut(feeAmount), humanStatus), feeAmount)
        val title1 = if (info.actualStatus == WAITING) expiryTitle(info.rd.lastExpiry, title) else title

        // Can send this payment again if it's not a success yet
        paymentDetails setText s"$description<br><br>$humanOut".html
        mkForm(bld1, title1.html, detailsWrapper)
      }
    }

    sendPayment = pr => {
      // Somewhat counterintuitive: remoteParams.channelReserveSatoshis is OUR unspendable reseve
      // we can not calculate an exact commitTx fee + HTLC fees in advance so just use a minChannelMargin
      // which also guarantees a user has some substantial amount to be refunded once a channel is exhausted
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
          pay apply emptyRPI(pr).copy(firstMsat = ms.amount)
        }
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(sendAttempt)
      for (sum <- pr.amount) rateManager setSum Try(sum)
    }

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
            // Peer has not yet sent us a ChannelUpdate
            // most likely because a funding transaction is not deep enough
            new Builder(me).setCustomTitle(me getString err_ln_cant_ask).show
        }
      }
    }

    whenStop = anyToRunnable {
      // On leaving this activity we can not show a progress bar anymore
      // so we must replace it with a basic task of just asking for routes
      app.ChannelManager.getOutPaymentObs = app.ChannelManager.outPaymentObs
      chan.listeners -= chanListener
      super.onStop
    }

    getChannelInfo = anyToRunnable {
      val (view, field) = str2Tuple(getString(ln_chan_info_details).format(chan.data.announce.alias,
        chan.data.announce.addresses.headOption.map(_.getHostString).orNull, nodePrivateKey.publicKey.toString,
        chan.data.announce.nodeId.toString, chan(_.channelId).get.toString).html)

      // Show channel and peer details and ofer to cooperatively close a given channel
      lazy val dialog = mkChoiceDialog(none, rm(alert)(proceed), dialog_ok, ln_chan_close)
      lazy val alert: AlertDialog = mkForm(dialog, null, view)
      field setTextIsSelectable true
      alert

      def proceed = passWrap(me getString ln_chan_close_details) apply checkPassNotify { _ =>
        // attempt to close all of the channels in case we have more than one operational left
        for (chan <- app.ChannelManager.notClosing) chan process CMDShutdown
      }
    }

    app.ChannelManager.getOutPaymentObs = rpi => {
      // On entering this activity we show a progress bar animation, a call may happen outside of this activity
      val progressBar = layoutInflater.inflate(R.layout.frag_progress_bar, null).asInstanceOf[SmoothProgressBar]
      // Stopping animation immediately does not work sometimes so we use a guarded timer here once again
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

    chan.listeners += chanListener
    chanListener reloadOnBecome chan
    me updTitle chan
    checkTransData
  }

  // DATA READING AND BUTTON ACTIONS

  def checkTransData = app.TransData.value match {
    case uri: BitcoinURI => me goTo classOf[BtcActivity]
    case adr: Address => me goTo classOf[BtcActivity]

    case pr: PaymentRequest if pr.isFresh =>
      app.TransData.value = null
      sendPayment(pr)

    case pr: PaymentRequest =>
      onFail(me getString err_ln_old)
      app.TransData.value = null

    case otherwise =>
      app.TransData.value = null
  }

  def goReceive(top: View) = makePaymentRequest.run
  def goQR(top: View) = me goTo classOf[ScanActivity]
  def goBitcoin(top: View) = me goTo classOf[BtcActivity]

  def toggle(v: View) = {
    // Expand or collapse all txs
    // adapter contains all history

    adapter.switch
    adapter set adapter.availableItems
    adapter.notifyDataSetChanged
  }

  // Payment history and search results loader
  class PaymentsViewProvider extends ReactCallback(me) { self =>
    def onCreateLoader(id: Int, bundle: Bundle) = if (lastQuery.isEmpty) recent else search
    def search = new ExtendedPaymentInfoLoader { def getCursor = bag byQuery lastQuery }
    def recent = new ExtendedPaymentInfoLoader { def getCursor = bag byRecent }
    val observeTablePath = db sqlPath PaymentTable.table
    var lastQuery = new String

    def reload(txt: String) = runAnd(lastQuery = txt) {
      // Remember last search term to handle possible reload
      getSupportLoaderManager.restartLoader(1, null, self).forceLoad
    }

    abstract class ExtendedPaymentInfoLoader extends ReactLoader[PaymentInfo](me) {
      val consume: InfoVec => Unit = payments => me runOnUiThread updatePaymentList(payments)
      def updatePaymentList(pays: InfoVec) = wrap(adapter.notifyDataSetChanged)(adapter set pays)
      def createItem(shifted: RichCursor) = bag toPaymentInfo shifted
      type InfoVec = Vector[PaymentInfo]
    }
  }

  def getDescription(pr: PaymentRequest) = pr.description match {
    case Right(description) if description.nonEmpty => description take 140
    case Left(descriptionHash) => s"<i>${descriptionHash.toString}</i>"
    case _ => s"<i>${me getString ln_no_description}</i>"
  }
}