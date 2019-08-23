package com.lightning.walletapp

import android.view._
import android.widget._
import com.lightning.walletapp.ln._
import android.text.format.DateUtils._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.ln.NormalChannel._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import scala.util.{Success, Try}
import android.app.{Activity, AlertDialog}
import com.lightning.walletapp.lnutils.{GDrive, PaymentInfoWrap}
import com.lightning.walletapp.lnutils.JsonHttpUtils.{queue, to}
import com.lightning.walletapp.lnutils.IconGetter.{bigFont, scrWidth}
import com.lightning.walletapp.ln.wire.{LightningMessage, NodeAnnouncement, OpenChannel}

import io.github.douglasjunior.androidSimpleTooltip.SimpleTooltip
import android.support.v4.app.FragmentStatePagerAdapter
import org.ndeftools.util.activity.NfcReaderActivity
import com.lightning.walletapp.helper.AwaitService
import android.support.v4.content.ContextCompat
import com.github.clans.fab.FloatingActionMenu
import android.support.v7.widget.SearchView
import fr.acinq.bitcoin.Crypto.PublicKey
import android.text.format.DateFormat
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat
import scodec.bits.ByteVector
import android.content.Intent
import org.ndeftools.Message
import android.os.Bundle
import java.util.Date


trait SearchBar { me =>
  var isSearching = false
  var lastQuery = new String
  var searchView: SearchView = _

  def setupSearch(m: Menu) = {
    searchView = m.findItem(R.id.action_search).getActionView.asInstanceOf[SearchView]
    searchView addOnAttachStateChangeListener new View.OnAttachStateChangeListener {
      def onViewDetachedFromWindow(lens: View) = runAnd(isSearching = false)(react)
      def onViewAttachedToWindow(lens: View) = runAnd(isSearching = true)(react)
    }

    searchView setOnQueryTextListener new SearchView.OnQueryTextListener {
      def onQueryTextChange(txt: String) = runAnd(true)(me search txt)
      def onQueryTextSubmit(txt: String) = true
    }
  }

  def react: Unit
  def search(txt: String) = {
    // Update and do the search
    lastQuery = txt
    react
  }
}

trait HumanTimeDisplay {
  lazy val timeString = DateFormat is24HourFormat host match {
    case false if scrWidth < 2.2 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if scrWidth < 2.2 => "MM/dd/yy' <small>'h:mma'</small>'"

    case false if scrWidth < 2.5 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if scrWidth < 2.5 => "MM/dd/yy' <small>'h:mma'</small>'"
    case false => "MMM dd, yyyy' <small>'h:mma'</small>'"

    case true if scrWidth < 2.2 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if scrWidth < 2.2 => "d MMM yyyy' <small>'HH:mm'</small>'"

    case true if scrWidth < 2.4 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if scrWidth < 2.5 => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true => "d MMM yyyy' <small>'HH:mm'</small>'"
  }

  val host: TimerActivity
  val time: Date => String = new SimpleDateFormat(timeString) format _
  def when(now: Long, thenDate: Date) = thenDate.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString
    else time(thenDate)
  }

  def initToolbar(toolbar: android.support.v7.widget.Toolbar) = {
    // Show back arrow button to allow users to get back to wallet
    // just kill current activity once a back button is tapped

    host.setSupportActionBar(toolbar)
    host.getSupportActionBar.setDisplayHomeAsUpEnabled(true)
    host.getSupportActionBar.setDisplayShowHomeEnabled(true)
    toolbar.setNavigationOnClickListener(host onButtonTap host.finish)
  }
}

class WalletActivity extends NfcReaderActivity with ScanActivity { me =>
  lazy val foregroundServiceIntent = new Intent(me, AwaitService.classof)
  lazy val floatingActionMenu = findViewById(R.id.fam).asInstanceOf[FloatingActionMenu]
  lazy val slidingFragmentAdapter = new FragmentStatePagerAdapter(getSupportFragmentManager) {
    def getItem(currentFragmentPos: Int) = if (0 == currentFragmentPos) new FragWallet else new FragScan
    def getCount = 2
  }

  override def onDestroy = wrap(super.onDestroy)(stopDetecting)
  override def onResume = wrap(super.onResume)(me returnToBase null)
  override def onOptionsItemSelected(m: MenuItem): Boolean = runAnd(true) {
    if (m.getItemId == R.id.actionSettings) me goTo classOf[SettingsActivity]
  }

  override def onBackPressed = {
    val isExpanded = FragWallet.worker.currentCut > FragWallet.worker.minLinesNum
    if (1 == walletPager.getCurrentItem) walletPager.setCurrentItem(0, true)
    else if (floatingActionMenu.isOpened) floatingActionMenu close true
    else if (isExpanded) FragWallet.worker.toggler.performClick
    else super.onBackPressed
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    // Called after worker sets toolbar as actionbar
    getMenuInflater.inflate(R.menu.wallet, menu)

    // Worker is definitely not null
    FragWallet.worker.setupSearch(menu)
    FragWallet.worker.searchView.setQueryHint(app getString search_hint_payments)
    val showTooltip = app.prefs.getBoolean(AbstractKit.SHOW_TOOLTIP, true)

    if (showTooltip) try {
      app.prefs.edit.putBoolean(AbstractKit.SHOW_TOOLTIP, false).commit
      new SimpleTooltip.Builder(me).anchorView(floatingActionMenu.getMenuIconView)
        .text("Menu").gravity(Gravity.START).transparentOverlay(false).animated(true)
        .build.show
    } catch none
    true
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    wrap(me setDetecting true)(me initNfc state)
    me setContentView R.layout.activity_double_pager
    walletPager setAdapter slidingFragmentAdapter

    val backupUnknownOrFailed = app.prefs.getLong(AbstractKit.GDRIVE_LAST_SAVE, 0L) <= 0L
    val needsCheck = !GDrive.isMissing(app) && app.prefs.getBoolean(AbstractKit.GDRIVE_ENABLED, true) && backupUnknownOrFailed
    if (needsCheck) queue.map(_ => GDrive signInAccount me).foreach(accountOpt => if (accountOpt.isEmpty) askGDriveSignIn)
  } else me exitTo classOf[MainActivity]

  override def onActivityResult(reqCode: Int, resultCode: Int, results: Intent) = {
    val isGDriveSignInSuccessful = reqCode == 102 && resultCode == Activity.RESULT_OK
    app.prefs.edit.putBoolean(AbstractKit.GDRIVE_ENABLED, isGDriveSignInSuccessful).commit
    // This updates lastSaved if user restores wallet from migration file, otherwise no effect
    if (isGDriveSignInSuccessful) ChannelManager.backUp else app toast gdrive_disabled
  }

  // NFC

  def readEmptyNdefMessage = app toast err_no_data
  def readNonNdefMessage = app toast err_no_data
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNdefMessage(nfcMessage: Message) =
    <(app.TransData recordValue ndefMessageString(nfcMessage),
      _ => app toast err_no_data)(_ => checkTransData)

  // EXTERNAL DATA CHECK

  def checkTransData = app.TransData checkAndMaybeErase {
    case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]

    case FragWallet.REDIRECT =>
      // Erase TransData value
      goOps(null): Unit

    case btcURI: BitcoinURI =>
      val canSendOffChain = Try(btcURI.getAmount).map(coin2MSat).filter(msat => ChannelManager.estimateAIRCanSend >= msat.amount).isSuccess
      if (canSendOffChain && btcURI.getLightningRequest != null) <(app.TransData recordValue btcURI.getLightningRequest, onFail)(_ => checkTransData)
      else FragWallet.worker.sendBtcPopup(btcURI)
      me returnToBase null

    case lnUrl: LNUrl =>
      if (lnUrl.isLogin) showLoginForm(lnUrl)
      else fetch1stLevelUrl(lnUrl)
      me returnToBase null

    case pr: PaymentRequest if PaymentRequest.prefixes(LNParams.chainHash) != pr.prefix =>
      // Payee has provided a payment request from some other network, can't be fulfilled
      app toast err_no_data
      me returnToBase null

    case pr: PaymentRequest if !pr.isFresh =>
      // Payment request has expired by now
      app toast dialog_pr_expired
      me returnToBase null

    case pr: PaymentRequest if ChannelManager.all.exists(isOpening) && ChannelManager.mostFundedChanOpt.isEmpty =>
      // Only opening channels are present so sending is not enabled yet, inform user about situation
      onFail(app getString err_ln_still_opening)
      me returnToBase null

    case pr: PaymentRequest if ChannelManager.mostFundedChanOpt.isEmpty =>
      // No channels are present at all currently, inform user about what to do
      showForm(negTextBuilder(dialog_ok, app.getString(ln_send_howto).html).create)
      me returnToBase null

    case pr: PaymentRequest =>
      // We have operational channels at this point
      FragWallet.worker.standardOffChainSend(pr)
      me returnToBase null

    case _ =>
  }

  // LNURL

  def fetch1stLevelUrl(lnUrl: LNUrl) = {
    val awaitRequest = get(lnUrl.uri.toString, true).connectTimeout(15000)
    val sslAwareRequest = awaitRequest.trustAllCerts.trustAllHosts
    app toast ln_url_resolving

    <(to[LNUrlData](LNUrlData guardResponse sslAwareRequest.body), onFail) {
      case incomingChan: IncomingChannelRequest => me initConnection incomingChan
      case withdrawal: WithdrawRequest => me doReceivePayment Some(withdrawal, lnUrl)
      case _ => app toast err_no_data
    }
  }

  def initConnection(incoming: IncomingChannelRequest) = {
    ConnectionManager.listeners += new ConnectionListener { self =>
      override def onDisconnect(nodeId: PublicKey) = ConnectionManager.listeners -= self
      override def onOperational(nodeId: PublicKey, isCompatible: Boolean) = if (isCompatible) {
        queue.map(_ => incoming.requestChannel).map(LNUrlData.guardResponse).foreach(none, onFail)
      }

      override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
        case open: OpenChannel if !open.channelFlags.isPublic => onOpenOffer(nodeId, open)
        case _ => // Ignore anything else including public channel offers
      }

      override def onOpenOffer(nodeId: PublicKey, open: OpenChannel) =
        ConnectionManager.workers get nodeId foreach { existingWorkerConnection =>
          val startFundIncomingChannel = app getString ln_ops_start_fund_incoming_channel
          val hnv = HardcodedNodeView(existingWorkerConnection.ann, startFundIncomingChannel)
          app.TransData.value = IncomingChannelParams(hnv, open)
          me goTo classOf[LNStartFundActivity]
          onDisconnect(nodeId)
        }
    }

    <(incoming.resolveAnnounce, onFail) { ann =>
      val hasChanAlready = ChannelManager hasNormalChanWith ann.nodeId
      if (hasChanAlready) app toast err_ln_chan_exists_already
      else ConnectionManager.connectTo(ann, notify = true)
    }
  }

  def showLoginForm(lnUrl: LNUrl) = lnUrl.k1 map { k1 =>
    lazy val linkingPrivKey = LNParams getLinkingKey lnUrl.uri.getHost
    lazy val linkingPubKey = linkingPrivKey.publicKey.toString

    def wut(alert: AlertDialog): Unit = {
      val bld = baseTextBuilder(getString(ln_url_info_login).format(lnUrl.uri.getHost, linkingPubKey).html)
      mkCheckFormNeutral(_.dismiss, none, _ => me share linkingPubKey, bld, dialog_ok, -1, dialog_share_key)
    }

    def doLogin(alert: AlertDialog) = rm(alert) {
      val signature = Tools.sign(ByteVector fromValidHex k1, linkingPrivKey).toHex
      val secondLevelCallback = get(s"${lnUrl.request}?k1=$k1&sig=$signature&key=$linkingPubKey", true)
      val secondLevelRequest = secondLevelCallback.connectTimeout(7500).trustAllCerts.trustAllHosts
      queue.map(_ => secondLevelRequest.body).map(LNUrlData.guardResponse).foreach(none, onFail)
      app.toast(ln_url_resolving)
    }

    val title = updateView2Blue(oldView = str2View(new String), s"<big>${lnUrl.uri.getHost}</big>")
    mkCheckFormNeutral(doLogin, none, wut, baseBuilder(title, null), dialog_login, dialog_cancel, dialog_wut)
  } getOrElse app.toast(err_no_data)

  // BUTTONS REACTIONS

  type RequestAndLNUrl = (WithdrawRequest, LNUrl)
  def doReceivePayment(extra: Option[RequestAndLNUrl] = None) = {
    val viableChannels = ChannelManager.all.filter(isOpeningOrOperational)
    val withRoutes = viableChannels.filter(isOperational).flatMap(channelAndHop).toMap

    // For now we a bounded to single largest channel
    val receivables = withRoutes.keys.map(_.estimateCanReceive)
    val largestOne = if (receivables.isEmpty) 0L else receivables.max
    val maxCanReceive = MilliSatoshi(largestOne)

    // maxCanReceive may be negative, show a warning to user in this case
    val humanShouldSpend = s"<strong>${denom parsedWithSign -maxCanReceive}</strong>"
    val reserveUnspentWarning = getString(ln_receive_reserve) format humanShouldSpend

    extra match {
      case Some(wr \ lnUrl) =>
        val title = updateView2Blue(str2View(new String), app getString ln_receive_title)
        val finalMaxCanReceiveCapped = MilliSatoshi(wr.maxWithdrawable min maxCanReceive.amount)

        if (viableChannels.isEmpty) showForm(negTextBuilder(dialog_ok, getString(ln_receive_howto).html).create)
        else if (withRoutes.isEmpty) showForm(negTextBuilder(dialog_ok, getString(ln_receive_6conf).html).create)
        else if (maxCanReceive.amount < 0L) showForm(negTextBuilder(dialog_ok, reserveUnspentWarning.html).create)
        else FragWallet.worker.receive(withRoutes, finalMaxCanReceiveCapped, wr.minCanReceive, title, wr.defaultDescription) { rd =>
          def requestWithdraw = wr.unsafe(s"${wr.callback}?k1=${wr.k1}&sig=$makeWithdrawSignature&pr=${PaymentRequest write rd.pr}")
          def makeWithdrawSignature = Tools.sign(ByteVector fromValidHex wr.k1, LNParams getLinkingKey lnUrl.uri.getHost).toHex
          def onRequestFailed(response: Throwable) = wrap(PaymentInfoWrap failOnUI rd)(me onFail response)
          queue.map(_ => requestWithdraw).map(LNUrlData.guardResponse).foreach(none, onRequestFailed)
        }

      case None =>
        val alertLNHint =
          if (viableChannels.isEmpty) getString(ln_receive_suggestion)
          else if (withRoutes.isEmpty) getString(ln_receive_6conf)
          else if (maxCanReceive.amount < 0L) reserveUnspentWarning
          else getString(ln_receive_ok)

        val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_receive, lst).create)
        val options = Array(getString(ln_receive_option).format(alertLNHint).html, getString(btc_receive_option).html)

        def offChain = rm(alert) {
          if (viableChannels.isEmpty) showForm(negTextBuilder(dialog_ok, app.getString(ln_receive_howto).html).create)
          else FragWallet.worker.receive(withRoutes, maxCanReceive, MilliSatoshi(1L), app.getString(ln_receive_title).html, new String) { rd =>
            foregroundServiceIntent.putExtra(AwaitService.SHOW_AMOUNT, denom asString rd.pr.amount.get).setAction(AwaitService.SHOW_AMOUNT)
            ContextCompat.startForegroundService(me, foregroundServiceIntent)
            timer.schedule(me stopService foregroundServiceIntent, 1800000)
            me PRQR rd.pr
          }
        }

        def onChain = rm(alert) {
          app.TransData.value = app.kit.currentAddress
          me goTo classOf[RequestActivity]
        }

        lst setOnItemClickListener onTap { case 0 => offChain case 1 => onChain }
        lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options)
        lst setDividerHeight 0
        lst setDivider null
    }
  }

  def PRQR(pr: PaymentRequest) = {
    me goTo classOf[RequestActivity]
    app.TransData.value = pr
  }

  def goSendPaymentForm(top: View) = {
    val fragCenterList = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_send, fragCenterList).create)
    val options = Array(send_scan_qr, send_paste_payment_request, send_hivemind_deposit).map(res => getString(res).html)
    fragCenterList setOnItemClickListener onTap { case 0 => scanQR case 1 => pasteRequest case 2 => depositHivemind }
    fragCenterList setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options)
    fragCenterList setDividerHeight 0
    fragCenterList setDivider null

    def scanQR = rm(alert) {
      // Just jump to QR scanner section
      walletPager.setCurrentItem(1, true)
    }

    def pasteRequest = rm(alert) {
      def mayResolve(rawBufferString: String) = <(app.TransData recordValue rawBufferString, onFail)(_ => checkTransData)
      scala.util.Try(app.getBufferUnsafe) match { case scala.util.Success(raw) => mayResolve(raw) case _ => app toast err_no_data }
    }

    def depositHivemind = rm(alert) {
      // Show a warning for now since hivemind sidechain is not enabled yet
      val alert = showForm(negTextBuilder(dialog_ok, getString(hivemind_details).html).create)
      try Utils clickableTextField alert.findViewById(android.R.id.message) catch none
    }
  }

  def goStart = me goTo classOf[LNStartActivity]
  def goOps(top: View) = me goTo classOf[LNOpsActivity]
  def goReceivePayment(top: View) = doReceivePayment(extra = Option.empty)
  def goAddChannel(top: View) = if (app.olympus.backupExhausted) warnAboutTokens else goStart

  def warnAboutTokens = {
    val tokensPrice = MilliSatoshi(1000000L)
    val fiatAmount = msatInFiatHuman(tokensPrice)
    val amount = denom.coloredIn(tokensPrice, denom.sign)
    val body = getString(tokens_warn).format(s"$amount <font color=#999999>$fiatAmount</font>")
    val bld = baseTextBuilder(body.html).setCustomTitle(me getString action_ln_open)
    mkCheckForm(alert => rm(alert)(goStart), none, bld, dialog_ok, dialog_cancel)
  }
}