package com.lightning.walletapp

import android.view._
import com.lightning.walletapp.ln._
import android.text.format.DateUtils._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.Denomination._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import scala.util.{Success, Try}
import android.app.{Activity, AlertDialog}
import org.bitcoinj.core.{Block, FilteredBlock, Peer}
import com.lightning.walletapp.lnutils.{GDrive, PaymentInfoWrap}
import com.lightning.walletapp.lnutils.JsonHttpUtils.{queue, to}
import com.lightning.walletapp.lnutils.IconGetter.{bigFont, scrWidth}
import io.github.douglasjunior.androidSimpleTooltip.SimpleTooltip
import android.support.v4.app.FragmentStatePagerAdapter
import org.ndeftools.util.activity.NfcReaderActivity
import com.lightning.walletapp.helper.AwaitService
import android.support.v4.content.ContextCompat
import com.github.clans.fab.FloatingActionMenu
import android.support.v7.widget.SearchView
import org.bitcoinj.script.ScriptBuilder
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
    val autoHostedChan = app.prefs.getBoolean(AbstractKit.AUTO_HOSTED_CHAN, true)
    val showTooltip = app.prefs.getBoolean(AbstractKit.SHOW_TOOLTIP, true)

    if (autoHostedChan) {
      val refundScriptPubKey = ByteVector(ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram)
      val waitData = WaitRemoteHostedReply(defaultHostedNode.ann, refundScriptPubKey, defaultHostedNode.secret)
      val freshChannel = ChannelManager.createHostedChannel(Set.empty, waitData)

      val autoOpenListener = new ConnectionListener with ChannelListener {
        override def onHostedMessage(ann: NodeAnnouncement, message: HostedChannelMessage) =
          // At this point hosted channel can only receive hosted messages or remote Error
          if (ann.nodeId == defaultHostedNode.ann.nodeId) freshChannel process message

        override def onDisconnect(nodeId: PublicKey) =
          // We risk nothing by just halting and not doing anything
          if (nodeId == defaultHostedNode.ann.nodeId) detachItself

        override def onMessage(nodeId: PublicKey, remoteMessage: LightningMessage) = remoteMessage match {
          case error: wire.Error if nodeId == defaultHostedNode.ann.nodeId => translateHostedTaggedError(error)
          case _ => // Do nothing
        }

        override def onOperational(nodeId: PublicKey, isCompat: Boolean) =
          // All sanity checks have already been made at this point so just proceed
          if (nodeId == defaultHostedNode.ann.nodeId && isCompat) freshChannel.startUp

        override def onBecome = {
          case (_: HostedChannel, _, WAIT_FOR_ACCEPT, OPEN) =>
            freshChannel.listeners = ChannelManager.operationalListeners
            ChannelManager.all +:= freshChannel
            FragWallet.worker.updTitleTask.run
            detachItself
        }

        override def onException = {
          case (_: HostedChannel, openingError) =>
            // Cancel everything in case of local failure
            UITask(app quickToast openingError.getMessage).run
            detachItself
        }

        def translateHostedTaggedError(error: wire.Error) = {
          val errorMessage = ChanErrorCodes.translateTag(error)
          onException(freshChannel -> errorMessage)
        }

        def detachItself = {
          // In case of success or any failure we cancel further automatic attemps
          app.prefs.edit.putBoolean(AbstractKit.AUTO_HOSTED_CHAN, false).commit
          ConnectionManager.listeners -= this
          freshChannel.listeners -= this
        }
      }

      new BlocksListener {
        app.kit.peerGroup addBlocksDownloadedEventListener this
        var exists = ChannelManager hasHostedChanWith defaultHostedNode.ann.nodeId
        def onBlocksDownloaded(peer: Peer, block: Block, fileterdBlock: FilteredBlock, left: Int) = {
          if (exists) app.prefs.edit.putBoolean(AbstractKit.AUTO_HOSTED_CHAN, false).commit else attemptHosted
          app.kit.peerGroup removeBlocksDownloadedEventListener this
        }

        def attemptHosted = {
          freshChannel.listeners += autoOpenListener
          ConnectionManager.listeners += autoOpenListener
          ConnectionManager.connectTo(defaultHostedNode.ann, notify = true)
          exists = true
        }
      }
    }

    if (showTooltip) try {
      app.prefs.edit.putBoolean(AbstractKit.SHOW_TOOLTIP, false).commit
      val tip = new SimpleTooltip.Builder(me).anchorView(floatingActionMenu.getMenuIconView)
      tip.text("Menu").gravity(Gravity.START).transparentOverlay(false).animated(true).build.show
    } catch none
    true
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    wrap(me setDetecting true)(me initNfc state)
    me setContentView R.layout.activity_double_pager
    walletPager setAdapter slidingFragmentAdapter
    var warnedOffline = Set.empty[ByteVector]

    PaymentInfoWrap.newRoutesOrGiveUp = rd => {
      val termNodes = rd.pr.routingInfo.flatMap(_.route.lastOption).map(_.nodeId)
      val reportedTermOfflineNodes = PaymentInfo.terminalOfflineNodeIds(rd.pr.paymentHash)
      val isConvincing = reportedTermOfflineNodes.intersect(termNodes).size > termNodes.size / 3

      if (!warnedOffline.contains(rd.pr.paymentHash) && isConvincing) {
        // One of terminal nodes returned ChannelDisabled or UnknownNextPeer
        PaymentInfoWrap.updStatus(PaymentInfo.FAILURE, rd.pr.paymentHash)
        UITask(me toast err_ln_receiver_offline).run
        warnedOffline += rd.pr.paymentHash
      } else if (rd.callsLeft > 0 && ChannelManager.checkIfSendable(rd).isRight) {
        // We do not care about options such as AIR or AMP here, this HTLC may be one of them
        PaymentInfoWrap fetchAndSend rd.copy(callsLeft = rd.callsLeft - 1, useCache = false)
      } else {
        // Too many attempts and still no luck so we give up for now
        PaymentInfoWrap.updStatus(PaymentInfo.FAILURE, rd.pr.paymentHash)
      }
    }

    PaymentInfoWrap.failOnUI = rd => {
      PaymentInfoWrap.unsentPayments -= rd.pr.paymentHash
      PaymentInfoWrap.updStatus(PaymentInfo.FAILURE, rd.pr.paymentHash)
      if (rd.onChainFeeBlockWasUsed) UITask(me toast ln_fee_expesive_omitted).run
      PaymentInfoWrap.uiNotify
    }

  } else me exitTo classOf[MainActivity]

  override def onActivityResult(reqCode: Int, resultCode: Int, results: Intent) = {
    val isGDriveSignInSuccessful = reqCode == 102 && resultCode == Activity.RESULT_OK
    app.prefs.edit.putBoolean(AbstractKit.GDRIVE_ENABLED, isGDriveSignInSuccessful).commit
    // This updates lastSaved if user restores wallet from migration file, otherwise no effect
    if (isGDriveSignInSuccessful) ChannelManager.backUp else app quickToast gdrive_disabled
  }

  // NFC

  def readEmptyNdefMessage = app quickToast err_nothing_useful
  def readNonNdefMessage = app quickToast err_nothing_useful
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNdefMessage(nfcMessage: Message) =
    <(app.TransData recordValue ndefMessageString(nfcMessage),
      error => app quickToast err_nothing_useful)(ok => checkTransData)

  // EXTERNAL DATA CHECK

  def checkTransData = app.TransData checkAndMaybeErase {
    case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
    case FragWallet.REDIRECT => goOps(null): Unit

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
      app quickToast err_nothing_useful
      me returnToBase null

    case pr: PaymentRequest if !pr.isFresh =>
      // Payment request has expired by now
      app quickToast dialog_pr_expired
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
    app quickToast ln_url_resolving

    <(to[LNUrlData](LNUrl guardResponse sslAwareRequest.body), onFail) {
      case withdrawal: WithdrawRequest => me doReceivePayment Some(withdrawal, lnUrl)
      case hostedRequest: HostedChannelRequest => me goLNStartFund hostedRequest
      case incoming: IncomingChannelRequest => me initIncoming incoming
      case _ => app quickToast err_nothing_useful
    }
  }

  def goLNStartFund(data: Any) = {
    me goTo classOf[LNStartFundActivity]
    app.TransData.value = data
  }

  def initIncoming(incoming: IncomingChannelRequest) = {
    val initialListener = new ConnectionListener { self =>
      override def onDisconnect(nodeId: PublicKey) = ConnectionManager.listeners -= self
      override def onOperational(nodeId: PublicKey, isCompatible: Boolean) = if (isCompatible) {
        queue.map(_ => incoming.requestChannel.body).map(LNUrl.guardResponse).foreach(none, onCallFailed)
      }

      override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
        case open: OpenChannel if !open.channelFlags.isPublic => onOpenOffer(nodeId, open)
        case _ => // Ignore anything else including public channel offers
      }

      override def onOpenOffer(nodeId: PublicKey, open: OpenChannel) = {
        val incomingTip = app getString ln_ops_start_fund_incoming_channel
        val hnv = HardcodedNodeView(incoming.ann, incomingTip)
        me goLNStartFund IncomingChannelParams(hnv, open)
        ConnectionManager.listeners -= self
      }

      def onCallFailed(err: Throwable) = {
        ConnectionManager.listeners -= self
        onFail(err)
      }
    }

    if (ChannelManager hasNormalChanWith incoming.ann.nodeId) {
      // TODO: remove this limitation once random shortId is merged
      me toast err_ln_chan_exists_already
    } else {
      ConnectionManager.listeners += initialListener
      ConnectionManager.connectTo(incoming.ann, notify = true)
    }
  }

  def showLoginForm(lnUrl: LNUrl) = lnUrl.k1 foreach { k1 =>
    val linkingPrivKey = LNParams.makeLinkingKey(lnUrl.uri.getHost)
    val linkingPubKey = linkingPrivKey.publicKey.toString
    val dataToSign = ByteVector.fromValidHex(k1)

    def wut(alert: AlertDialog): Unit = {
      val bld = baseTextBuilder(getString(ln_url_info_login).format(lnUrl.uri.getHost, linkingPubKey).html)
      mkCheckFormNeutral(_.dismiss, none, _ => me share linkingPubKey, bld, dialog_ok, -1, dialog_share_key)
    }

    def doLogin(alert: AlertDialog) = rm(alert) {
      val sig = Tools.sign(dataToSign, linkingPrivKey)
      val secondLevelRequestUri = lnUrl.uri.buildUpon.appendQueryParameter("sig", sig.toHex).appendQueryParameter("key", linkingPubKey)
      val sslAwareSecondRequest = get(secondLevelRequestUri.build.toString, true).connectTimeout(15000).trustAllCerts.trustAllHosts
      queue.map(_ => sslAwareSecondRequest.body).map(LNUrl.guardResponse).foreach(_ => onLoginSuccess.run, onFail)
      app quickToast ln_url_resolving
    }

    def onLoginSuccess = UITask {
      val message = getString(ln_url_login_ok).format(lnUrl.uri.getHost).html
      mkCheckForm(alert => rm(alert)(finish), none, baseTextBuilder(message), dialog_close, -1)
    }

    val title = updateView2Blue(oldView = str2View(new String), s"<big>${lnUrl.uri.getHost}</big>")
    mkCheckFormNeutral(doLogin, none, wut, baseBuilder(title, null), dialog_login, dialog_cancel, dialog_wut)
  }

  // BUTTONS REACTIONS

  type RequestAndLNUrl = (WithdrawRequest, LNUrl)
  def doReceivePayment(extra: Option[RequestAndLNUrl] = None) = {
    val viableChannels = ChannelManager.all.filter(isOpeningOrOperational)
    val withRoutes = viableChannels.filter(isOperational).flatMap(channelAndHop).toMap

    // For now we a bounded to single largest channel
    val receivables = withRoutes.keys.map(_.estCanReceiveMsat)
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
          queue.map(_ => wr.requestWithdraw(lnUrl, rd.pr).body).map(LNUrl.guardResponse).foreach(none, onRequestFailed)
          def onRequestFailed(response: Throwable) = wrap(PaymentInfoWrap failOnUI rd)(me onFail response)
        }

      case None =>
        val alertLNHint =
          if (viableChannels.isEmpty) getString(ln_receive_suggestion)
          else if (withRoutes.isEmpty) getString(ln_receive_6conf)
          else if (maxCanReceive.amount < 0L) reserveUnspentWarning
          else getString(ln_receive_ok)

        val actions = Array(getString(ln_receive_option).format(alertLNHint).html, getString(btc_receive_option).html)
        val lst \ alert = makeChoiceList(actions, me getString action_coins_receive)
        lst setOnItemClickListener onTap { case 0 => offChain case 1 => onChain }

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
    }
  }

  def PRQR(pr: PaymentRequest) = {
    me goTo classOf[RequestActivity]
    app.TransData.value = pr
  }

  def goSendPaymentForm(top: View) = {
    val actions = Array(send_scan_qr, send_paste_payment_request, send_hivemind_deposit)
    val lst \ alert = makeChoiceList(for (res <- actions) yield getString(res).html, me getString action_coins_send)
    lst setOnItemClickListener onTap { case 0 => scanQR case 1 => pasteRequest case 2 => depositHivemind }

    def scanQR = rm(alert) {
      // Just jump to QR scanner section
      walletPager.setCurrentItem(1, true)
    }

    def pasteRequest = rm(alert) {
      def mayResolve(rawBufferString: String) = <(app.TransData recordValue rawBufferString, onFail)(_ => checkTransData)
      Try(app.getBufferUnsafe) match { case Success(rawData) => mayResolve(rawData) case _ => app quickToast err_nothing_useful }
    }

    def depositHivemind = rm(alert) {
      // Show a warning for now since hivemind sidechain is not enabled yet
      val alert = showForm(negTextBuilder(dialog_ok, getString(hivemind_details).html).create)
      try Utils clickableTextField alert.findViewById(android.R.id.message) catch none
    }
  }

  def goOps(top: View) = me goTo classOf[LNOpsActivity]
  def goAddChannel(top: View) = me goTo classOf[LNStartActivity]
  def goReceivePayment(top: View) = doReceivePayment(extra = Option.empty)
}