package com.lightning.walletapp

import android.view._
import android.widget._
import com.lightning.walletapp.ln._
import android.text.format.DateUtils._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.IconGetter.{bigFont, scrWidth}
import com.lightning.walletapp.ln.wire.{NodeAnnouncement, Started}
import com.lightning.walletapp.lnutils.JsonHttpUtils.{obsOnIO, to}
import org.bitcoinj.core.{Address, TxWrap}
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import android.support.v4.app.FragmentStatePagerAdapter
import com.lightning.walletapp.Denomination.coin2MSat
import org.ndeftools.util.activity.NfcReaderActivity
import com.lightning.walletapp.helper.AwaitService
import com.github.clans.fab.FloatingActionMenu
import com.lightning.walletapp.lnutils.GDrive
import android.support.v7.widget.SearchView
import fr.acinq.bitcoin.Crypto.PublicKey
import android.text.format.DateFormat
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat

import android.content.Intent
import org.ndeftools.Message
import android.app.Activity

import scala.util.Success
import android.os.Bundle
import java.util.Date

import com.lightning.walletapp.test.{FailureMessageLightningMessageCodecsSpec, GeneratorsSpec, SphinxSpec, WireSpec}


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
  val host: TimerActivity
  // Should be accessed after activity is initialized
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
  lazy val awaitServiceIntent: Intent = new Intent(me, AwaitService.classof)
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
    // Called after fragLN sets toolbar as actionbar
    getMenuInflater.inflate(R.menu.wallet, menu)
    // Updated here to make sure it's present
    FragWallet.worker setupSearch menu
    true
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    wrap(me setDetecting true)(me initNfc state)
    me setContentView R.layout.activity_double_pager
    walletPager setAdapter slidingFragmentAdapter

    val shouldCheck = app.prefs.getLong(AbstractKit.GDRIVE_LAST_SAVE, 0L) <= 0L // Unknown or failed
    val needsCheck = !GDrive.isMissing(app) && app.prefs.getBoolean(AbstractKit.GDRIVE_ENABLED, true) && shouldCheck
    if (needsCheck) obsOnIO.map(_ => GDrive signInAccount me).foreach(accountOpt => if (accountOpt.isEmpty) askGDriveSignIn)
  } else me exitTo classOf[MainActivity]

  override def onActivityResult(reqCode: Int, resultCode: Int, results: Intent) = {
    val isGDriveSignInSuccessful = reqCode == 102 && resultCode == Activity.RESULT_OK
    app.prefs.edit.putBoolean(AbstractKit.GDRIVE_ENABLED, isGDriveSignInSuccessful).commit
    if (!isGDriveSignInSuccessful) app toast gdrive_disabled
  }

  // NFC

  def readEmptyNdefMessage = app toast err_no_data
  def readNonNdefMessage = app toast err_no_data
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNdefMessage(m: Message) =
    <(app.TransData recordValue ndefMessageString(m),
      _ => app toast err_no_data)(_ => checkTransData)

  // EXTERNAL DATA CHECK

  def checkTransData: Unit =
    app.TransData checkAndMaybeErase {
      // TransData value should be retained in both of these cases
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case _: Started => goStart

      case FragWallet.REDIRECT =>
        // TransData value should be erased here
        // so goOps return type is forced to Unit
        goOps(null): Unit

      case lnUrl: LNUrl =>
        // LNURL is unbounded when directly scanned i.e. user is aware of what is going on
        // LNURL is bounded when it is extracted from a payment request since user is unaware
        // bounded LNURLs require explicit user approval to make HTTP calls

        lnUrl.action match {
          case Success("proofOfPayer") =>

          case _ if isUnbounded =>
            // TransData value will be erased here
            // This lnURL has no known action, historically this means it's a chan open request
            // we must fetch chan parameters and make sure node is connected before asking for new channel
            val request = get(lnUrl.uri.toString, true).trustAllCerts.trustAllHosts.connectTimeout(7500)
            val ask = obsOnIO.map(_ => request.body) map to[IncomingChannelRequest]
            ask.foreach(initConnection, Tools.errlog)
            app toast ln_url_requesting_new_channel
            me returnToBase null

          case _ =>
            // Something is wrong, go back home
            // TransData value will be erased here
            me returnToBase null
        }

      case address: Address =>
        // TransData value will be erased here
        FragWallet.worker.sendBtcPopup(address)(none)
        me returnToBase null

      case uri: BitcoinURI =>
        // TransData value will be erased here
        val manager = FragWallet.worker.sendBtcPopup(uri.getAddress)(none)
        manager setSum scala.util.Try(uri.getAmount)
        me returnToBase null

      case pr: PaymentRequest =>
        if (pr.lnUrlOpt.isDefined) {
          // Presence of LNURL overrides pr processing
          app.TransData.value = pr.lnUrlOpt.get
          checkTransData

        } else if (ChannelManager.notClosingOrRefunding.isEmpty) {
          // No operational channels are present, offer to open a new one
          // TransData should be set to batch or null to erase previous value
          app.TransData.value = TxWrap findBestBatch pr getOrElse null
          // Do not erase a previously set data
          goStart

        } else {
          // We have open or at least opening channels
          // TransData value will be erased here
          FragWallet.worker sendPayment pr
          me returnToBase null
        }

      case otherwise =>
    }

  def initConnection(incoming: IncomingChannelRequest) =
    ConnectionManager.listeners += new ConnectionListener { self =>
      // This is done to make sure we definitely have an LN connection
      ConnectionManager.connectTo(ann = incoming.getAnnounce, notify = true)
      override def onOperational(nodeId: PublicKey, isCompat: Boolean) = if (isCompat) {
        // Remove listener and make a request, their OpenChannel message should arrive shortly
        incoming.requestChannel.foreach(none, none)
        ConnectionManager.listeners -= self
      }
    }

  // BUTTONS REACTIONS

  def goReceivePayment(top: View) = {
    val operationalChannels = ChannelManager.notClosingOrRefunding.filter(isOperational)
    val operationalChannelsWithRoutes: Map[Channel, PaymentRoute] = operationalChannels.flatMap(channelAndHop).toMap
    val maxCanReceiveMsat = operationalChannelsWithRoutes.keys.map(estimateCanReceiveCapped).reduceOption(_ max _) getOrElse 0L
    val maxCanReceive = MilliSatoshi(maxCanReceiveMsat)

    val reserveUnspent = getString(ln_receive_reserve) format denom.coloredOut(-maxCanReceive, denom.sign)
    val lnReceiveText = if (operationalChannels.isEmpty) getString(ln_receive_option).format(me getString ln_no_open_chans)
      else if (operationalChannelsWithRoutes.isEmpty) getString(ln_receive_option).format(me getString ln_receive_6conf)
      else if (maxCanReceiveMsat < 0L) getString(ln_receive_option).format(reserveUnspent)
      else getString(ln_receive_option).format(me getString ln_receive_ok)

    val options = Array(lnReceiveText.html, getString(btc_receive_option).html)
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_receive, lst).create)
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options)
    lst setOnItemClickListener onTap { case 0 => offChain case 1 => onChain }
    lst setDividerHeight 0
    lst setDivider null

    def onChain = rm(alert) {
      app.TransData.value = app.kit.currentAddress
      me goTo classOf[RequestActivity]
    }

    def offChain = rm(alert) {
      // Provide filtered channels with real hops and real receivable amount
      FragWallet.worker.receive(operationalChannelsWithRoutes, maxCanReceive)
    }
  }

  def goSendPayment(top: View) = {
    val fragCenterList = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_send, fragCenterList).create)
    val options = Array(send_scan_qr, send_paste_payment_request, send_hivemind_deposit).map(res => getString(res).html)
    fragCenterList setOnItemClickListener onTap { case 0 => scanQR case 1 => pasteRequest case 2 => depositHivemind }
    fragCenterList setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options)
    fragCenterList setDividerHeight 0
    fragCenterList setDivider null

    def pasteRequest = rm(alert) {
      val resultTry = app.getBufferTry map app.TransData.recordValue
      if (resultTry.isSuccess) checkTransData else app toast err_no_data
    }

    def scanQR = rm(alert) {
      // Just jump to QR scanner section
      walletPager.setCurrentItem(1, true)
    }

    def depositHivemind = rm(alert) {
      // Show a warning for now since hivemind sidechain is not enabled yet
      val alert = showForm(negTextBuilder(dialog_ok, getString(hivemind_details).html).create)
      try Utils clickableTextField alert.findViewById(android.R.id.message) catch none
    }
  }

  val tokensPrice = MilliSatoshi(1000000L)
  def goStart = me goTo classOf[LNStartActivity]
  def goOps(top: View) = me goTo classOf[LNOpsActivity]
  def goAddChannel(top: View) = if (app.olympus.backupExhausted) {
    val coloredAmount = denom.coloredIn(msat = tokensPrice, denom.sign)
    val warn = getString(tokens_warn) format s"$coloredAmount <font color=#999999>${msatInFiatHuman apply tokensPrice}</font>"
    mkCheckForm(alert => rm(alert)(goStart), none, baseTextBuilder(warn.html).setCustomTitle(me getString action_ln_open), dialog_ok, dialog_cancel)
  } else goStart
}