package com.lightning.walletapp

import android.view._
import android.widget._
import com.lightning.walletapp.ln._
import android.text.format.DateUtils._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.Denomination._
import android.support.v4.view.MenuItemCompat._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import scala.util.{Failure, Try}
import org.bitcoinj.core.{Address, TxWrap}
import fr.acinq.bitcoin.{MilliSatoshi, Satoshi}
import com.lightning.walletapp.ln.wire.{NodeAnnouncement, Started}
import com.lightning.walletapp.lnutils.IconGetter.{bigFont, scrWidth}
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import android.support.v4.app.FragmentStatePagerAdapter
import org.ndeftools.util.activity.NfcReaderActivity
import com.github.clans.fab.FloatingActionMenu
import android.support.v7.widget.SearchView
import com.lightning.walletapp.helper.AES
import org.bitcoinj.store.SPVBlockStore
import android.text.format.DateFormat
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat
import org.ndeftools.Message
import android.os.Bundle
import java.util.Date


trait SearchBar { me =>
  var react: String => Unit = _
  var searchView: SearchView = _

  // This may be queried before search menu is created so null check
  def isSearching = searchView != null && !searchView.isIconified

  def setupSearch(menu: Menu) = {
    val item = menu findItem R.id.action_search
    searchView = getActionView(item).asInstanceOf[SearchView]
    searchView setOnQueryTextListener new SearchView.OnQueryTextListener {
      def onQueryTextChange(queryText: String) = runAnd(true)(me react queryText)
      def onQueryTextSubmit(queryText: String) = true
    }
  }
}

trait HumanTimeDisplay {
  val host: TimerActivity
  val time: Date => String = date => {
    new SimpleDateFormat(timeString) format date
  }

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

  def when(now: Long, date: Date) = date.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString
    else time(date)
  }
}

class WalletActivity extends NfcReaderActivity with ScanActivity { me =>
  lazy val floatingActionMenu = findViewById(R.id.fam).asInstanceOf[FloatingActionMenu]
  lazy val slidingFragmentAdapter = new FragmentStatePagerAdapter(getSupportFragmentManager) {
    def getItem(currentFragmentPos: Int) = if (0 == currentFragmentPos) new FragWallet else new FragScan
    def getCount = 2
  }

  override def onDestroy = wrap(super.onDestroy)(stopDetecting)
  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionSettings) makeSettingsForm
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
  } else me exitTo classOf[MainActivity]

  // NFC

  def readEmptyNdefMessage = app toast err_no_data
  def readNonNdefMessage = app toast err_no_data
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNdefMessage(m: Message) =
    <(app.TransData recordValue ndefMessageString(m),
      fail => app toast err_no_data)(ok => checkTransData)

  // EXTERNAL DATA CHECK

  def checkTransData = {
    returnToBase(view = null)
    app.TransData checkAndMaybeErase {
      case _: Started => me goTo classOf[LNStartActivity]
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case onChainAddress: Address => FragWallet.worker.sendBtcPopup(onChainAddress)(none)
      case uri: BitcoinURI => FragWallet.worker.sendBtcPopup(uri.getAddress)(none) setSum Try(uri.getAmount)
      case pr: PaymentRequest if app.ChannelManager.notClosingOrRefunding.isEmpty => maybeOfferBatch(pr)
      case pr: PaymentRequest => FragWallet.worker sendPayment pr
      case FragWallet.REDIRECT => goOps(null)
      case _ =>
    }
  }

  def maybeOfferBatch(pr: PaymentRequest) = {
    // TransData should be set to batch or null to erase previous
    app.TransData.value = TxWrap findBestBatch pr getOrElse null
    me goTo classOf[LNStartActivity]
  }

  // BUTTONS REACTIONS

  def goReceivePayment(top: View) = {
    val operationalChannels = app.ChannelManager.notClosingOrRefunding.filter(isOperational)
    val operationalChannelsWithRoutes: Map[Channel, PaymentRoute] = operationalChannels.flatMap(channelAndHop).toMap
    val maxCanReceiveMsat = operationalChannelsWithRoutes.keys.map(estimateCanReceiveCapped).reduceOption(_ max _) getOrElse 0L
    val maxCanReceive = MilliSatoshi(math abs maxCanReceiveMsat)

    val reserveUnspent = getString(ln_receive_reserve) format coloredOut(maxCanReceive)
    val lnReceiveText = if (operationalChannels.isEmpty) getString(ln_receive_option).format(me getString ln_receive_nochan)
      else if (operationalChannelsWithRoutes.isEmpty) getString(ln_receive_option).format(me getString ln_receive_6conf)
      else if (maxCanReceiveMsat < 0L) getString(ln_receive_option).format(reserveUnspent)
      else getString(ln_receive_option).format(me getString ln_receive_ok)

    val options = Array(lnReceiveText.html, getString(btc_receive_option).html)
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_receive, lst).create)

    lst setDivider null
    lst setDividerHeight 0
    lst setOnItemClickListener onTap { case 0 => offChain case 1 => onChain }
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options) {
      override def isEnabled(position: Int) = position != 0 || maxCanReceiveMsat > 0L
    }

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
    val options = Array(getString(send_scan_qr).html, getString(send_paste_payment_request).html)
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_send, lst).create)
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options)
    lst setOnItemClickListener onTap { case 0 => scanQR case 1 => pasteRequest }
    lst setDividerHeight 0
    lst setDivider null

    def pasteRequest = rm(alert) {
      app.getBufferTry map app.TransData.recordValue match {
        case Failure(junkDataProvided) => app toast err_no_data
        case _ => checkTransData
      }
    }

    def scanQR = rm(alert) {
      // Just jump to QR scanner section
      walletPager.setCurrentItem(1, true)
    }
  }

  val tokensPrice = MilliSatoshi(1000000L)
  def goLNStart: Unit = me goTo classOf[LNStartActivity]
  def goOps(top: View): Unit = me goTo classOf[LNOpsActivity]
  def goAddChannel(top: View) = if (OlympusWrap.backupExhausted) {
    val humanPrice = s"${coloredIn apply tokensPrice} <font color=#999999>${msatInFiatHuman apply tokensPrice}</font>"
    val warn = baseTextBuilder(getString(tokens_warn).format(humanPrice).html).setCustomTitle(me getString action_ln_open)
    mkCheckForm(alert => rm(alert)(goLNStart), none, warn, dialog_ok, dialog_cancel)
  } else goLNStart

  def showDenomChooser = {
    val lnTotalMsat = app.ChannelManager.notClosingOrRefunding.map(estimateCanSend).sum
    val walletTotalSum = Satoshi(app.kit.conf0Balance.value + lnTotalMsat / 1000L)
    val rate = msatInFiatHuman apply MilliSatoshi(100000000000L)

    val title = getLayoutInflater.inflate(R.layout.frag_wallet_state, null)
    val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
    val stateContent = title.findViewById(R.id.stateContent).asInstanceOf[TextView]
    val denomChoiceList = form.findViewById(R.id.choiceList).asInstanceOf[ListView]
    val allDenominations = getResources.getStringArray(R.array.denoms).map(_.html)
    title.findViewById(R.id.stateExchange).asInstanceOf[TextView] setText rate

    def updateDenomination = {
      // Update denom first so UI update can react to changes, also persist user choice in local data
      app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, denomChoiceList.getCheckedItemPosition).commit
      denom = denoms(denomChoiceList.getCheckedItemPosition)
      FragWallet.worker.adapter.notifyDataSetChanged
      FragWallet.worker.updTitle.run
    }

    denomChoiceList.getCheckedItemPosition
    denomChoiceList setAdapter new ArrayAdapter(me, singleChoice, allDenominations)
    denomChoiceList.setItemChecked(app.prefs.getInt(AbstractKit.DENOM_TYPE, 0), true)
    stateContent setText s"${denom withSign walletTotalSum}<br><small>${msatInFiatHuman apply walletTotalSum}</small>".html
    mkCheckForm(alert => rm(alert)(updateDenomination), none, negBuilder(dialog_ok, title, form), dialog_ok, dialog_cancel)
  }

  // SETTINGS FORM

  def makeSettingsForm = {
    val title = getString(read_settings).html
    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val menu = showForm(negBuilder(dialog_ok, title, form).create)

    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val manageOlympus = form.findViewById(R.id.manageOlympus).asInstanceOf[Button]
    val recoverFunds = form.findViewById(R.id.recoverChannelFunds).asInstanceOf[Button]
    recoverFunds.setEnabled(app.ChannelManager.currentBlocksLeft < broadcaster.blocksPerDay)

    recoverFunds setOnClickListener onButtonTap {
      def recover = OlympusWrap.getBackup(cloudId) foreach { backups =>
        // Decrypt channel recovery data upon successful call and put them
        // into an active channel list, then connect to remote peers

        for {
          encryptedBackup <- backups
          ref <- AES.decode(encryptedBackup, cloudSecret) map to[RefundingData]
          if !app.ChannelManager.all.flatMap(_ apply identity).exists(_.channelId == ref.commitments.channelId)
        } app.ChannelManager.all +:= app.ChannelManager.createChannel(app.ChannelManager.operationalListeners, ref)
        app.ChannelManager.initConnect
      }

      rm(menu) {
        def go = runAnd(app toast dialog_recovering)(recover)
        val bld = baseTextBuilder(me getString channel_recovery_info)
        mkCheckForm(alert => rm(alert)(go), none, bld, dialog_next, dialog_cancel)
      }
    }

    manageOlympus setOnClickListener onButtonTap {
      // Just show a list of available Olympus servers
      def proceed = me goTo classOf[OlympusActivity]
      rm(menu)(proceed)
    }

    rescanWallet setOnClickListener onButtonTap {
      // May be needed in case of blockchain glitches
      // warn user as this is a time consuming operation

      rm(menu) {
        val bld = baseTextBuilder(me getString sets_rescan_ok)
        mkCheckForm(alert => rm(alert)(go), none, bld, dialog_ok, dialog_cancel)
      }

      def go = try {
        app.chainFile.delete
        app.kit.wallet.reset
        app.kit.store = new SPVBlockStore(app.params, app.chainFile)
        app.kit useCheckPoints app.kit.wallet.getEarliestKeyCreationTime
        app.kit.wallet saveToFile app.walletFile
      } catch none finally System exit 0
    }

    viewMnemonic setOnClickListener onButtonTap {
      // Can be accessed here and from page button
      rm(menu)(me viewMnemonic null)
    }
  }
}