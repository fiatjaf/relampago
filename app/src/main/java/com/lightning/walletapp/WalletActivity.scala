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
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import scala.util.{Failure, Try}
import fr.acinq.bitcoin.{MilliSatoshi, Satoshi}
import com.lightning.walletapp.lnutils.IconGetter.{bigFont, scrWidth}
import com.lightning.walletapp.ln.wire.{NodeAnnouncement, WalletZygote}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.walletZygoteCodec
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
import com.google.common.io.Files
import org.bitcoinj.core.Address
import android.content.Intent
import org.ndeftools.Message
import android.os.Bundle
import android.net.Uri
import java.util.Date
import java.io.File


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
  val time: Date => String = date =>
    new SimpleDateFormat(timeString) format date

  // Should be accessed after activity is initialized
  lazy val timeString = DateFormat is24HourFormat host match {
    case false if scrWidth < 2.2 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if scrWidth < 2.2 => "MM/dd/yy' <small>'h:mma'</small>'"

    case false if scrWidth < 2.5 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if scrWidth < 2.5 => "MM/dd/yy' <small>'h:mma'</small>'"

    case false if bigFont => "MMM dd, yyyy' <small>'h:mma'</small>'"
    case false => "MMMM dd, yyyy' <small>'h:mma'</small>'"

    case true if scrWidth < 2.2 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if scrWidth < 2.2 => "d MMM yyyy' <small>'HH:mm'</small>'"

    case true if scrWidth < 2.4 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if scrWidth < 2.5 => "d MMM yyyy' <small>'HH:mm'</small>'"

    case true if bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true => "d MMMM yyyy' <small>'HH:mm'</small>'"
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
    if (walletPager.getCurrentItem == 1) walletPager.setCurrentItem(0, true)
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
    app.TransData.value match {
      case paymentRequest: PaymentRequest => FragWallet.worker sendPayment paymentRequest
      case bu: BitcoinURI => FragWallet.worker sendBtcPopup bu.getAddress setSum Try(bu.getAmount)
      case btcAddress: Address => FragWallet.worker sendBtcPopup btcAddress
      case FragWallet.REDIRECT => goChanDetails(null)
      case _ =>
    }

    // Clear value in all cases except NodeAnnouncement, we'll need this one
    val isNodeAnnounce = app.TransData.value.isInstanceOf[NodeAnnouncement]
    if (isNodeAnnounce) me goTo classOf[LNStartFundActivity]
    else app.TransData.value = null
  }

  // BUTTONS REACTIONS

  def goReceivePayment(top: View) = {
    val totalOperationalChannels = app.ChannelManager.notClosingOrRefunding.filter(isOperational)
    val operationalChannelsWithRoutes = totalOperationalChannels.flatMap(channelAndHop).toMap
    val maxCanReceive = Try(operationalChannelsWithRoutes.keys.map(estimateCanReceive).max)
    val maxCanReceive1 = MilliSatoshi(maxCanReceive getOrElse 0L)
    val canReceiveLNPayments = maxCanReceive1 >= minHtlcValue

    val reserveUnspent = getString(ln_receive_reserve) format coloredOut(maxCanReceive1)
    val lnReceiveText = if (totalOperationalChannels.isEmpty) getString(ln_receive_option).format(me getString ln_receive_no_channels)
      else if (operationalChannelsWithRoutes.isEmpty) getString(ln_receive_option).format(me getString ln_receive_6conf)
      else if (!canReceiveLNPayments) getString(ln_receive_option).format(reserveUnspent)
      else getString(ln_receive_option).format(me getString ln_receive_ok)

    val options = Array(lnReceiveText.html, getString(btc_receive_option).html)
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_receive, lst).create)

    lst setDivider null
    lst setDividerHeight 0
    lst setOnItemClickListener onTap { case 0 => offChain case 1 => onChain }
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options) {
      override def isEnabled(itemPosition: Int) = itemPosition != 0 || canReceiveLNPayments
    }

    def onChain = rm(alert) {
      app.TransData.value = app.kit.currentAddress
      me goTo classOf[RequestActivity]
    }

    def offChain = rm(alert) {
      // Provide filtered channels with real hops and real receivable amount
      FragWallet.worker.receive(operationalChannelsWithRoutes, maxCanReceive1)
    }
  }

  def goSendPayment(top: View) = {
    val options = Array(send_scan_qr, send_paste).map(option => getString(option).html)
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
      walletPager.setCurrentItem(1, true)
    }
  }

  def goChanDetails(top: View) = {
    val nothingToShow = app.ChannelManager.all.isEmpty
    if (nothingToShow) app toast ln_status_none
    else me goTo classOf[LNOpsActivity]
  }

  val tokensPrice = MilliSatoshi(1000000L)
  def goAddChannel(top: View) = if (app.ChannelManager.all.isEmpty && OlympusWrap.hasAuthEnabled) {
    val humanPrice = s"${coloredIn apply tokensPrice} <font color=#999999>${msatInFiatHuman apply tokensPrice}</font>"
    val warn = baseTextBuilder(getString(tokens_warn).format(humanPrice).html).setCustomTitle(me getString action_ln_open)
    mkForm(me goTo classOf[LNStartActivity], none, warn, dialog_ok, dialog_cancel)
  } else me goTo classOf[LNStartActivity]

  def showDenomChooser = {
    val lnTotalMsat = app.ChannelManager.notClosingOrRefunding.map(estimateCanSend).sum
    val walletTotalSum = Satoshi(app.kit.conf0Balance.value + lnTotalMsat / 1000L)
    val inFiatTotal = msatInFiatHuman apply walletTotalSum

    val title = getLayoutInflater.inflate(R.layout.frag_wallet_state, null)
    val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
    val stateContent = title.findViewById(R.id.stateContent).asInstanceOf[TextView]
    val denomChoiceList = form.findViewById(R.id.choiceList).asInstanceOf[ListView]
    val allDenominations = getResources.getStringArray(R.array.denoms).map(_.html)

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
    stateContent setText s"${coloredIn apply walletTotalSum}<br><small>$inFiatTotal</small>".html
    mkForm(updateDenomination, none, negBuilder(dialog_ok, title, form), dialog_ok, dialog_cancel)
  }

  // SETTINGS FORM

  def makeSettingsForm = {
    val title = getString(read_settings).html
    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val menu = showForm(negBuilder(dialog_ok, title, form).create)

    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val createZygote = form.findViewById(R.id.createZygote).asInstanceOf[Button]
    val manageOlympus = form.findViewById(R.id.manageOlympus).asInstanceOf[Button]
    val recoverFunds = form.findViewById(R.id.recoverChannelFunds).asInstanceOf[Button]
    recoverFunds.setEnabled(app.ChannelManager.currentBlocksLeft < broadcaster.blocksPerDay)

    recoverFunds setOnClickListener onButtonTap {
      // When wallet data is lost users may recover channel funds
      // by fetching encrypted static channel params from server

      rm(menu) {
        val bld = baseTextBuilder(me getString channel_recovery_info)
        mkForm(recover, none, bld, dialog_next, dialog_cancel)

        def recover = {
          OlympusWrap.getBackup(cloudId).foreach(backups => {
            // Decrypt channel recovery data upon successful call and put
            // them into an active channel list, then connect to peers

            for {
              encrypted <- backups
              ref <- Try apply AES.decode(encrypted, cloudSecret) map to[RefundingData]
              if !app.ChannelManager.all.exists(chan => chan(_.channelId) contains ref.commitments.channelId)
            } app.ChannelManager.all +:= app.ChannelManager.createChannel(app.ChannelManager.operationalListeners, ref)
            // Now reconnect all fresh created channels
            app.ChannelManager.initConnect
            // Inform if not fine
          }, onFail)

          // Let user know it's happening
          app toast dialog_recovering
        }
      }
    }

    manageOlympus setOnClickListener onButtonTap {
      // Just show a list of available Olympus servers
      def proceed = me goTo classOf[OlympusActivity]
      rm(menu)(proceed)
    }

    createZygote setOnClickListener onButtonTap {
      def openForm = mkForm(ok = <(createZygote, onFail) { zygote =>
        val zygoteFileShare = new Intent setAction Intent.ACTION_SEND setType "text/plain"
        me startActivity zygoteFileShare.putExtra(Intent.EXTRA_STREAM, Uri fromFile zygote)
      }, none, baseTextBuilder(getString(zygote_details).html), dialog_next, dialog_cancel)

      def createZygote = {
        val dbFile = new File(app.getDatabasePath(dbFileName).getPath)
        val sourceFilesSeq = Seq(dbFile, app.walletFile, app.chainFile)
        val Seq(dbBytes, walletBytes, chainBytes) = sourceFilesSeq map Files.toByteArray
        val encoded = walletZygoteCodec encode WalletZygote(1, dbBytes, walletBytes, chainBytes)
        val zygote = FileOps shell s"Bitcoin Wallet Snapshot ${new Date}.txt"
        Files.write(encoded.require.toByteArray, zygote)
        zygote
      }

      rm(menu)(openForm)
    }

    rescanWallet setOnClickListener onButtonTap {
      // May be needed in case of blockchain glitches
      // warn user as this is a time consuming operation

      rm(menu) {
        val bld = baseTextBuilder(me getString sets_rescan_ok)
        mkForm(go, none, bld, dialog_ok, dialog_cancel)
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