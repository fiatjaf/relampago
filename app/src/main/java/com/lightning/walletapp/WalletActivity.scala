package com.lightning.walletapp

import android.view._
import android.widget._
import com.lightning.walletapp.ln._
import android.text.format.DateUtils._
import com.lightning.walletapp.Utils._
import com.journeyapps.barcodescanner._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.Denomination._
import android.support.v4.view.MenuItemCompat._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import scala.util.{Failure, Success, Try}
import fr.acinq.bitcoin.{MilliSatoshi, Satoshi}
import android.provider.Settings.{System => FontSystem}
import android.support.v4.app.{Fragment, FragmentStatePagerAdapter}
import com.lightning.walletapp.ln.wire.{NodeAnnouncement, WalletZygote}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.walletZygoteCodec
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import org.ndeftools.util.activity.NfcReaderActivity
import com.lightning.walletapp.lnutils.RatesSaver
import com.github.clans.fab.FloatingActionMenu
import android.support.v7.widget.SearchView
import com.lightning.walletapp.helper.AES
import android.support.v4.view.ViewPager
import org.bitcoinj.store.SPVBlockStore
import android.text.format.DateFormat
import org.bitcoinj.uri.BitcoinURI
import com.google.common.io.Files
import java.text.SimpleDateFormat
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
  val time: Date => String = date => new SimpleDateFormat(timeString) format date
  lazy val bigFont = FontSystem.getFloat(host.getContentResolver, FontSystem.FONT_SCALE, 1) > 1

  // Should be accessed after activity is initialized
  lazy val timeString = DateFormat is24HourFormat host match {
    case false if host.scrWidth < 2.2 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if host.scrWidth < 2.2 => "MM/dd/yy' <small>'h:mma'</small>'"

    case false if host.scrWidth < 2.5 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if host.scrWidth < 2.5 => "MM/dd/yy' <small>'h:mma'</small>'"

    case false if bigFont => "MMM dd, yyyy' <small>'h:mma'</small>'"
    case false => "MMMM dd, yyyy' <small>'h:mma'</small>'"

    case true if host.scrWidth < 2.2 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if host.scrWidth < 2.2 => "d MMM yyyy' <small>'HH:mm'</small>'"

    case true if host.scrWidth < 2.4 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if host.scrWidth < 2.5 => "d MMM yyyy' <small>'HH:mm'</small>'"

    case true if bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true => "d MMMM yyyy' <small>'HH:mm'</small>'"
  }

  def when(now: Long, date: Date) = date.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString
    else time(date)
  }
}

class WalletActivity extends NfcReaderActivity with TimerActivity { me =>
  lazy val walletPager = findViewById(R.id.walletPager).asInstanceOf[ViewPager]
  lazy val floatingButton = findViewById(R.id.fab).asInstanceOf[FloatingActionMenu]

  lazy val slidingFragmentAdapter =
    new FragmentStatePagerAdapter(getSupportFragmentManager) {
      def getItem(pos: Int) = if (pos == 0) new FragWallet else new FragScan
      def getCount = 2
    }

  override def onDestroy = wrap(super.onDestroy)(stopDetecting)
  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionSettings) makeSettingsForm
  }

  override def onBackPressed =
    if (walletPager.getCurrentItem == 1) walletPager.setCurrentItem(0, true)
    else if (floatingButton.isOpened) floatingButton close true
    else super.onBackPressed

  override def onCreateOptionsMenu(menu: Menu) = {
    // Called after fragLN sets toolbar as actionbar
    getMenuInflater.inflate(R.menu.wallet, menu)
    FragWallet.worker setupSearch menu
    true
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_wallet
    wrap(me setDetecting true)(me initNfc state)
    walletPager setAdapter slidingFragmentAdapter
  } else me exitTo classOf[MainActivity]

  // NFC

  def readEmptyNdefMessage = app toast err_no_data
  def readNonNdefMessage = app toast err_no_data
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNdefMessage(msg: Message) = try {
    val data = readFirstTextNdefMessage(msg)
    app.TransData recordValue data
    me checkTransData null

  } catch { case _: Throwable =>
    // Could not process a message
    app toast err_no_data
  }

  // EXTERNAL DATA CHECK

  def checkTransData(top: View) = {
    walletPager.setCurrentItem(0, false)

    app.TransData.value match {
      case paymentRequest: PaymentRequest => FragWallet.worker sendPayment paymentRequest
      case bu: BitcoinURI => FragWallet.worker sendBtcPopup bu.getAddress setSum Try(bu.getAmount)
      case btcAddress: Address => FragWallet.worker sendBtcPopup btcAddress
      case FragWallet.REDIRECT => goChanDetails(null)
      case _ =>
    }

    app.TransData.value match {
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case otherwise => app.TransData.value = null
    }
  }

  // BUTTONS REACTIONS

  def goReceivePayment(top: View) = {
    val options = Array(getString(btc_receive_option).html, getString(ln_receive_option).html)
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_receive, lst).create)
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options)

    lst setDivider null
    lst setDividerHeight 0
    lst setOnItemClickListener onTap {
      case 0 => generateOnChainBitcoinQR
      case 1 => makeOffChainRequest
    }

    def generateOnChainBitcoinQR = rm(alert) {
      app.TransData.value = app.kit.currentAddress
      me goTo classOf[RequestActivity]
    }

    def makeOffChainRequest = rm(alert) {
      FragWallet.worker.makePaymentRequest
    }
  }

  def goSendPayment(top: View) = {
    val options = Array(send_paste, send_scan_qr) map getString
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_send, lst).create)
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options)

    lst setDivider null
    lst setDividerHeight 0
    lst setOnItemClickListener onTap {
      case 0 => pasteAddressOrPaymentRequest
      case 1 => scanQRCode
    }

    def pasteAddressOrPaymentRequest = rm(alert) {
      app.getBufferTry map app.TransData.recordValue match {
        case Failure(noUsableDataFound) => app toast err_no_data
        case _ => me checkTransData null
      }
    }

    def scanQRCode = rm(alert) {
      walletPager.setCurrentItem(1, true)
    }
  }

  def goChanDetails(top: View) = {
    val nothingToShow = app.ChannelManager.all.isEmpty
    if (nothingToShow) app toast ln_status_none
    else me goTo classOf[LNOpsActivity]
  }

  def goAddChannel(top: View) =
    if (app.ChannelManager.all.isEmpty) {
      val tokens = MilliSatoshi(amount = 500000L)
      val warning = getString(tokens_warn).format(coloredIn apply tokens).html
      val warn = baseTextBuilder(warning).setCustomTitle(me getString action_ln_open)
      mkForm(me goTo classOf[LNStartActivity], none, warn, dialog_ok, dialog_cancel)
    } else me goTo classOf[LNStartActivity]

  def showDenomChooser = {
    val lnTotalMsat = app.ChannelManager.notClosingOrRefunding.map(estimateTotalCanSend).sum
    val walletTotalSum = Satoshi(app.kit.conf0Balance.value + lnTotalMsat / 1000L)

    val walletTotalFiat = msatInFiat(walletTotalSum) match {
      case Success(amt) if fiatName == strYuan => s"<small><font color=#999999>≈ ${formatFiat format amt} cny</font></small>"
      case Success(amt) if fiatName == strEuro => s"<small><font color=#999999>≈ ${formatFiat format amt} eur</font></small>"
      case Success(amt) if fiatName == strYen => s"<small><font color=#999999>≈ ${formatFiat format amt} jpy</font></small>"
      case Success(amt) => s"<small><font color=#999999>≈ ${formatFiat format amt} usd</font></small>"
      case _ => new String
    }

    val title = getLayoutInflater.inflate(R.layout.frag_wallet_state, null)
    val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
    val stateContent = title.findViewById(R.id.stateContent).asInstanceOf[TextView]
    val denomChoiceList = form.findViewById(R.id.choiceList).asInstanceOf[ListView]
    val allDenominations = getResources.getStringArray(R.array.denoms).map(_.html)

    denomChoiceList setOnItemClickListener onTap { pos =>
      // Update denom first so UI update can react to changes
      // also persist user choice in app local data

      denom = denoms(pos)
      FragWallet.worker.updTitle.run
      FragWallet.worker.adapter.notifyDataSetChanged
      app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
    }

    denomChoiceList setAdapter new ArrayAdapter(me, singleChoice, allDenominations)
    denomChoiceList.setItemChecked(app.prefs.getInt(AbstractKit.DENOM_TYPE, 0), true)
    stateContent setText s"${coloredIn apply walletTotalSum}<br>$walletTotalFiat".html
    showForm(negBuilder(dialog_ok, title, form).create)
  }

  // SETTINGS FORM

  def makeSettingsForm = {
    val feePerKb = denom withSign RatesSaver.rates.feeSix
    val title = getString(read_settings).format(feePerKb).html
    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val menu = showForm(negBuilder(dialog_ok, title, form).create)

    val recoverFunds = form.findViewById(R.id.recoverChannelFunds).asInstanceOf[Button]
    val manageOlympus = form.findViewById(R.id.manageOlympus).asInstanceOf[Button]
    val createZygote = form.findViewById(R.id.createZygote).asInstanceOf[Button]
    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]

    manageOlympus setOnClickListener onButtonTap {
      // Just show a list of available Olympus servers
      def proceed = me goTo classOf[OlympusActivity]
      rm(menu)(proceed)
    }

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
              jsonDecoded = AES.decode(encrypted, cloudSecret)
              refundingData <- Try(jsonDecoded) map to[RefundingData]

              // Now throw it away if it is already present in a list of local channels
              if !app.ChannelManager.all.exists(chan => chan(_.channelId) contains refundingData.commitments.channelId)
              chan = app.ChannelManager.createChannel(app.ChannelManager.operationalListeners, refundingData)
              // Start watching this channel's funding tx output right away
              watched = app.kit watchFunding refundingData.commitments
            } app.ChannelManager.all +:= chan
            app.ChannelManager.initConnect
          }, none)

          // Let user know it's happening
          app toast dialog_recovering
        }
      }
    }

    createZygote setOnClickListener onButtonTap {
      def openForm = mkForm(ok = <(createZygote, onFail) { zygote =>
        val zygoteFileShare = new Intent setAction Intent.ACTION_SEND setType "text/plain"
        me startActivity zygoteFileShare.putExtra(Intent.EXTRA_STREAM, Uri fromFile zygote)
      }, none, baseTextBuilder(getString(zygote_details).html), dialog_next, dialog_cancel)

      def createZygote = {
        val zygote = FileOps shell s"zygote ${new Date}.txt"
        val dbFile = new File(app.getDatabasePath(dbFileName).getPath)
        val sourceFilesSeq = Seq(dbFile, app.walletFile, app.chainFile)
        val Seq(dbBytes, walletBytes, chainBytes) = sourceFilesSeq map Files.toByteArray
        val encoded = walletZygoteCodec encode WalletZygote(1, dbBytes, walletBytes, chainBytes)
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

class FragScan extends Fragment with BarcodeCallback { me =>
  type Points = java.util.List[com.google.zxing.ResultPoint]
  lazy val host = getActivity.asInstanceOf[WalletActivity]
  var lastAttempt = System.currentTimeMillis
  var barcodeReader: BarcodeView = _

  override def onCreateView(inflator: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inflator.inflate(R.layout.frag_view_pager_scan, vg, false)

  override def onViewCreated(view: View, savedInstanceState: Bundle) = {
    barcodeReader = view.findViewById(R.id.reader).asInstanceOf[BarcodeView]
    barcodeReader decodeContinuous me
  }

  override def setUserVisibleHint(isVisibleToUser: Boolean) = {
    if (isAdded) if (isVisibleToUser) barcodeReader.resume else {
      getFragmentManager.beginTransaction.detach(me).attach(me).commit
      barcodeReader.pause
    }

    // Remove snapshot traces if stopped
    super.setUserVisibleHint(isVisibleToUser)
  }

  // Only try to decode result after some time
  override def possibleResultPoints(points: Points) = none
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 3000) tryParseQR(rawText)
  }

  def tryParseQR(text: String) = try {
    // May throw which is expected and fine
    lastAttempt = System.currentTimeMillis
    app.TransData recordValue text
    host checkTransData null

  } catch app.TransData.onFail { code =>
    // Inform user about error details
    app toast host.getString(code)
  }
}