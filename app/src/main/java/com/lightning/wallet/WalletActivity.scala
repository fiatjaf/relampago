package com.lightning.wallet

import android.view._
import android.widget._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Tools._
import android.text.format.DateUtils._
import com.journeyapps.barcodescanner._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.Denomination._
import android.support.v4.view.MenuItemCompat._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._

import co.infinum.goldfinger.{Error => GFError}
import android.content.{DialogInterface, Intent}
import co.infinum.goldfinger.{Goldfinger, Warning}
import android.support.v7.widget.{SearchView, Toolbar}
import android.provider.Settings.{System => FontSystem}
import com.lightning.wallet.ln.wire.{NodeAnnouncement, WalletZygote}
import fr.castorflex.android.smoothprogressbar.{SmoothProgressBar, SmoothProgressDrawable}
import com.ogaclejapan.smarttablayout.utils.v4.{FragmentPagerItemAdapter, FragmentPagerItems}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.walletZygoteCodec
import android.support.v4.view.ViewPager.OnPageChangeListener
import android.content.DialogInterface.OnDismissListener
import android.content.Context.LAYOUT_INFLATER_SERVICE
import com.ogaclejapan.smarttablayout.SmartTabLayout
import org.ndeftools.util.activity.NfcReaderActivity
import com.lightning.wallet.ln.Channel.myBalanceMsat
import android.widget.AbsListView.OnScrollListener
import com.google.zxing.client.android.BeepManager
import com.lightning.wallet.lnutils.RatesSaver
import android.view.ViewGroup.LayoutParams
import android.support.v4.view.ViewPager
import org.bitcoinj.store.SPVBlockStore
import android.support.v4.app.Fragment
import com.lightning.wallet.helper.AES
import android.text.format.DateFormat
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import com.google.common.io.Files
import java.text.SimpleDateFormat
import org.bitcoinj.core.Address
import scala.collection.mutable
import android.text.InputType
import org.ndeftools.Message
import android.os.Bundle
import android.net.Uri
import scala.util.Try
import java.util.Date
import java.io.File


trait SearchBar { me =>
  var react: String => Unit = _
  var searchView: SearchView = _

  val queryListener = new SearchView.OnQueryTextListener {
    def onQueryTextChange(ask: String) = runAnd(true)(me react ask)
    def onQueryTextSubmit(ask: String) = true
  }

  def setupSearch(menu: Menu) = {
    val item = menu findItem R.id.action_search
    searchView = getActionView(item).asInstanceOf[SearchView]
    searchView setOnQueryTextListener queryListener
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

trait ToolbarFragment extends HumanTimeDisplay { me =>
  // Manages multiple info tickers in toolbar subtitle
  var infos = List.empty[Informer]
  val toolbar: Toolbar

  // Informer CRUD
  def delete(tag: Int) = host UITask {
    infos = infos.filterNot(_.tag == tag)
    toolbar setSubtitle infos.head.value
  }

  def add(text: String, tag: Int) = host UITask {
    infos = new Informer(text, tag) :: infos
    toolbar setSubtitle infos.head.value
  }

  def update(text: String, tag: Int) = host UITask {
    for (vs <- infos if vs.tag == tag) vs.value = text
    toolbar setSubtitle infos.head.value
  }

  def setTitle(titleText: String) = {
    // A workaround to prevent ellipsis
    toolbar setTitle titleText
    toolbar setTitle titleText
  }
}

trait ListToggler extends HumanTimeDisplay {
  lazy val allTxsWrapper = host.getLayoutInflater.inflate(R.layout.frag_toggler, null)
  lazy val toggler = allTxsWrapper.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  val minLinesNum = 4

  abstract class CutAdapter[T](val max: Int, viewLine: Int) extends BaseAdapter {
    // Automatically switches list view from short to long version and back again
    def switch = cut = if (cut == minLinesNum) max else minLinesNum
    def getItemId(position: Int) = position
    def getCount = visibleItems.size

    var cut = minLinesNum
    var visibleItems = Vector.empty[T]
    var availableItems = Vector.empty[T]

    val set: Vector[T] => Unit = items1 => {
      val visibility = if (items1.size > minLinesNum) View.VISIBLE else View.GONE
      val resource = if (cut == minLinesNum) R.drawable.ic_expand_more_black_24dp
        else R.drawable.ic_expand_less_black_24dp

      allTxsWrapper setVisibility visibility
      toggler setImageResource resource
      visibleItems = items1 take cut
      availableItems = items1
    }

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val view = if (null == savedView) host.getLayoutInflater.inflate(viewLine, null) else savedView
      val hold = if (null == view.getTag) getHolder(view) else view.getTag.asInstanceOf[TxViewHolder]
      hold fillView visibleItems(position)
      view
    }

    def getHolder(view: View): TxViewHolder
    abstract class TxViewHolder(view: View) {
      val transactCircle = view.findViewById(R.id.transactCircle).asInstanceOf[ImageView]
      val transactWhen = view.findViewById(R.id.transactWhen).asInstanceOf[TextView]
      val transactSum = view.findViewById(R.id.transactSum).asInstanceOf[TextView]
      def fillView(data: T): Unit
      view setTag this
    }
  }
}

object WalletActivity {
  val frags = mutable.Set.empty[Fragment]
  def lnOpt = frags collectFirst { case ln: FragLN => ln.worker }
  def btcOpt = frags collectFirst { case btc: FragBTC => btc.worker }
}

class WalletActivity extends NfcReaderActivity with TimerActivity { me =>
  lazy val walletPager = findViewById(R.id.walletPager).asInstanceOf[ViewPager]
  lazy val walletPagerTab = findViewById(R.id.walletPagerTab).asInstanceOf[SmartTabLayout]
  lazy val layoutInflater = app.getSystemService(LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
  lazy val viewParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT)
  lazy val container = findViewById(R.id.container).asInstanceOf[FrameLayout]
  private[this] var listState = OnScrollListener.SCROLL_STATE_IDLE
  private[this] var pagerState = ViewPager.SCROLL_STATE_IDLE
  import WalletActivity.{lnOpt, btcOpt}

  val listListener = new OnScrollListener {
    def onScroll(view: AbsListView, first: Int, vis: Int, total: Int) = none
    def onScrollStateChanged(view: AbsListView, state: Int) = listState = state
  }

  val pagerListener = new OnPageChangeListener {
    def onPageSelected(pos: Int) = app.prefs.edit.putInt(AbstractKit.LANDING, pos).commit
    def onPageScrolled(pos: Int, positionOffset: Float, offsetPixels: Int) = none
    def onPageScrollStateChanged (state: Int) = pagerState = state
  }

  lazy val adapter = {
    val fragScan = classOf[FragScan]
    val fragBTC = classOf[FragBTC]
    val fragLN = classOf[FragLN]

    val items = FragmentPagerItems `with` me
    val ready = items.add("bitcoin", fragBTC).add("lightning", fragLN).add("scanner", fragScan)
    new FragmentPagerItemAdapter(getSupportFragmentManager, ready.create)
  }

  def placeLoader = {
    val loader = layoutInflater.inflate(R.layout.frag_progress_bar, null).asInstanceOf[SmoothProgressBar]
    val drawable: SmoothProgressDrawable = loader.getIndeterminateDrawable.asInstanceOf[SmoothProgressDrawable]
    timer.schedule(drawable.setColors(getResources getIntArray R.array.bar_colors), 100)
    container.addView(loader, viewParams)
    loader
  }

  def removeLoader(loader: SmoothProgressBar) = {
    timer.schedule(container removeView loader, 3000)
    timer.schedule(loader.progressiveStop, 200)
  }

  override def onResume = wrap(super.onResume) {
    val pos = app.prefs.getInt(AbstractKit.LANDING, 0)
    walletPager.setCurrentItem(math.min(pos, 1), false)
  }

  override def onDestroy = wrap(super.onDestroy)(stopDetecting)
  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionSettings) makeSettingsForm
    if (m.getItemId == R.id.actionLNOps) goChanDetails(null)
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    // Called after fragLN sets toolbar as actionbar
    getMenuInflater.inflate(R.menu.ln, menu)
    for (ln <- lnOpt) ln setupSearch menu
    true
  }

  override def onBackPressed =
    if (walletPager.getCurrentItem < 2) super.onBackPressed
    else walletPager.setCurrentItem(walletPager.getCurrentItem - 1)

  def updateListsTime = {
    val notPaging = ViewPager.SCROLL_STATE_IDLE == pagerState
    val notScrolling = OnScrollListener.SCROLL_STATE_IDLE == listState
    if (notPaging && notScrolling) for (ln <- lnOpt) ln.adapter.notifyDataSetChanged
    if (notPaging && notScrolling) for (btc <- btcOpt) btc.adapter.notifyDataSetChanged
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_wallet

    walletPager setAdapter adapter
    walletPager setOffscreenPageLimit 2
    walletPagerTab setViewPager walletPager
    walletPagerTab setOnPageChangeListener pagerListener

    // NFC will happen while app is open so
    // there is no need to run it from fragment
    wrap(me setDetecting true)(me initNfc state)
    timer.schedule(updateListsTime, 10000, 10000)
  } else me exitTo classOf[MainActivity]

  // NFC

  def readEmptyNdefMessage = app toast nfc_error
  def readNonNdefMessage = app toast nfc_error
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNdefMessage(msg: Message) = try {
    val asText = readFirstTextNdefMessage(msg)
    app.TransData recordValue asText
    checkTransData

  } catch { case _: Throwable =>
    // Could not process a message
    app toast nfc_error
  }

  // EXTERNAL DATA CHECK

  def checkTransData =
    app.TransData.value match {
      case request: PaymentRequest =>
        for (ln <- lnOpt) ln.sendPayment(request)
        walletPager.setCurrentItem(1, false)
        app.TransData.value = null

      case addr: Address =>
        for (btc <- btcOpt) btc.sendBtcPopup.setAddress(addr)
        walletPager.setCurrentItem(0, false)
        app.TransData.value = null

      case link: BitcoinURI =>
        val amount: TryMSat = Try(link.getAmount)
        // We have a bitcoin link which may contain a payment sum
        for (btc <- btcOpt) btc.sendBtcPopup.set(amount, link.getAddress)
        walletPager.setCurrentItem(0, false)
        app.TransData.value = null

      case _: NodeAnnouncement =>
        // Don't clear trans data just yet
        walletPager.setCurrentItem(1, false)
        me goTo classOf[LNStartFundActivity]

      case otherwise =>
        // Do nothing here
    }

  //BUTTONS REACTIONS

  def goReceiveLN(top: View) = for (ln <- lnOpt) ln.makePaymentRequest
  def goSendBTC(top: View) = for (btc <- btcOpt) btc.sendBtcPopup

  def goReceiveBTC(top: View) = {
    app.TransData.value = app.kit.currentAddress
    me goTo classOf[RequestActivity]
  }

  def goChanDetails(top: View) = {
    val nothingToShow = app.ChannelManager.all.isEmpty
    if (nothingToShow) app toast ln_status_none
    else me goTo classOf[LNOpsActivity]
  }

  def goAddChannel(top: View) = {
    val minAmount = RatesSaver.rates.feeLive multiply 2
    val isFine = app.kit.conf1Balance isGreaterThan minAmount

    lazy val notEnough = {
      val txt = getString(err_ln_not_enough_funds)
      val zeroConf = app.kit.conf0Balance minus app.kit.conf1Balance
      val canSend = sumIn format denom.withSign(app.kit.conf1Balance)
      val minRequired = sumIn format denom.withSign(minAmount)
      val pending = sumIn format denom.withSign(zeroConf)
      txt.format(canSend, minRequired, pending)
    }

    if (isFine) me goTo classOf[LNStartActivity]
    else mkForm(me negBld dialog_ok, notEnough.html, null)
  }

  def showDenomChooser = {
    val btcFunds = coin2MSat(app.kit.conf0Balance)
    val lnFunds = MilliSatoshi(app.ChannelManager.notClosingOrRefunding.map(myBalanceMsat).sum)
    val btcPrettyFunds = humanFiat(sumIn format denom.withSign(btcFunds), btcFunds, " ")
    val lnPrettyFunds = humanFiat(sumIn format denom.withSign(lnFunds), lnFunds, " ")
    val title = getString(fiat_set_denom).format(btcPrettyFunds, lnPrettyFunds)
    val denominations = getResources.getStringArray(R.array.denoms).map(_.html)
    val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
    val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

    lst setOnItemClickListener onTap { pos =>
      // Persist user setting first and update view

      denom = denoms(pos)
      app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
      for (btc <- btcOpt) wrap(btc.adapter.notifyDataSetChanged)(btc.updTitle)
      for (ln <- lnOpt) wrap(ln.adapter.notifyDataSetChanged)(ln.updTitleSubtitleAndButtons)
    }

    lst setAdapter new ArrayAdapter(me, singleChoice, denominations)
    lst.setItemChecked(app.prefs.getInt(AbstractKit.DENOM_TYPE, 0), true)
    mkForm(me negBld dialog_ok, title.html, form)
  }

  // SETTINGS FORM

  def makeSettingsForm = {
    val leftOps = getResources getStringArray R.array.info_storage_tokens
    val tokensLeft = app.plurOrZero(leftOps, LNParams.cloud.data.tokens.size)

    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val menu = mkForm(me negBld dialog_ok, getString(read_settings).format(tokensLeft).html, form)
    val recoverChannelFunds = form.findViewById(R.id.recoverChannelFunds).asInstanceOf[Button]
    val useFingerprint = form.findViewById(R.id.useFingerprint).asInstanceOf[Button]
    val createZygote = form.findViewById(R.id.createZygote).asInstanceOf[Button]
    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val changePass = form.findViewById(R.id.changePass).asInstanceOf[Button]
    lazy val gf = new Goldfinger.Builder(me).build

    if (gf.hasEnrolledFingerprint) {
      // Only if basic prerequisites are here
      useFingerprint setVisibility View.VISIBLE
      if (FingerPassCode.exists) setButtonDisable
      else setButtonEnable

      def setButtonEnable = {
        // Offer user to enter a passcode and fingerprint
        useFingerprint setOnClickListener onButtonTap(proceed)
        useFingerprint setText fp_enable

        def proceed = rm(menu) {
          passWrap(me getString fp_enable) apply checkPass { pass =>
            // The password is guaranteed to be correct here, proceed with auth
            val content = getLayoutInflater.inflate(R.layout.frag_touch, null)
            lazy val alert = mkForm(dlg, getString(fp_enable).html, content)
            lazy val dlg = me negBld dialog_cancel

            val callback = new Goldfinger.Callback {
              def onWarning(warn: Warning) = FingerPassCode informUser warn
              def onSuccess(cipher: String) = runAnd(alert.dismiss)(FingerPassCode record cipher)
              def onError(err: GFError) = runAnd(alert.dismiss)(FingerPassCode informUser err)
            }

            dlg setOnDismissListener new OnDismissListener {
              def onDismiss(dialog: DialogInterface) = gf.cancel
              gf.encrypt(fileName, pass, callback)
              alert
            }
          }
        }
      }

      def setButtonDisable = {
        def proceed = rm(menu)(FingerPassCode.erase)
        useFingerprint setOnClickListener onButtonTap(proceed)
        useFingerprint setText fp_disable
      }
    }

    recoverChannelFunds setOnClickListener onButtonTap {
      // When wallet data is lost users may recover channel funds
      // by fetching encrypted static channel params from server

      rm(menu)(app toast dialog_recovering)
      cloud.connector.getBackup(cloudId).foreach(serverDataVec => {
        // Decrypt channel recovery datas upon successful call and put
        // them into an active channel list, then connect to peers

        for {
          encrypted <- serverDataVec
          jsonDecoded = AES.decode(encrypted, cloudSecret)
          // This may be some garbage so omit this one if it fails
          refundingData <- Try apply to[RefundingData](jsonDecoded)
          // Now throw it away if it is already present in list of local channels
          if !app.ChannelManager.all.exists(chan => chan(_.channelId) contains refundingData.commitments.channelId)
          chan = app.ChannelManager.createChannel(app.ChannelManager.operationalListeners, refundingData)
          // Start watching this channel's funding tx output right away
          isAdded = app.kit watchFunding refundingData.commitments
        } app.ChannelManager.all +:= chan

        // New channels have been added
        // so they need to be reconnected
        app.ChannelManager.initConnect
      }, none)
    }

    createZygote setOnClickListener onButtonTap {
      def openForm = showForm(mkChoiceDialog(ok = <(createZygote, onFail) { zygote =>
        val share = new Intent setAction Intent.ACTION_SEND_MULTIPLE setType "text/plain"
        val data = new java.util.ArrayList[Uri]
        data.add(Uri fromFile zygote)

        // Explain all the caveates to user before procceding with zygote sharing
        me startActivity share.putParcelableArrayListExtra(Intent.EXTRA_STREAM, data)
      }, none, dialog_ok, dialog_cancel).setMessage(zygote_details).create)

      def createZygote = {
        val backupFile = FileOps shell s"zygote ${new Date}.txt"
        val dbFile = new File(app.getDatabasePath(dbFileName).getPath)
        val sourceFilesSeq = Seq(dbFile, app.walletFile, app.chainFile)
        val Seq(dbBytes, walletBytes, chainBytes) = sourceFilesSeq map Files.toByteArray
        val encoded = walletZygoteCodec encode WalletZygote(1, dbBytes, walletBytes, chainBytes)
        Files.write(encoded.require.toByteArray, backupFile)
        backupFile
      }

      rm(menu)(openForm)
    }

    rescanWallet setOnClickListener onButtonTap {
      def openForm = passWrap(me getString sets_rescan) apply checkPass { pass =>
        val dlg = mkChoiceDialog(go, none, dialog_ok, dialog_cancel).setMessage(sets_rescan_ok)
        showForm(dlg.create)
      }

      def go = try {
        app.chainFile.delete
        app.kit.wallet.reset
        app.kit.store = new SPVBlockStore(app.params, app.chainFile)
        app.kit useCheckPoints app.kit.wallet.getEarliestKeyCreationTime
        app.kit.wallet saveToFile app.walletFile
      } catch none finally System exit 0

      rm(menu)(openForm)
    }

    viewMnemonic setOnClickListener onButtonTap {
      // Provided as an external function because
      // it may be accessed directly from mainpage
      rm(menu)(me viewMnemonic null)
    }

    changePass setOnClickListener onButtonTap {
      def openForm = passWrap(me getString sets_secret_change) apply checkPass { oldPass =>
        val view \ field = generatePromptView(InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD, secret_new, null)
        mkForm(mkChoiceDialog(react, none, dialog_ok, dialog_cancel), getString(sets_secret_change), view)
        def react = if (field.getText.length >= 6) changePassword else app toast secret_too_short

        def changePassword = {
          // Decrypt an old password and set a new one right away
          <(rotatePass, onFail)(_ => app toast sets_secret_ok)
          app toast secret_changing
        }

        def rotatePass = {
          app.kit.wallet decrypt oldPass
          app.encryptWallet(app.kit.wallet, field.getText.toString)
          // Make sure we definitely have alphabetical keyboard for now
          app.prefs.edit.putBoolean(AbstractKit.PASS_INPUT, true).commit
          // Remove an old encrypted passcode
          FingerPassCode.erase
        }
      }

      rm(menu)(openForm)
    }
  }
}

class FragScan extends Fragment with BarcodeCallback { me =>
  type Points = java.util.List[com.google.zxing.ResultPoint]
  lazy val host = getActivity.asInstanceOf[WalletActivity]
  lazy val beepManager = new BeepManager(host)
  var lastAttempt = System.currentTimeMillis
  var barcodeReader: BarcodeView = _

  override def onCreateView(inflator: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inflator.inflate(R.layout.frag_view_pager_scan, vg, false)

  override def onViewCreated(view: View, savedInstanceState: Bundle) = {
    barcodeReader = view.findViewById(R.id.reader).asInstanceOf[BarcodeView]
    barcodeReader decodeContinuous me
  }

  override def setUserVisibleHint(isVisibleToUser: Boolean) = {
    if (barcodeReader != null) if (isVisibleToUser) barcodeReader.resume else barcodeReader.pause
    if (!isVisibleToUser) getFragmentManager.beginTransaction.detach(me).attach(me).commit
    super.setUserVisibleHint(isVisibleToUser)
  }

  // Only try to decode result after 2 seconds
  override def possibleResultPoints(points: Points) = none
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 2000) tryParseQR(rawText)
  }

  def tryParseQR(scannedText: String) = try {
    // This may throw which is expected and fine

    barcodeReader.pause
    beepManager.playBeepSound
    lastAttempt = System.currentTimeMillis
    app.TransData recordValue scannedText
    host.checkTransData

  } catch app.TransData.onFail { code =>
    val builder = host negBld dialog_ok setMessage code
    host.walletPager.setCurrentItem(1, false)
    host showForm builder.create
  }
}