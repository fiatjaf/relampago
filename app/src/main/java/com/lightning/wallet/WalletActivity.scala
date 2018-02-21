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

import android.support.v7.widget.{SearchView, Toolbar}
import android.provider.Settings.{System => FontSystem}
import fr.castorflex.android.smoothprogressbar.{SmoothProgressBar, SmoothProgressDrawable}
import com.ogaclejapan.smarttablayout.utils.v4.{FragmentPagerItemAdapter, FragmentPagerItems}

import android.support.v7.widget.SearchView.OnQueryTextListener
import android.support.v4.view.ViewPager.OnPageChangeListener
import android.content.Context.LAYOUT_INFLATER_SERVICE
import com.lightning.wallet.ln.wire.NodeAnnouncement
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
import java.text.SimpleDateFormat
import org.bitcoinj.core.Address
import scala.collection.mutable
import android.text.InputType
import org.ndeftools.Message
import android.os.Bundle
import scala.util.Try
import java.util.Date


trait SearchBar { me =>
  var react: String => Unit = _
  private[this] val queryListener = new OnQueryTextListener {
    def onQueryTextChange(query: String) = runAnd(true)(me react query)
    def onQueryTextSubmit(query: String) = true
  }

  def setupSearch(menu: Menu) = {
    val search = getActionView(menu findItem R.id.action_search)
    search.asInstanceOf[SearchView].setOnQueryTextListener(queryListener)
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
    if (walletPager.getCurrentItem <= 1) super.onBackPressed
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

  def checkTransData = {
    app.TransData.value match {
      case link: BitcoinURI => for (btc <- btcOpt) btc.sendBtcPopup.set(Try(link.getAmount), link.getAddress)
      case bitcoinAddress: Address => for (btc <- btcOpt) btc.sendBtcPopup.setAddress(bitcoinAddress)
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case pr: PaymentRequest => for (ln <- lnOpt) ln.sendPayment(pr)
      case _ =>
    }

    app.TransData.value match {
      case _: NodeAnnouncement => // Do nothing
      case _ => app.TransData.value = null
    }
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
    val minRequired = RatesSaver.rates.feeLive multiply 2
    val isFine = app.kit.conf1Balance isGreaterThan minRequired

    lazy val notEnough = {
      val txt = me getString err_ln_not_enough_funds
      val pending = app.kit.conf0Balance minus app.kit.conf1Balance
      val balanceHuman = sumIn format denom.withSign(app.kit.conf1Balance)
      val requiredHuman = sumIn format denom.withSign(minRequired)
      val pendingHuman = sumIn format denom.withSign(pending)
      txt.format(balanceHuman, pendingHuman, requiredHuman)
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
      for (ln <- lnOpt) wrap(ln.adapter.notifyDataSetChanged)(ln.updTitleSubtitleAbdButtons)
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
    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val changePass = form.findViewById(R.id.changePass).asInstanceOf[Button]

    recoverChannelFunds setOnClickListener onButtonTap {
      // When wallet data is lost users may recover channel funds
      // by fetching encrypted static channel params from server

      rm(menu)(app toast dialog_recovering)
      cloud.connector.getBackup(cloudId).foreach(serverDataVec => {
        // Decrypt channel recovery datas upon successful call and put
        // them into an active channel list, then connect to peers

        for {
          encoded <- serverDataVec
          jsonDecoded = AES.decode(cloudSecret)(encoded)
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
        mkForm(mkChoiceDialog(if (field.getText.length >= 6) changePassword else app toast secret_too_short,
          none, dialog_ok, dialog_cancel), getString(sets_secret_change), view)

        def changePassword = {
          // Decrypt an old password and set a new one right away
          <(rotatePass, _ => System exit 0)(_ => app toast sets_secret_ok)
          app toast secret_changing
        }

        def rotatePass = {
          app.kit.wallet decrypt oldPass
          // Make sure we have alphabetical keyboard from now on
          app.encryptWallet(app.kit.wallet, field.getText.toString)
          app.prefs.edit.putBoolean(AbstractKit.PASS_INPUT, true).commit
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