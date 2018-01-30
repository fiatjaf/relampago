package com.lightning.wallet

import android.widget._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Tools._
import android.text.format.DateUtils._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.Denomination._
import android.support.v4.view.MenuItemCompat._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import android.widget.AbsListView.OnScrollListener._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._

import android.support.v7.widget.SearchView.OnQueryTextListener
import android.support.v4.app.FragmentStatePagerAdapter
import org.ndeftools.util.activity.NfcReaderActivity
import android.widget.AbsListView.OnScrollListener
import me.relex.circleindicator.CircleIndicator
import org.bitcoinj.store.SPVBlockStore
import com.lightning.wallet.helper.AES
import android.text.format.DateFormat
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat
import org.bitcoinj.core.Address
import fr.acinq.bitcoin.Satoshi
import android.text.InputType
import org.ndeftools.Message
import android.os.Bundle
import java.util.Date

import com.lightning.wallet.lnutils.{CloudDataSaver, PublicCloud, RatesSaver}
import android.provider.Settings.{System => FontSystem}
import android.support.v7.widget.{SearchView, Toolbar}
import android.view.{Menu, MenuItem, View, ViewGroup}
import scala.util.{Success, Try}


trait SearchBar { me =>
  var react: String => Any = none
  private[this] val queryListener = new OnQueryTextListener {
    def onQueryTextChange(qt: String) = runAnd(true)(me react qt)
    def onQueryTextSubmit(qt: String) = true
  }

  def setupSearch(menu: Menu) = {
    val search = getActionView(menu findItem R.id.action_search)
    search.asInstanceOf[SearchView].setOnQueryTextListener(queryListener)
  }
}

trait HumanTimeDisplay {
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

  val host: TimerActivity
  def when(now: Long, date: Date) = date.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString
    else time(date)
  }
}

trait ToolbarFragment extends HumanTimeDisplay { me =>
  // Manages multiple info tickers in toolbar subtitle
  var infos = List.empty[Informer]
  var toolbar: Toolbar = _

  lazy val uitask = host uiTask {
    toolbar setSubtitle infos.head.value
  }

  // Informer CRUD
  def delete(tag: Int) = host uiTask {
    infos = infos.filterNot(_.tag == tag)
    toolbar setSubtitle infos.head.value
  }

  def add(text: String, tag: Int) = {
    infos = new Informer(text, tag) :: infos
    me
  }

  def update(text: String, tag: Int) = runAnd(me) {
    for (info <- infos if info.tag == tag) info.value = text
  }

  def setTitle(titleText: String) = {
    // A workaround to prevent ellipsis
    toolbar setTitle titleText
    toolbar setTitle titleText
  }
}

trait ListUpdater extends HumanTimeDisplay {
  lazy val allTxsWrapper = host.getLayoutInflater.inflate(R.layout.frag_txs_all, null)
  lazy val toggler = allTxsWrapper.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  private[this] var state = SCROLL_STATE_IDLE
  val minLinesNum = 4

  def startListUpdates(list: ListView, adapter: BaseAdapter) = list setOnScrollListener new OnScrollListener {
    def onScroll(absListView: AbsListView, firstListElement: Int, visibleElement: Int, totalElements: Int) = none
    def maybeUpdate = anyToRunnable { if (SCROLL_STATE_IDLE == state) adapter.notifyDataSetChanged }
    def onScrollStateChanged(v: AbsListView, newState: Int) = state = newState
    host.timer.schedule(host uiTask maybeUpdate, 10000, 10000)
    allTxsWrapper setVisibility View.GONE
    list addFooterView allTxsWrapper
  }

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

class WalletActivity extends NfcReaderActivity with TimerActivity { me =>
  lazy val walletPager = findViewById(R.id.walletPager).asInstanceOf[android.support.v4.view.ViewPager]
  lazy val walletPagerIndicator = findViewById(R.id.chanPagerIndicator).asInstanceOf[CircleIndicator]
  lazy val fragBTC = new FragBTC(me)
  lazy val fragLN = new FragLN(me)

  lazy val slidingFragmentAdapter =
    new FragmentStatePagerAdapter(getSupportFragmentManager) {
      def getItem(pos: Int) = if (pos == 0) fragBTC else fragLN
      def getCount = 2
    }

  override def onBackPressed =
    if (walletPager.getCurrentItem == 0) super.onBackPressed
    else walletPager.setCurrentItem(walletPager.getCurrentItem - 1)

  override def onResume = wrap(super.onResume)(checkTransData)
  override def onCreateOptionsMenu(menu: Menu) = runAnd(true) {
    getMenuInflater.inflate(R.menu.ln_normal_ops, menu)
    fragLN setupSearch menu
  }

  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionChanInfo) enterChannelSpace
    else if (m.getItemId == R.id.actionSettings) mkSetsForm
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_wallet
    wrap(me setDetecting true)(me initNfc state)
    walletPager setAdapter slidingFragmentAdapter
    walletPagerIndicator setViewPager walletPager
  } else me exitTo classOf[MainActivity]

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

  def checkTransData = {
    app.TransData.value match {
      case pr: PaymentRequest => if (pr.isFresh) fragLN sendPayment pr else onFail(me getString err_ln_old)
      case link: BitcoinURI => fragBTC.sendBtcPopup.set(Try(link.getAmount), link.getAddress)
      case address: Address => fragBTC.sendBtcPopup setAddress address
      case _ =>
    }

    // Always clear after checking
    app.TransData.value = null
  }

  def goQR(top: View) = me goTo classOf[ScanActivity]
  def goLNOps(top: View) = me goTo classOf[LNOpsActivity]
  def goReceive(top: View) = fragLN.makePaymentRequest.run

  def goReceiveBtcAddress(top: View) = {
    app.TransData.value = app.kit.currentAddress
    me goTo classOf[RequestActivity]
  }

  def tryGoLNStart(top: View) = {
    val minRequired = RatesSaver.rates.feeLive multiply 2
    lazy val required = sumIn.format(denom withSign minRequired)
    lazy val balance = sumIn.format(denom withSign app.kit.conf1Balance)
    lazy val notEnough = getString(err_ln_not_enough_funds).format(balance, required)
    if (app.kit.conf1Balance isGreaterThan minRequired) goLNStart
    else mkForm(me negBld dialog_ok, notEnough.html, null)
  }

  def goLNStart: Unit = cloud match {
    case _: PublicCloud if app.prefs.getBoolean(AbstractKit.TOKENS_WARN, true) =>
      // This is the first time a user tries to open a channel so show tokens warning

      val humanSum = coloredOut apply Satoshi(2000)
      val text = getString(tokens_warn).format(humanSum).html
      showForm(mkChoiceDialog(go, none, dialog_ok, dialog_cancel).setView(text).create)
      def go = runAnd(app.prefs.edit.putBoolean(AbstractKit.TOKENS_WARN, false).commit)(goLNStart)

    // Either private cloud or warning was cleared
    case _ => me goTo classOf[LNStartActivity]
  }

  def showDenomChooser(change: Int => Unit) = {
    val denominations = getResources.getStringArray(R.array.denoms).map(_.html)
    val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
    val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

    lst setOnItemClickListener onTap(change)
    lst setAdapter new ArrayAdapter(me, singleChoice, denominations)
    lst.setItemChecked(app.prefs.getInt(AbstractKit.DENOM_TYPE, 0), true)
    mkForm(me negBld dialog_ok, getString(fiat_set_denom).html, form)
  }

  def enterChannelSpace = {
    val nothingToShow = app.ChannelManager.all.isEmpty
    if (nothingToShow) app toast ln_notify_none
    else me goTo classOf[LNOpsActivity]
  }

  def mkSetsForm: Unit = {
    val leftOps = getResources getStringArray R.array.info_storage_tokens
    val tokensLeft = app.plurOrZero(leftOps, LNParams.cloud.data.tokens.size)

    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val menu = mkForm(me negBld dialog_ok, getString(read_settings).format(tokensLeft).html, form)
    val recoverChannelFunds = form.findViewById(R.id.recoverChannelFunds).asInstanceOf[Button]
    val setBackupServer = form.findViewById(R.id.setBackupServer).asInstanceOf[Button]
    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val changePass = form.findViewById(R.id.changePass).asInstanceOf[Button]

    recoverChannelFunds setOnClickListener onButtonTap {
      // After wallet data is lost users may recover channel funds
      // by fetching encrypted static channel params from server

      rm(menu) {
        val request = LNParams.cloud.connector getBackup LNParams.cloudId.toString
        val localCommitments = app.ChannelManager.all.flatMap(_ apply identity)
        app toast ln_notify_recovering

        request.foreach(serverDataVec => {
          // Decrypt channel datas upon successful call
          // then put them in a list and connect to peers

          for {
            encoded <- serverDataVec
            jsonDecoded = AES.decode(LNParams.cloudSecret)(encoded)
            // This may be some garbage so omit this one if it fails
            refundingData <- Try apply to[RefundingData](jsonDecoded)
            // Now throw it away if it is already present in list of local channels
            if !localCommitments.exists(_.channelId == refundingData.commitments.channelId)
            chan = app.ChannelManager.createChannel(app.ChannelManager.operationalListeners, refundingData)
            isAdded = app.kit watchFunding refundingData.commitments
          } app.ChannelManager.all +:= chan

          // New channels have been added
          // and they need to be reconnected
          app.ChannelManager.initConnect
        }, none)
      }
    }

    setBackupServer setOnClickListener onButtonTap {
      // Power users may provide their own backup servers
      rm(menu)(new SetBackupServer)
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
      // Provided as an external function because it
      // may be accessed directly from mainpage
      rm(menu)(me viewMnemonic null)
    }

    changePass setOnClickListener onButtonTap {
      def openForm = passWrap(me getString sets_secret_change) apply checkPass { oldPass =>
        val view \ field = generatePromptView(InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD, secret_new, null)
        mkForm(mkChoiceDialog(checkNewPass, none, dialog_ok, dialog_cancel), me getString sets_secret_change, view)
        def checkNewPass = if (field.getText.toString.length >= 6) changePassword else app toast secret_too_short

        def changePassword = {
          // Decrypt an old password and set a new one right away
          <(rotatePass, _ => System exit 0)(_ => app toast sets_secret_ok)
          app toast secret_changing
        }

        def rotatePass = {
          app.kit.wallet.decrypt(oldPass)
          // Make sure we have alphabetical keyboard from now on
          app.encryptWallet(app.kit.wallet, field.getText.toString)
          app.prefs.edit.putBoolean(AbstractKit.PASS_INPUT, true).commit
        }
      }

      rm(menu)(openForm)
    }
  }

  class SetBackupServer {
    val view \ field = str2Tuple(LNParams.cloudPrivateKey.publicKey.toString)
    val dialog = mkChoiceDialog(proceed, none, dialog_next, dialog_cancel)
    val alert = mkForm(dialog, getString(ln_olympus_key).html, view)
    field setTextIsSelectable true

    def proceed: Unit = rm(alert) {
      val view1 \ field1 = generatePromptView(InputType.TYPE_CLASS_TEXT, ln_olympus_ip, null)
      val dialog = mkChoiceDialog(trySave(field1.getText.toString), none, dialog_ok, dialog_cancel)
      mkForm(dialog, me getString sets_olympus, view1)
      field1 setText LNParams.cloud.data.url
    }

    def trySave(url1: String) = delayUI {
      val data1 = LNParams.cloud.data.copy(url = url1)
      val cloud1 = LNParams getCloud Success(data1)

      cloud1.checkIfWorks.subscribe(done => {
        // Just send a dummy data with signature
        runOnUiThread(app toast ln_olympus_success)
        CloudDataSaver saveObject data1
        LNParams.cloud = cloud1
      }, onError)
    }

    def onError(error: Throwable) = error.getMessage match {
      case "keynotfound" => onFail(me getString ln_olympus_key_error)
      case "siginvalid" => onFail(me getString ln_olympus_sig_error)
      case _ => onFail(me getString ln_olympus_net_error)
    }
  }
}