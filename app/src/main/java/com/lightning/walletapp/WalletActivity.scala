package com.lightning.walletapp

import android.view._
import android.widget._
import com.lightning.walletapp.ln._
import android.text.format.DateUtils._
import com.lightning.walletapp.Utils._
import com.journeyapps.barcodescanner._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.Denomination._
import android.support.v4.view.MenuItemCompat._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import android.support.v7.widget.{SearchView, Toolbar}
import android.provider.Settings.{System => FontSystem}
import com.lightning.walletapp.ln.wire.{NodeAnnouncement, WalletZygote}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.walletZygoteCodec
import com.lightning.walletapp.ln.Channel.estimateTotalCanSend
import android.support.v4.view.ViewPager.OnPageChangeListener
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import org.ndeftools.util.activity.NfcReaderActivity
import android.widget.AbsListView.OnScrollListener
import com.lightning.walletapp.lnutils.RatesSaver
import com.lightning.walletapp.helper.AES
import android.support.v4.view.ViewPager
import org.bitcoinj.store.SPVBlockStore
import android.support.v4.app.{Fragment, FragmentStatePagerAdapter}
import android.text.format.DateFormat
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import com.google.common.io.Files
import java.text.SimpleDateFormat

import org.bitcoinj.core.Address

import scala.collection.mutable
import android.content.Intent
import org.ndeftools.Message
import android.os.Bundle
import android.net.Uri
import java.util.Date

import scala.util.Try
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
  val REDIRECT = "goToLnOpsActivity"
  var walletFrag = Option.empty[FragWallet]
}

class WalletActivity extends NfcReaderActivity with TimerActivity { me =>
  lazy val walletPager = findViewById(R.id.walletPager).asInstanceOf[ViewPager]

  lazy val slidingFragmentAdapter =
    new FragmentStatePagerAdapter(getSupportFragmentManager) {
      def getItem(pos: Int) = if (pos == 0) new FragWallet else new FragScan
      def getCount = 2
    }

  override def onDestroy = wrap(super.onDestroy)(stopDetecting)
  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionSettings) makeSettingsForm
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    // Called after fragLN sets toolbar as actionbar
    getMenuInflater.inflate(R.menu.wallet, menu)
    true
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_wallet
    wrap(me setDetecting true)(me initNfc state)
    walletPager setAdapter slidingFragmentAdapter
  } else me exitTo classOf[MainActivity]

  // NFC

  def readEmptyNdefMessage = app toast nfc_error
  def readNonNdefMessage = app toast nfc_error
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNdefMessage(msg: Message) = try {
    val data: String = readFirstTextNdefMessage(msg)
    app.TransData recordValue data
    checkTransData

  } catch { case _: Throwable =>
    // Could not process a message
    app toast nfc_error
  }

  // EXTERNAL DATA CHECK

  def checkTransData = {
    walletPager.setCurrentItem(1, false)

    app.TransData.value match {
      case _: NodeAnnouncement => // nope
      case _ => app.TransData.value = null
    }
  }

  // BUTTONS REACTIONS

  def goReceiveBTC(top: View) = {
    app.TransData.value = app.kit.currentAddress
    me goTo classOf[RequestActivity]
  }

  def goChanDetails(top: View) = {
    val nothingToShow = app.ChannelManager.all.isEmpty
    if (nothingToShow) app toast ln_status_none
    else me goTo classOf[LNOpsActivity]
  }

  def goAddChannel(top: View) =
    if (app.ChannelManager.all.isEmpty) {
      val tokens = MilliSatoshi(amount = 500000L)
      val humanIn = humanFiat(coloredIn(tokens), tokens, " ")
      val warning = getString(tokens_warn).format(humanIn).html
      val warn = baseTextBuilder(warning).setCustomTitle(me getString action_ln_open)
      mkForm(me goTo classOf[LNStartActivity], none, warn, dialog_ok, dialog_cancel)
    } else me goTo classOf[LNStartActivity]

  def showDenomChooser = {
    val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
    val denomChoiceList = form.findViewById(R.id.choiceList).asInstanceOf[ListView]
    val allDenominations = getResources.getStringArray(R.array.denoms).map(_.html)


    denomChoiceList setOnItemClickListener onTap { pos =>
      app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
      denom = denoms(pos)
    }

    denomChoiceList setAdapter new ArrayAdapter(me, singleChoice, allDenominations)
    denomChoiceList.setItemChecked(app.prefs.getInt(AbstractKit.DENOM_TYPE, 0), true)
    showForm(negBuilder(dialog_ok, getString(wallet_unit), form).create)
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

        def recover: Unit = {
          OlympusWrap.getBackup(cloudId).foreach(backups => {
            // Decrypt channel recovery data upon successful call and put
            // them into an active channel list, then connect to peers

            for {
              encrypted <- backups
              jsonDecoded = AES.decode(encrypted, cloudSecret)
              refundingData = to[RefundingData](jsonDecoded)

              // Now throw it away if it is already present in a list of local channels
              if !app.ChannelManager.all.exists(chan => chan(_.channelId) contains refundingData.commitments.channelId)
              chan = app.ChannelManager.createChannel(app.ChannelManager.operationalListeners, refundingData)
              // Start watching this channel's funding tx output right away
              _ = app.kit watchFunding refundingData.commitments
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

  // Only try to decode result after 2 seconds
  override def possibleResultPoints(points: Points) = none
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 3000) tryParseQR(rawText)
  }

  def tryParseQR(text: String) = try {
    // May throw which is expected and fine
    lastAttempt = System.currentTimeMillis
    app.TransData recordValue text
    host.checkTransData

  } catch app.TransData.onFail { code =>
    // Inform user about error details
    app toast host.getString(code)
  }
}