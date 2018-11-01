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
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import org.bitcoinj.core.{Address, TxWrap}
import com.lightning.walletapp.lnutils.IconGetter.{bigFont, scrWidth}
import com.lightning.walletapp.ln.wire.{NodeAnnouncement, Started, WalletZygote}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.walletZygoteCodec
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import android.support.v4.app.FragmentStatePagerAdapter
import com.lightning.walletapp.Denomination.coin2MSat
import org.ndeftools.util.activity.NfcReaderActivity
import android.support.v4.content.FileProvider
import com.github.clans.fab.FloatingActionMenu
import android.support.v7.widget.SearchView
import com.lightning.walletapp.helper.AES
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.store.SPVBlockStore
import android.text.format.DateFormat
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat
import com.google.common.io.Files
import android.content.Intent
import org.ndeftools.Message
import android.os.Bundle
import java.util.Date
import java.io.File


trait SearchBar { me =>
  var react: String => Unit = _
  var searchView: SearchView = _

  // This may be queried before search menu is created so null check
  def isSearching = searchView != null && !searchView.isIconified

  def setupSearch(menu: Menu) = {
    val item = menu findItem R.id.action_search
    searchView = item.getActionView.asInstanceOf[SearchView]
    searchView setOnQueryTextListener new SearchView.OnQueryTextListener {
      def onQueryTextChange(queryText: String) = runAnd(true)(me react queryText)
      def onQueryTextSubmit(queryText: String) = true
    }
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
  lazy val floatingActionMenu = findViewById(R.id.fam).asInstanceOf[FloatingActionMenu]
  lazy val slidingFragmentAdapter = new FragmentStatePagerAdapter(getSupportFragmentManager) {
    def getItem(currentFragmentPos: Int) = if (0 == currentFragmentPos) new FragWallet else new FragScan
    def getCount = 2
  }

  override def onDestroy = wrap(super.onDestroy)(stopDetecting)
  override def onResume = wrap(super.onResume)(me returnToBase null)
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
      _ => app toast err_no_data)(_ => checkTransData)

  // EXTERNAL DATA CHECK

  def checkTransData = {
    app.TransData.value match {
      case _: LNUrl => me returnToBase null
      case _: Address => me returnToBase null
      case _: BitcoinURI => me returnToBase null
      case _: PaymentRequest => me returnToBase null
      case _ => // Switching activity
    }

    app.TransData checkAndMaybeErase {
      case FragWallet.REDIRECT => goOps(null)
      case _: Started => me goTo classOf[LNStartActivity]
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case lnLink: LNUrl => lnLink.resolve.foreach(initConnection, none)
      case address: Address => FragWallet.worker.sendBtcPopup(address)(none)

      case uri: BitcoinURI =>
        // Bitcoin URI may possibly have an amount which we then fill in
        val manager = FragWallet.worker.sendBtcPopup(uri.getAddress)(none)
        manager setSum scala.util.Try(uri.getAmount)

      case pr: PaymentRequest if ChannelManager.notClosingOrRefunding.nonEmpty =>
        // We have open or at least opening channels so show a form or message to user
        FragWallet.worker sendPayment pr

      case pr: PaymentRequest =>
        // TransData should be set to batch or null to erase previous
        app.TransData.value = TxWrap findBestBatch pr getOrElse null
        me goTo classOf[LNStartActivity]
        app toast ln_empty

      case _ =>
    }
  }

  def initConnection(icr: IncomingChannelRequest) =
    ConnectionManager.listeners += new ConnectionListener { self =>
      // This is done to make sure we definitely have an LN connection
      ConnectionManager.connectTo(icr.getAnnounce, notify = true)
      app toast ln_url_resolving

      override def onOperational(nodeId: PublicKey) = {
        // Immediately remove listener and make a request
        // their OpenChannel message should arrive shortly
        ConnectionManager.listeners -= self
        icr.requestChannel
      }
    }

  // BUTTONS REACTIONS

  def goReceivePayment(top: View) = {
    val operationalChannels = ChannelManager.notClosingOrRefunding.filter(isOperational)
    val operationalChannelsWithRoutes: Map[Channel, PaymentRoute] = operationalChannels.flatMap(channelAndHop).toMap
    val maxCanReceiveMsat = operationalChannelsWithRoutes.keys.map(estimateCanReceiveCapped).reduceOption(_ max _) getOrElse 0L
    val maxCanReceive = MilliSatoshi(maxCanReceiveMsat)

    val reserveUnspent = getString(ln_receive_reserve) format coloredOut(-maxCanReceive) // Negate to cancel out a minus
    val lnReceiveText = if (operationalChannels.isEmpty) getString(ln_receive_option).format(me getString ln_no_open_chans)
      else if (operationalChannelsWithRoutes.isEmpty) getString(ln_receive_option).format(me getString ln_receive_6conf)
      else if (maxCanReceiveMsat < 0L) getString(ln_receive_option).format(reserveUnspent)
      else getString(ln_receive_option).format(me getString ln_receive_ok)

    val options = Array(lnReceiveText.html, getString(btc_receive_option).html)
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = showForm(negBuilder(dialog_cancel, me getString action_coins_receive, lst).create)

    lst setDivider null
    lst setDividerHeight 0
    lst setOnItemClickListener onTap { case 0 => offChain case 1 => onChain }
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, options)

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
      val resultTry = app.getBufferTry map app.TransData.recordValue
      if (resultTry.isSuccess) checkTransData else app toast err_no_data
    }

    def scanQR = rm(alert) {
      // Just jump to QR scanner section
      walletPager.setCurrentItem(1, true)
    }
  }

  val tokensPrice = MilliSatoshi(1000000L)
  def goLNStart: Unit = me goTo classOf[LNStartActivity]
  def goOps(top: View): Unit = me goTo classOf[LNOpsActivity]
  def goAddChannel(top: View) = if (app.olympus.backupExhausted) {
    val humanPrice = s"${coloredIn apply tokensPrice} <font color=#999999>${msatInFiatHuman apply tokensPrice}</font>"
    val warn = baseTextBuilder(getString(tokens_warn).format(humanPrice).html).setCustomTitle(me getString action_ln_open)
    mkCheckForm(alert => rm(alert)(goLNStart), none, warn, dialog_ok, dialog_cancel)
  } else goLNStart

  // SETTINGS FORM

  def makeSettingsForm = {
    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val allDenoms = getResources.getStringArray(R.array.denoms).map(_.html)
    val menu = showForm(baseBuilder(getString(read_settings).html, form).create)

    val choiceList = form.findViewById(R.id.choiceList).asInstanceOf[ListView]
    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val manageOlympus = form.findViewById(R.id.manageOlympus).asInstanceOf[Button]
    val recoverFunds = form.findViewById(R.id.recoverChannelFunds).asInstanceOf[Button]
    val exportWalletSnapshot = form.findViewById(R.id.exportWalletSnapshot).asInstanceOf[Button]
    recoverFunds.setEnabled(ChannelManager.currentBlocksLeft < broadcaster.blocksPerDay)

    recoverFunds setOnClickListener onButtonTap {
      def recover = app.olympus getBackup cloudId foreach { backups =>
        // Decrypt channel recovery data and put it to channels list if it is not present already
        // then try to get new NodeAnnouncement for refunding channels, otherwise use an old one

        for {
          encryptedBackup <- backups
          ref <- AES.decHex2Readable(encryptedBackup, cloudSecret) map to[RefundingData]
          if !ChannelManager.all.flatMap(_ apply identity).exists(_.channelId == ref.commitments.channelId)
        } ChannelManager.all +:= ChannelManager.createChannel(ChannelManager.operationalListeners, ref)

        for {
          chan <- ChannelManager.all if chan.state == REFUNDING
          // Try to connect right away and maybe use new address later
          _ = ConnectionManager.connectTo(chan.data.announce, notify = false)
          // Can call findNodes without `retry` wrapper because it gives `Obs.empty` on error
          Vector(ann1 \ _, _*) <- app.olympus findNodes chan.data.announce.nodeId.toString
        } chan process ann1
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

    exportWalletSnapshot setOnClickListener onButtonTap {
      // Users may export a whole wallet snapshot and restore later
      // but warn them about all the risks involved before proceeding
      rm(menu)(openForm)

      def openForm = {
        def proceed = <(createZygote, onFail)(none)
        val bld = me baseTextBuilder getString(migrator_usage_warning).html
        mkCheckForm(alert => rm(alert)(proceed), none, bld, dialog_next, dialog_cancel)
      }

      def createZygote = {
        val dbFile = new File(app.getDatabasePath(dbFileName).getPath)
        val sourceFilesSeq = Seq(dbFile, app.walletFile, app.chainFile)
        val Seq(dbBytes, walletBytes, chainBytes) = sourceFilesSeq map Files.toByteArray
        val encoded = walletZygoteCodec encode WalletZygote(1, dbBytes, walletBytes, chainBytes)

        val name = s"Bitcoin Wallet Snapshot ${new Date}.txt"
        val walletSnapshotFilePath = new File(getCacheDir, "images")
        if (!walletSnapshotFilePath.isFile) walletSnapshotFilePath.mkdirs
        val savedFile = new File(walletSnapshotFilePath, name)
        Files.write(encoded.require.toByteArray, savedFile)

        val fileURI = FileProvider.getUriForFile(me, "com.lightning.walletapp", savedFile)
        val share = new Intent setAction Intent.ACTION_SEND addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
        share.putExtra(Intent.EXTRA_STREAM, fileURI).setDataAndType(fileURI, getContentResolver getType fileURI)
        me startActivity Intent.createChooser(share, "Choose an app")
      }
    }

    def updateDenomination(pos: Int) = {
      // Update denom so UI update can react to changes
      // then persist user choice in local data storage

      denom = denoms(pos)
      app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
      FragWallet.worker.adapter.notifyDataSetChanged
      FragWallet.worker.updTitle.run
    }

    choiceList setAdapter new ArrayAdapter(me, singleChoice, allDenoms)
    choiceList.setItemChecked(app.prefs.getInt(AbstractKit.DENOM_TYPE, 0), true)
    choiceList setOnItemClickListener onTap(updateDenomination)
  }
}