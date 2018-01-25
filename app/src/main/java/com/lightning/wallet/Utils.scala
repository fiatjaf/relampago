package com.lightning.wallet

import R.string._
import android.text._
import android.view._
import android.widget._
import org.bitcoinj.core._
import android.text.method._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import org.bitcoinj.core.listeners._
import org.bitcoinj.wallet.listeners._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._

import com.lightning.wallet.lnutils.{CloudDataSaver, RatesSaver}
import android.content.{Context, DialogInterface, Intent}
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import R.id.{typeCNY, typeEUR, typeJPY, typeUSD}
import fr.acinq.bitcoin.{Crypto, MilliSatoshi}
import scala.util.{Failure, Success, Try}
import android.app.{AlertDialog, Dialog}
import java.util.{Timer, TimerTask}

import org.bitcoinj.wallet.Wallet.ExceededMaxTransactionSize
import org.bitcoinj.wallet.Wallet.CouldNotAdjustDownwards
import android.widget.RadioGroup.OnCheckedChangeListener
import android.widget.AdapterView.OnItemClickListener
import com.lightning.wallet.lnutils.JsonHttpUtils.to
import info.hoang8f.android.segmented.SegmentedGroup
import concurrent.ExecutionContext.Implicits.global
import android.view.inputmethod.InputMethodManager
import com.lightning.wallet.ln.LNParams.minDepth
import android.support.v7.app.AppCompatActivity
import org.bitcoinj.crypto.KeyCrypterException
import fr.acinq.bitcoin.Crypto.PublicKey
import android.support.v7.widget.Toolbar
import android.view.View.OnClickListener
import org.bitcoinj.store.SPVBlockStore
import android.app.AlertDialog.Builder
import com.lightning.wallet.helper.AES
import language.implicitConversions
import android.util.DisplayMetrics
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.script.Script
import scala.concurrent.Future
import android.os.Bundle

import ViewGroup.LayoutParams.WRAP_CONTENT
import InputMethodManager.HIDE_NOT_ALWAYS
import Context.INPUT_METHOD_SERVICE


object Utils {
  type TryMSat = Try[MilliSatoshi]
  var appReference: WalletApp = _
  var denom: Denomination = _
  var fiatName: String = _

  lazy val app = appReference
  lazy val appName = app getString app_name
  lazy val sumIn = app getString txs_sum_in
  lazy val sumOut = app getString txs_sum_out
  lazy val denoms = List(SatDenomination, FinDenomination, BtcDenomination)
  val coloredOut: MilliSatoshi => String = amt => sumOut.format(denom withSign amt)
  val coloredIn: MilliSatoshi => String = amt => sumIn.format(denom withSign amt)
  val singleChoice = android.R.layout.select_dialog_singlechoice

  // Mapping from text to Android id integer
  val Seq(strDollar, strEuro, strYen, strYuan) = Seq("dollar", "euro", "yen", "yuan")
  val fiatMap = Map(typeUSD -> strDollar, typeEUR -> strEuro, typeJPY -> strYen, typeCNY -> strYuan)
  val revFiatMap = Map(strDollar -> typeUSD, strEuro -> typeEUR, strYen -> typeJPY, strYuan -> typeCNY)
  val passNoSuggest = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD
  def isMnemonicCorrect(mnemonic: String) = mnemonic.split("\\s+").length > 11

  def humanAddr(adr: Address) = s"$adr" grouped 4 mkString "\u0020"
  def humanNode(nodeId: PublicKey, separator: String) = nodeId.toString
    .grouped(24).map(_ grouped 3 mkString "\u0020").mkString(separator)

  def clickableTextField(view: View): TextView = {
    val textView: TextView = view.asInstanceOf[TextView]
    textView setMovementMethod LinkMovementMethod.getInstance
    textView
  }

  def currentRate: Try[Double] = Try(RatesSaver.rates exchange fiatName)
  def msatInFiat(msat: MilliSatoshi) = currentRate.map(perBtc => msat.amount * perBtc / btc2msatFactor)
  def humanFiat(prefix: String, ms: MilliSatoshi, div: String = "<br>"): String = msatInFiat(ms) match {
    case Success(amt) if fiatName == strYuan => s"$prefix$div<font color=#999999>≈ ${formatFiat format amt} CNY</font>"
    case Success(amt) if fiatName == strEuro => s"$prefix$div<font color=#999999>≈ ${formatFiat format amt} EUR</font>"
    case Success(amt) if fiatName == strYen => s"$prefix$div<font color=#999999>≈ ${formatFiat format amt} JPY</font>"
    case Success(amt) => s"$prefix$div<font color=#999999>≈ ${formatFiat format amt} USD</font>"
    case _ => prefix
  }
}

trait ToolbarActivity extends TimerActivity { me =>
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  lazy val flash = uiTask(getSupportActionBar setSubtitle infos.head.value)
  private[this] var infos = List.empty[Informer]

  val catchListener = new BlocksListener {
    def getNextTracker(initBlocksLeft: Int) = new BlocksListener {
      def onBlocksDownloaded(peer: Peer, block: Block, fb: FilteredBlock, blocksLeft: Int) = {
        if (blocksLeft % blocksPerDay == 0) update(app.plurOrZero(syncOps, blocksLeft / blocksPerDay), Informer.CHAINSYNC)
        if (blocksLeft < 1) add(me getString info_progress_done, Informer.CHAINSYNC).timer.schedule(delete(Informer.CHAINSYNC), 5000)
        if (blocksLeft < 1) app.kit.peerGroup removeBlocksDownloadedEventListener this
        flash.run
      }

      // We only add a SYNC item if we have a large enough
      // lag (more than a day), otherwise no updates are visible
      private val syncOps = app.getResources getStringArray R.array.info_progress
      private val text = app.plurOrZero(syncOps, initBlocksLeft / blocksPerDay)
      if (initBlocksLeft > blocksPerDay) add(text, Informer.CHAINSYNC)
    }

    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = {
      app.kit.peerGroup addBlocksDownloadedEventListener getNextTracker(left)
      app.kit.peerGroup removeBlocksDownloadedEventListener this
    }
  }

  val constListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerConnected(peer: Peer, peerCount: Int) = update(me getString status, Informer.PEER).flash.run
    def onPeerDisconnected(peer: Peer, peerCount: Int) = update(me getString status, Informer.PEER).flash.run
    def status = if (app.kit.peerGroup.numConnectedPeers < 1) btc_notify_connecting else btc_notify_operational
  }

  val txTracker = new TxTracker {
    override def coinsSent(tx: Transaction) = notifySubTitle(me getString tx_sent, Informer.BTCEVENT)
    override def coinsReceived(tx: Transaction) = notifySubTitle(me getString tx_received, Informer.BTCEVENT)
  }

  // Informer CRUD
  def delete(tag: Int) = uiTask {
    infos = infos.filterNot(_.tag == tag)
    getSupportActionBar setSubtitle infos.head.value
  }

  def add(text: String, tag: Int) = runAnd(me) {
    infos = new Informer(text, tag) :: infos
  }

  def update(text: String, tag: Int) = runAnd(me) {
    for (info <- infos if info.tag == tag) info.value = text
  }

  def setTitle(titleText: String) = {
    // A workaround to prevent ellipsis
    getSupportActionBar setTitle titleText
    getSupportActionBar setTitle titleText
  }

  def notifySubTitle(subtitle: String, infoType: Int)
  def showDenominationChooser(change: Int => Unit) = {
    val denominations = getResources.getStringArray(R.array.denoms).map(_.html)
    val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
    val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

    lst setOnItemClickListener onTap(change)
    lst setAdapter new ArrayAdapter(me, singleChoice, denominations)
    lst.setItemChecked(app.prefs.getInt(AbstractKit.DENOM_TYPE, 0), true)
    mkForm(me negBld dialog_ok, getString(fiat_set_denom).html, form)
  }

  def checkPassNotify(next: String => Unit)(pass: String) = {
    add(app getString secret_checking, Informer.CODECHECK).flash.run
    timer.schedule(delete(Informer.CODECHECK), 2500)
    checkPass(next)(pass)
  }

  def doViewMnemonic(password: String) =
    <(app.kit decryptSeed password, onFail) { seed =>
      val wordsText = TextUtils.join("\u0020", seed.getMnemonicCode)
      lazy val dialog = mkChoiceDialog(none, warnUser, dialog_ok, dialog_export)
      lazy val alert = mkForm(dialog, getString(sets_noscreen).html, wordsText)
      alert

      def warnUser: Unit = rm(alert) {
        lazy val dialog1 = mkChoiceDialog(encryptAndExport, none, dialog_ok, dialog_cancel)
        lazy val alert1 = mkForm(dialog1, null, getString(mnemonic_export_details).html)
        alert1

        def encryptAndExport: Unit = rm(alert1) {
          val packed = AES.encode(wordsText, Crypto sha256 password.binary.data)
          me share s"Encrypted BIP32 code ${new java.util.Date}: ${packed.toString}"
        }
      }
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
        val serverRequest = LNParams.cloud.connector getBackup LNParams.cloudId.toString
        val localCommitments = app.ChannelManager.all.flatMap(_ apply identity)
        add(app getString ln_notify_recovering, Informer.LNPAYMENT).flash.run
        timer.schedule(delete(Informer.LNPAYMENT), 16000)

        serverRequest.foreach(serverDataVec => {
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
      def openForm = passWrap(me getString sets_rescan) apply checkPassNotify { pass =>
        showForm(mkChoiceDialog(go, none, dialog_ok, dialog_cancel).setMessage(sets_rescan_ok).create)
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
      // Provided as an external function because may be accessed directly from main page
      def openForm = passWrap(me getString sets_mnemonic) apply checkPassNotify(doViewMnemonic)
      rm(menu)(openForm)
    }

    changePass setOnClickListener onButtonTap {
      def openForm = passWrap(me getString sets_secret_change) apply checkPassNotify { oldPass =>
        val view \ field = generatePromptView(InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD, secret_new, null)
        mkForm(mkChoiceDialog(checkNewPass, none, dialog_ok, dialog_cancel), me getString sets_secret_change, view)
        def checkNewPass = if (field.getText.toString.length >= 6) changePassword else app toast secret_too_short

        def changePassword = {
          <(rotatePass, _ => System exit 0)(_ => app toast sets_secret_ok)
          add(app getString secret_changing, Informer.CODECHECK).flash.run
          timer.schedule(delete(Informer.CODECHECK), 5000)
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
    val (view, field) = str2Tuple(LNParams.cloudPrivateKey.publicKey.toString)
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
      println(s"url1: $url1")
      val data1 = LNParams.cloud.data.copy(url = url1)
      println(s"data1: $data1")
      val cloud1 = LNParams getCloud Success(data1)
      println(s"cloud1: $cloud1")

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

  abstract class TxProcessor {
    def onTxFail(exc: Throwable): Unit
    def processTx(pass: String, feePerKb: Coin)
    val pay: PayData

    def chooseFee: Unit =
      passWrap(getString(step_2).format(pay cute sumOut).html) { pass =>
        add(app getString secret_checking, Informer.CODECHECK).flash.run
        timer.schedule(delete(Informer.CODECHECK), 2500)

        <(makeTx(pass, RatesSaver.rates.feeLive), onTxFail) { estimateTx =>
          // Get live final fee and set a risky final fee to be 2 times less

          val livePerTxFee: MilliSatoshi = estimateTx.getFee
          val riskyPerTxFee: MilliSatoshi = livePerTxFee / 2
          val markedLivePerTxFee = sumOut format denom.withSign(livePerTxFee)
          val markedRiskyPerTxFee = sumOut format denom.withSign(riskyPerTxFee)
          val txtFeeLive = getString(fee_live) format humanFiat(markedLivePerTxFee, livePerTxFee, " ")
          val txtFeeRisky = getString(fee_risky) format humanFiat(markedRiskyPerTxFee, riskyPerTxFee, " ")
          val feesOptions = Array(txtFeeRisky.html, txtFeeLive.html)

          // Prepare popup interface with fee options
          val adp = new ArrayAdapter(me, singleChoice, feesOptions)
          val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
          val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

          def proceed = lst.getCheckedItemPosition match {
            case 0 => processTx(pass, RatesSaver.rates.feeLive div 2)
            case 1 => processTx(pass, RatesSaver.rates.feeLive)
          }

          lst.setAdapter(adp)
          lst.setItemChecked(0, true)
          lazy val dialog: Builder = mkChoiceDialog(rm(alert)(proceed), none, dialog_pay, dialog_cancel)
          lazy val alert = mkForm(dialog, getString(step_3).format(pay cute sumOut).html, form)
          alert
        }
      }

    def makeTx(pass: String, fee: Coin) = {
      val crypter = app.kit.wallet.getKeyCrypter
      val keyParameter = crypter deriveKey pass
      val request = pay.sendRequest

      request.feePerKb = fee
      request.aesKey = keyParameter
      app.kit.wallet completeTx request
      request.tx.verify
      request.tx
    }

    def messageWhenMakingTx: PartialFunction[Throwable, CharSequence] = {
      case _: ExceededMaxTransactionSize => app getString err_transaction_too_large
      case _: CouldNotAdjustDownwards => app getString err_empty_shrunk
      case notEnough: InsufficientMoneyException =>

        val sending = sumOut.format(denom withSign pay.cn)
        val missing = sumOut.format(denom withSign notEnough.missing)
        val balance = sumIn.format(denom withSign app.kit.conf1Balance)
        getString(err_not_enough_funds).format(balance, sending, missing).html

      case _: KeyCrypterException => app getString err_secret
      case _: Throwable => app getString err_general
    }
  }
}

trait TimerActivity extends AppCompatActivity { me =>
  val goTo: Class[_] => Unit = me startActivity new Intent(me, _)
  val exitTo: Class[_] => Unit = goto => wrap(finish)(goTo apply goto)
  val timer = new Timer

  // Screen size in inches and prefs reference
  lazy val maxDialog = metrics.densityDpi * 2.1
  lazy val scrWidth = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val metrics = new DisplayMetrics match { case metrix =>
    getWindowManager.getDefaultDisplay getMetrics metrix
    metrix
  }

  // Basis for dialog forms
  def str2Tuple(textFieldData: CharSequence): (LinearLayout, TextView) = {
    val view = getLayoutInflater.inflate(R.layout.frag_top_tip, null).asInstanceOf[LinearLayout]
    val titleTextField = Utils clickableTextField view.findViewById(R.id.actionTip)
    titleTextField setText textFieldData
    view -> titleTextField
  }

  def generatePromptView(inputType: Int, message: Int, transform: TransformationMethod) = {
    val passAsk = getLayoutInflater.inflate(R.layout.frag_changer, null).asInstanceOf[LinearLayout]
    val secretInputField = passAsk.findViewById(R.id.secretInput).asInstanceOf[EditText]
    passAsk.findViewById(R.id.secretTip).asInstanceOf[TextView] setText message
    secretInputField setTransformationMethod transform
    secretInputField setInputType inputType
    passAsk -> secretInputField
  }

  def delayUI(fun: => Unit) = timer.schedule(anyToRunnable(fun), 225)
  def rm(previous: Dialog)(fun: => Unit) = wrap(previous.dismiss)(me delayUI fun)
  def mkForm(bld: Builder, title: View, content: View) = showForm(bld.setCustomTitle(title).setView(content).create)
  def onFail(error: CharSequence): Unit = me runOnUiThread mkForm(me negBld dialog_ok, null, error).show
  def onFail(error: Throwable): Unit = onFail(error.getMessage)

  def showForm(alertDialog: AlertDialog) = {
    alertDialog setCanceledOnTouchOutside false
    alertDialog.getWindow.getAttributes.windowAnimations = R.style.SlidingDialog
    // This may be called after a host activity is destroyed and thus would throw
    try alertDialog.show catch none finally if (scrWidth > 2.3) alertDialog.getWindow
      .setLayout(maxDialog.toInt, WRAP_CONTENT)

    alertDialog
  }

  def negBld(neg: Int): Builder = new Builder(me).setNegativeButton(neg, null)
  def negPosBld(neg: Int, pos: Int): Builder = negBld(neg).setPositiveButton(pos, null)
  def mkChoiceDialog(ok: => Unit, no: => Unit, okResource: Int, noResource: Int): Builder = {
    val cancel = new DialogInterface.OnClickListener { def onClick(x: DialogInterface, w: Int) = no }
    val again = new DialogInterface.OnClickListener { def onClick(x: DialogInterface, w: Int) = ok }
    new Builder(me).setPositiveButton(okResource, again).setNegativeButton(noResource, cancel)
  }

  def INIT(savedInstanceState: Bundle): Unit
  override def onCreate(savedInstanceState: Bundle) = {
    Thread setDefaultUncaughtExceptionHandler new UncaughtHandler(me)
    super.onCreate(savedInstanceState)
    INIT(savedInstanceState)
  }

  override def onDestroy = wrap(super.onDestroy)(timer.cancel)
  implicit def uiTask(process: => Runnable): TimerTask = new TimerTask { def run = me runOnUiThread process }
  implicit def str2View(res: CharSequence): LinearLayout = str2Tuple(res) match { case view \ _ => view }

  // Run computation in Future, deal with results on UI thread
  def <[T](fun: => T, no: Throwable => Unit)(ok: T => Unit) = <<(Future(fun), no)(ok)
  def <<[T](future: Future[T], no: Throwable => Unit)(ok: T => Unit) = future onComplete {
    case Success(rs) => runOnUiThread(ok apply rs) case Failure(ex) => runOnUiThread(no apply ex)
  }

  // Utils

  def hideKeys(fun: => Unit): Unit = try {
    val mgr = getSystemService(INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
    mgr.hideSoftInputFromWindow(getCurrentFocus.getWindowToken, HIDE_NOT_ALWAYS)
  } catch none finally me delayUI fun

  def onTap(run: Int => Unit): OnItemClickListener = new OnItemClickListener {
    def onItemClick(adapter: AdapterView[_], view: View, pos: Int, id: Long) = run(pos)
  }

  def onButtonTap(run: => Unit): OnClickListener = new OnClickListener {
    // Shortcut for click listener for buttons, also tries to hide a keyboard
    def onClick(view: View) = me hideKeys run
  }

  def share(text: String): Unit = startActivity {
    val share = new Intent setAction Intent.ACTION_SEND setType "text/plain"
    share.putExtra(android.content.Intent.EXTRA_TEXT, text)
  }

  // Password prompting popup
  // but it does not actually check a password
  val passWrap = (title: CharSequence) => (next: String => Unit) => {
    val isPasswordKeyboard = app.prefs.getBoolean(AbstractKit.PASS_INPUT, true)
    val inputType = if (isPasswordKeyboard) passNoSuggest else InputType.TYPE_CLASS_NUMBER
    val (view, field) = generatePromptView(inputType, secret_wallet, new PasswordTransformationMethod)
    mkForm(mkChoiceDialog(next(field.getText.toString), none, dialog_next, dialog_cancel), title, view)
  }

  def checkPass(next: String => Unit)(pass: String) = {
    def proceed(isCorrect: Boolean) = if (isCorrect) next(pass) else app toast secret_wrong
    <(app.kit.wallet checkPassword pass, _ => app toast err_general)(proceed)
  }
}

class RateManager(extra: String, val content: View) { me =>
  val satInput = content.findViewById(R.id.inputAmount).asInstanceOf[EditText]
  val hintDenom = Utils clickableTextField content.findViewById(R.id.hintDenom)
  val fiatType = content.findViewById(R.id.fiatType).asInstanceOf[SegmentedGroup]
  val fiatInput = content.findViewById(R.id.fiatInputAmount).asInstanceOf[EditText]
  def result: TryMSat = Try(denom rawString2MSat satInput.getText.toString.noCommas)
  def setSum(res: TryMSat) = satInput.setText(res map denom.formatted getOrElse null)
  def fiatDecimal = BigDecimal(fiatInput.getText.toString.noCommas)

  val fiatListener = new TextChangedWatcher {
    def upd = setSum(currentRate.map(perBtc => fiatDecimal / perBtc) map btcBigDecimal2MSat)
    def onTextChanged(s: CharSequence, start: Int, b: Int, c: Int) = if (fiatInput.hasFocus) upd
  }

  val bitListener = new TextChangedWatcher {
    def upd = fiatInput.setText(result flatMap msatInFiat map formatFiat.format getOrElse null)
    def onTextChanged(s: CharSequence, start: Int, b: Int, c: Int) = if (satInput.hasFocus) upd
  }

  fiatType setOnCheckedChangeListener new OnCheckedChangeListener {
    def onCheckedChanged(radioGroupView: RadioGroup, newFiatName: Int) = {
      // We update both runtime variable and saved value for future launches

      fiatName = fiatMap apply newFiatName
      app.prefs.edit.putString(AbstractKit.FIAT_TYPE, fiatName).commit
      if (fiatInput.hasFocus) fiatListener.upd else bitListener.upd
      fiatInput setHint fiatName
    }
  }

  satInput addTextChangedListener bitListener
  fiatInput addTextChangedListener fiatListener
  hintDenom setText denom.txt.format(extra).html
  fiatType check revFiatMap(fiatName)
  satInput.requestFocus
}

class BtcManager(val man: RateManager) { me =>
  val addressData = man.content.findViewById(R.id.addressData).asInstanceOf[TextView]
  val addressPaste = man.content.findViewById(R.id.addressPaste).asInstanceOf[Button]
  def set(tm: TryMSat, adr: Address) = wrap(man setSum tm)(me setAddress adr)
  def getAddress = addressData.getTag.asInstanceOf[Address]

  addressPaste setOnClickListener new OnClickListener {
    def onClick(button: View) = app.getBufferTry map app.getTo match {
      case Success(validBitcoinAddress) => setAddress(validBitcoinAddress)
      case _ => app toast dialog_clipboard_absent
    }
  }

  def setAddress(adr: Address) = {
    addressPaste setVisibility View.GONE
    addressData setVisibility View.VISIBLE
    addressData setText humanAddr(adr)
    addressData setTag adr
  }
}


trait PayData {
  def sendRequest: SendRequest
  def destination: String
  def onClick: Unit
  def cn: Coin

  def cute(direction: String): String = coin2MSat(cn) match { case msat =>
    val top = destination + "<br><br>" + direction.format(denom withSign msat)
    humanFiat(top, msat)
  }
}

case class AddrData(cn: Coin, address: Address) extends PayData {
  def link = BitcoinURI.convertToBitcoinURI(address, cn, null, null)
  def onClick = app.setBuffer(address.toString)
  def destination = humanAddr(address)

  def sendRequest = {
    val isAll = app.kit.conf1Balance equals cn
    if (isAll) SendRequest.emptyWallet(address)
    else SendRequest.to(address, cn)
  }
}

case class P2WSHData(cn: Coin, pay2wsh: Script) extends PayData {
  // This will only be used for funding lightning payment channels
  def onClick = app.setBuffer(denom withSign cn)
  def destination = app getString txs_p2wsh

  def sendRequest = {
    val funding = new Transaction(app.params)
    funding.addOutput(cn, pay2wsh)
    SendRequest forTx funding
  }
}


abstract class TextChangedWatcher extends TextWatcher {
  override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = none
  override def afterTextChanged(editableCharSequence: Editable) = none
}

trait BlocksListener extends PeerDataEventListener {
  def getData(peer: Peer, message: GetDataMessage) = null
  def onChainDownloadStarted(peer: Peer, blocksLeft: Int) = none
  def onPreMessageReceived(peer: Peer, message: Message) = message
  val blocksPerDay = 144
}

abstract class TxTracker extends WalletCoinsSentEventListener
with WalletCoinsReceivedEventListener with TransactionConfidenceEventListener {
  def onCoinsSent(w: Wallet, tx: Transaction, a: Coin, b: Coin) = if (a isGreaterThan b) coinsSent(tx)
  def onCoinsReceived(w: Wallet, tx: Transaction, a: Coin, b: Coin) = if (b isGreaterThan a) coinsReceived(tx)

  def onTransactionConfidenceChanged(wallet: Wallet, tx: Transaction) =
    if (tx.getConfidence.getDepthInBlocks == minDepth) txConfirmed(tx)

  def coinsSent(tx: Transaction): Unit = none
  def coinsReceived(tx: Transaction): Unit = none
  def txConfirmed(tx: Transaction): Unit = none
}