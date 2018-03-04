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
import org.bitcoinj.wallet.Wallet.ExceededMaxTransactionSize
import org.bitcoinj.wallet.Wallet.CouldNotAdjustDownwards
import android.widget.RadioGroup.OnCheckedChangeListener
import android.widget.AdapterView.OnItemClickListener
import info.hoang8f.android.segmented.SegmentedGroup
import concurrent.ExecutionContext.Implicits.global
import android.view.inputmethod.InputMethodManager
import com.lightning.wallet.ln.LNParams.minDepth
import android.support.v7.app.AppCompatActivity
import org.bitcoinj.crypto.KeyCrypterException
import com.lightning.wallet.lnutils.RatesSaver
import android.view.View.OnClickListener
import android.app.AlertDialog.Builder
import com.lightning.wallet.helper.AES
import language.implicitConversions
import android.util.DisplayMetrics
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.script.Script
import scala.concurrent.Future
import android.os.Bundle
import java.util.Date

import android.content.{Context, DialogInterface, Intent}
import org.bitcoinj.wallet.SendRequest.{emptyWallet, to}
import com.lightning.wallet.ln.Tools.{none, wrap}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import R.id.{typeCNY, typeEUR, typeJPY, typeUSD}
import fr.acinq.bitcoin.{Crypto, MilliSatoshi}
import scala.util.{Failure, Success, Try}
import android.app.{AlertDialog, Dialog}
import java.util.{Timer, TimerTask}

import ViewGroup.LayoutParams.WRAP_CONTENT
import InputMethodManager.HIDE_NOT_ALWAYS
import Context.INPUT_METHOD_SERVICE


object Utils {
  type TryMSat = Try[MilliSatoshi]
  var appReference: WalletApp = _
  var denom: Denomination = _
  var fiatName: String = _

  val fileName = "Testnet"
  val dbFileName = s"$fileName-4.db"
  val walletFileName = s"$fileName.wallet"
  val chainFileName = s"$fileName.spvchain"

  lazy val app = appReference
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
  def humanNode(key: String, sep: String) = key.grouped(24).map(_ grouped 3 mkString "\u0020") mkString sep
  def isMnemonicCorrect(mnemonicCode: String) = mnemonicCode.split("\\s+").length > 11
  def humanFour(adr: String) = adr grouped 4 mkString "\u0020"

  def clickableTextField(view: View): TextView = {
    val textView: TextView = view.asInstanceOf[TextView]
    textView setMovementMethod LinkMovementMethod.getInstance
    textView
  }

  def currentRate = Try(RatesSaver.rates exchange fiatName)
  def msatInFiat(msat: MilliSatoshi) = currentRate.map(_ * msat.amount / BtcDenomination.factor)
  def humanFiat(prefix: String, ms: MilliSatoshi, div: String = "<br>"): String = msatInFiat(ms) match {
    case Success(amt) if fiatName == strYuan => s"$prefix$div<font color=#999999>≈ ${formatFiat format amt} CNY</font>"
    case Success(amt) if fiatName == strEuro => s"$prefix$div<font color=#999999>≈ ${formatFiat format amt} EUR</font>"
    case Success(amt) if fiatName == strYen => s"$prefix$div<font color=#999999>≈ ${formatFiat format amt} JPY</font>"
    case Success(amt) => s"$prefix$div<font color=#999999>≈ ${formatFiat format amt} USD</font>"
    case _ => prefix
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

  def finishMe(top: View) = finish
  def delayUI(fun: TimerTask) = timer.schedule(fun, 225)
  def rm(previous: Dialog)(exec: => Unit) = wrap(previous.dismiss)(me delayUI exec)
  def mkForm(bld: Builder, title: View, body: View) = showForm(bld.setCustomTitle(title).setView(body).create)
  def onFail(error: CharSequence): Unit = UITask(mkForm(me negBld dialog_ok, null, error).show).run
  def onFail(error: Throwable): Unit = onFail(error.getMessage)

  def showForm(alertDialog: AlertDialog) = {
    alertDialog setCanceledOnTouchOutside false
    alertDialog.getWindow.getAttributes.windowAnimations = R.style.SlidingDialog
    // This may be called after a host activity is destroyed and thus it may throw
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
  override def onCreate(savedActivityInstanceState: Bundle) = {
    Thread setDefaultUncaughtExceptionHandler new UncaughtHandler(me)
    super.onCreate(savedActivityInstanceState)
    INIT(savedActivityInstanceState)
  }

  override def onDestroy = wrap(super.onDestroy)(timer.cancel)
  implicit def str2View(res: CharSequence): LinearLayout =
    str2Tuple(res) match { case view \ _ => view }

  implicit def UITask(exec: => Unit): TimerTask = {
    val runnableExec = new Runnable { override def run = exec }
    new TimerTask { def run = me runOnUiThread runnableExec }
  }

  // Run computation in Future, deal with results on UI thread
  def <[T](fun: => T, no: Throwable => Unit)(ok: T => Unit) = <<(Future(fun), no)(ok)
  def <<[T](future: Future[T], no: Throwable => Unit)(ok: T => Unit) = future onComplete {
    case Success(rs) => UITask(ok apply rs).run case Failure(ex) => UITask(no apply ex).run
  }

  // Utils
  def hideKeys(exec: => Unit): Unit = try {
    val mgr = getSystemService(INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
    mgr.hideSoftInputFromWindow(getCurrentFocus.getWindowToken, HIDE_NOT_ALWAYS)
  } catch none finally me delayUI exec

  def onTap(run: Int => Unit): OnItemClickListener = new OnItemClickListener {
    def onItemClick(adapter: AdapterView[_], view: View, pos: Int, id: Long) = run(pos)
  }

  def onButtonTap(exec: => Unit) = new OnClickListener { def onClick(view: View) = me hideKeys exec }
  def onFastTap(fastExec: => Unit) = new OnClickListener { def onClick(view: View) = fastExec }

  def share(exportedTextData: String): Unit = {
    val share = new Intent setAction Intent.ACTION_SEND setType "text/plain"
    me startActivity share.putExtra(Intent.EXTRA_TEXT, exportedTextData)
  }

  val passWrap = (title: CharSequence) => (next: String => Unit) => {
    // Password prompting popup, but it does not actually check a password
    val passNoSuggest = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD
    val (view, field) = generatePromptView(passNoSuggest, secret_wallet, new PasswordTransformationMethod)
    mkForm(mkChoiceDialog(next(field.getText.toString), none, dialog_next, dialog_cancel), title, view)
  }

  def checkPass(next: String => Unit)(pass: String) = {
    // Takes a method to be executed after a pass check is successful, also shows a toast
    def proceed(isCorrect: Boolean) = if (isCorrect) next(pass) else app toast secret_wrong
    <(app.kit.wallet checkPassword pass, _ => app toast err_general)(proceed)
    app toast secret_checking
  }

  def viewMnemonic(view: View) =
    passWrap(me getString sets_mnemonic) apply checkPass { pass =>
      // Warn user and proceed to export an encrypted mnemonic code
      // using current wallet passcode if the user is fine with that

      <(app.kit decryptSeed pass, onFail) { seed =>
        val wordsText = TextUtils.join("\u0020", seed.getMnemonicCode)
        lazy val codeBld = mkChoiceDialog(none, warnUser, dialog_ok, dialog_export)
        lazy val codeAlert = mkForm(codeBld, getString(sets_noscreen).html, wordsText)
        codeAlert

        def warnUser: Unit = rm(codeAlert) {
          lazy val okBld = mkChoiceDialog(encryptAndExport, none, dialog_ok, dialog_cancel)
          lazy val okAlert = mkForm(okBld, null, getString(mnemonic_export_details).html)
          okAlert

          def encryptAndExport: Unit = rm(okAlert) {
            val hex = AES.encode(wordsText, Crypto sha256 pass.binary)
            me share s"Encrypted BIP32 mnemonic code ${new Date}: $hex"
          }
        }
      }
    }

  abstract class TxProcessor {
    def processTx(pass: String, feePerKb: Coin)
    def onTxFail(exc: Throwable): Unit
    val pay: PayData

    def chooseFee: Unit =
      passWrap(getString(step_2).format(pay cute sumOut).html) { pass =>
        <(app.kit sign unsigned(pass, RatesSaver.rates.feeLive), onTxFail) { signed =>
          // Get signed live final fee and set a risky final fee to be two times less

          val livePerTxFee: MilliSatoshi = signed.tx.getFee
          val riskyPerTxFee: MilliSatoshi = livePerTxFee / 2
          val markedLivePerTxFee = sumOut format denom.withSign(livePerTxFee)
          val markedRiskyPerTxFee = sumOut format denom.withSign(riskyPerTxFee)
          val txtFeeLive = getString(fee_live) format humanFiat(markedLivePerTxFee, livePerTxFee, " ")
          val txtFeeRisky = getString(fee_risky) format humanFiat(markedRiskyPerTxFee, riskyPerTxFee, " ")
          val feesOptions = Array(txtFeeRisky.html, txtFeeLive.html)

          // Prepare popup interface with fee options
          val adapter = new ArrayAdapter(me, singleChoice, feesOptions)
          val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
          val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

          def proceed = lst.getCheckedItemPosition match {
            case 0 => processTx(pass, RatesSaver.rates.feeLive div 2)
            case 1 => processTx(pass, RatesSaver.rates.feeLive)
          }

          lst.setAdapter(adapter)
          lst.setItemChecked(0, true)
          lazy val dialog: Builder = mkChoiceDialog(rm(alert)(proceed), none, dialog_pay, dialog_cancel)
          lazy val alert = mkForm(dialog, getString(step_3).format(pay cute sumOut).html, form)
          alert
        }

        // Let know something is on
        app toast secret_checking
      }

    def unsigned(pass: String, fee: Coin) = {
      // Create an unsigned transaction request
      val crypter = app.kit.wallet.getKeyCrypter
      val keyParameter = crypter deriveKey pass
      val request = pay.sendRequest

      request.feePerKb = fee
      request.aesKey = keyParameter
      app.kit.wallet assembleTx request
      request
    }

    def messageWhenMakingTx: PartialFunction[Throwable, CharSequence] = {
      case _: ExceededMaxTransactionSize => app getString err_transaction_too_large
      case _: CouldNotAdjustDownwards => app getString err_empty_shrunk
      case notEnough: InsufficientMoneyException =>

        val canSend = sumIn.format(denom withSign app.kit.conf1Balance)
        val sending = sumOut.format(denom withSign pay.cn)

        val txt = getString(err_not_enough_funds)
        val zeroConf = app.kit.conf0Balance minus app.kit.conf1Balance
        val missing = sumOut.format(denom withSign notEnough.missing)
        val pending = sumIn format denom.withSign(zeroConf)
        txt.format(canSend, sending, missing, pending).html

      case _: KeyCrypterException => app getString err_secret
      case _: Throwable => app getString err_general
    }
  }
}

class RateManager(extra: String, val content: View) { me =>
  val satInput = content.findViewById(R.id.inputAmount).asInstanceOf[EditText]
  val hintDenom = Utils clickableTextField content.findViewById(R.id.hintDenom)
  val fiatType = content.findViewById(R.id.fiatType).asInstanceOf[SegmentedGroup]
  val fiatInput = content.findViewById(R.id.fiatInputAmount).asInstanceOf[EditText]
  def result: TryMSat = Try(denom rawString2MSat satInput.getText.toString.noSpaces)
  def setSum(res: TryMSat) = satInput.setText(res map denom.formatted getOrElse null)
  def fiatDecimal = BigDecimal(fiatInput.getText.toString.noSpaces)

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

  def setAddress(addr: Address) = {
    addressPaste setVisibility View.GONE
    addressData setVisibility View.VISIBLE
    addressData setText humanFour(addr.toString)
    addressData setTag addr
  }
}

trait PayData {
  def isEmpty = app.kit.conf1Balance equals cn
  def sendRequest: SendRequest
  def destination: String
  def onClick: Unit
  def cn: Coin

  def cute(direction: String) = {
    val msat: MilliSatoshi = coin2MSat(cn)
    val human = direction.format(denom withSign msat)
    humanFiat(s"$destination<br><br>$human", msat)
  }
}

case class AddrData(cn: Coin, address: Address) extends PayData {
  def sendRequest = if (isEmpty) emptyWallet(address) else to(address, cn)
  def link = BitcoinURI.convertToBitcoinURI(address, cn, null, null)
  def onClick = app.setBuffer(address.toString)
  def destination = humanFour(address.toString)
}

case class P2WSHData(cn: Coin, pay2wsh: Script) extends PayData {
  // This will only be used for funding of LN payment channels as destination is unreadable
  def sendRequest = if (isEmpty) emptyWallet(app.params, pay2wsh) else to(app.params, pay2wsh, cn)
  def onClick = app.setBuffer(denom withSign cn)
  def destination = app getString txs_p2wsh
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

  def onTransactionConfidenceChanged(w: Wallet, tx: Transaction) =
    if (tx.getConfidence.getDepthInBlocks == minDepth) txConfirmed(tx)

  def coinsSent(tx: Transaction): Unit = none
  def coinsReceived(tx: Transaction): Unit = none
  def txConfirmed(tx: Transaction): Unit = none
}