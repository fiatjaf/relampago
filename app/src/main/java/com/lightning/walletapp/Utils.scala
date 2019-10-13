package com.lightning.walletapp

import R.string._
import android.text._
import android.view._
import android.widget._
import org.bitcoinj.core._
import android.text.method._
import org.bitcoinj.core.listeners._
import com.lightning.walletapp.Utils._
import org.bitcoinj.wallet.listeners._
import android.content.DialogInterface._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import org.bitcoinj.wallet.Wallet.ExceededMaxTransactionSize
import org.bitcoinj.wallet.Wallet.CouldNotAdjustDownwards
import android.widget.AdapterView.OnItemClickListener
import concurrent.ExecutionContext.Implicits.global
import android.support.v7.app.AppCompatActivity
import android.support.v4.content.ContextCompat
import ViewGroup.LayoutParams.WRAP_CONTENT
import android.view.View.OnClickListener
import org.aviran.cookiebar2.CookieBar
import android.app.AlertDialog.Builder
import org.bitcoinj.wallet.SendRequest
import fr.acinq.bitcoin.MilliSatoshi
import language.implicitConversions
import org.bitcoinj.script.Script
import scala.concurrent.Future
import android.content.Intent
import android.os.Bundle

import com.lightning.walletapp.lnutils.IconGetter.{maxDialog, scrWidth}
import com.lightning.walletapp.ln.Tools.{none, runAnd, wrap}
import com.lightning.walletapp.lnutils.{GDrive, RatesSaver}
import org.bitcoinj.wallet.SendRequest.{emptyWallet, to}
import scala.util.{Failure, Success, Try}
import android.app.{AlertDialog, Dialog}
import java.util.{Timer, TimerTask}


object Utils {
  type TryMSat = Try[MilliSatoshi]
  var appReference: WalletApp = _
  var denom: Denomination = _
  var fiatCode: String = _

  final val fileName = "SegwitTestnet4"
  final val dbFileName = s"$fileName.db"
  final val walletFileName = s"$fileName.wallet"
  final val chainFileName = s"$fileName.spvchain"

  lazy val app = appReference
  lazy val noDesc = app getString ln_no_description
  lazy val denoms = List(SatDenomination, BtcDenomination)

  lazy val defaultHostedNode = {
    val nodeId = "02330d13587b67a85c0a36ea001c4dba14bcd48dda8988f7303275b040bffb6abd"
    HostedChannelRequest(s"$nodeId@172.245.74.10:9935", Some("Testnet-Node"), "00")
  }

  val fiatNames =
    Map("usd" -> "US Dollar", "eur" -> "Euro", "jpy" -> "Japanese Yen", "cny" -> "Chinese Yuan",
      "inr" -> "Indian Rupee", "ils" -> "Israeli Shekel", "cad" -> "Canadian Dollar", "rub" -> "Русский Рубль",
      "brl" -> "Real Brasileiro", "czk" -> "Česká Koruna", "gbp" -> "Pound Sterling", "aud" -> "Australian Dollar")

  val viewMap = Map(true -> View.VISIBLE, false -> View.GONE)
  def getDescription(raw: String) = if (raw.isEmpty) s"<i>$noDesc</i>" else raw take 72
  def humanSix(bitcoinAddress: String) = bitcoinAddress grouped 6 mkString "\u0020"

  def clickableTextField(view: View): TextView = {
    val field: TextView = view.asInstanceOf[TextView]
    field setMovementMethod LinkMovementMethod.getInstance
    field
  }

  def currentRate = Try(RatesSaver.rates exchange fiatCode)
  def msatInFiat(milliSatoshi: MilliSatoshi) = currentRate map {
    perBtc => milliSatoshi.amount * perBtc / BtcDenomination.factor
  }

  val msatInFiatHuman: MilliSatoshi => String = ms => msatInFiat(ms) match {
    case Success(providedAmount) => s"≈ ${formatFiat format providedAmount} $fiatCode"
    case _ => s"≈ ? $fiatCode"
  }
}

trait TimerActivity extends AppCompatActivity { me =>
  override def onDestroy = wrap(super.onDestroy)(timer.cancel)
  override def onCreate(savedActivityInstanceState: Bundle) = {
    Thread setDefaultUncaughtExceptionHandler new UncaughtHandler(me)
    super.onCreate(savedActivityInstanceState)
    INIT(savedActivityInstanceState)
  }

  val timer = new Timer
  val goTo: Class[_] => Any = target => {
    me startActivity new Intent(this, target)
    app.TransData.DoNotEraseValue
  }

  val exitTo: Class[_] => Any = target => {
    me startActivity new Intent(this, target)
    runAnd(app.TransData.DoNotEraseValue)(finish)
  }

  def askGDriveSignIn = {
    val signInClient = GDrive signInAttemptClient me
    startActivityForResult(signInClient.getSignInIntent, 102)
  }

  def rm(prev: Dialog)(exe: => Unit) = {
    // Add some delay between popup switches
    timer.schedule(exe, 225)
    prev.dismiss
  }

  def finishMe(top: View) = finish
  def baseTextBuilder(msg: CharSequence) = new Builder(me).setMessage(msg)
  def baseBuilder(title: View, body: View) = new Builder(me).setCustomTitle(title).setView(body)
  def negTextBuilder(neg: Int, msg: CharSequence) = baseTextBuilder(msg).setNegativeButton(neg, null)
  def negBuilder(neg: Int, title: View, body: View) = baseBuilder(title, body).setNegativeButton(neg, null)

  def toast(code: Int): Unit = toast(me getString code)
  def toast(msg: String): Unit = try CookieBar.rebuild(me).setMessage(msg).setCookiePosition(CookieBar.BOTTOM).show catch none
  def onFail(error: CharSequence): Unit = UITask(me showForm negBuilder(dialog_ok, null, error).create).run
  def onFail(error: Throwable): Unit = onFail(error.getMessage)

  def mkCheckForm(ok: AlertDialog => Unit, no: => Unit, bld: Builder, okResource: Int, noResource: Int) = {
    // Create alert dialog with NEGATIVE button which removes a dialog and calls a respected provided function
    // both POSITIVE and NEGATIVE buttons may be omitted by providing -1 as their resource ids
    if (-1 != noResource) bld.setNegativeButton(noResource, null)
    if (-1 != okResource) bld.setPositiveButton(okResource, null)

    val alert = showForm(bld.create)
    val posAct = me onButtonTap ok(alert)
    val negAct = me onButtonTap rm(alert)(no)
    if (-1 != noResource) alert getButton BUTTON_NEGATIVE setOnClickListener negAct
    if (-1 != okResource) alert getButton BUTTON_POSITIVE setOnClickListener posAct
    alert
  }

  def mkCheckFormNeutral(ok: AlertDialog => Unit, no: => Unit, neutral: AlertDialog => Unit,
                         bld: Builder, okResource: Int, noResource: Int, neutralResource: Int) = {

    if (-1 != neutralResource) bld.setNeutralButton(neutralResource, null)
    val alert = mkCheckForm(ok, no, bld, okResource, noResource)
    val neutralAct = me onButtonTap neutral(alert)

    // Extend base dialog with a special NEUTRAL button, may be omitted by providing -1
    if (-1 != neutralResource) alert getButton BUTTON_NEUTRAL setOnClickListener neutralAct
    alert
  }

  def showForm(alertDialog: AlertDialog) = {
    // This may be called after a host activity is destroyed and thus it may throw
    alertDialog.getWindow.getAttributes.windowAnimations = R.style.SlidingDialog
    alertDialog setCanceledOnTouchOutside false

    // First, attempt to make dialog links clickable, then make sure it does not blow up if called on destroyed activity
    try alertDialog.show catch none finally if (scrWidth > 2.3) alertDialog.getWindow.setLayout(maxDialog.toInt, WRAP_CONTENT)
    try clickableTextField(alertDialog findViewById android.R.id.message) catch none
    alertDialog
  }

  def makeChoiceList[T <: Object](actions: Array[T], title: CharSequence) = {
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, actions)
    val alert = showForm(negBuilder(dialog_cancel, title, lst).create)
    lst setDividerHeight 0
    lst setDivider null
    lst -> alert
  }

  def INIT(savedInstanceState: Bundle): Unit
  def updateView2Blue(oldView: View, newText: String) = {
    val titleTip = oldView.findViewById(R.id.titleTip).asInstanceOf[TextView]
    oldView setBackgroundColor ContextCompat.getColor(me, R.color.ln)
    titleTip setText s"<font color=#FFFFFF>$newText</font>".html
    oldView
  }

  implicit def str2View(textFieldData: CharSequence): LinearLayout = {
    val view = getLayoutInflater.inflate(R.layout.frag_top_tip, null).asInstanceOf[LinearLayout]
    Utils clickableTextField view.findViewById(R.id.titleTip) setText textFieldData
    view setBackgroundColor 0x22AAAAAA
    view
  }

  implicit def UITask(exec: => Any): TimerTask = {
    val runnableExec = new Runnable { override def run = exec }
    new TimerTask { def run = me runOnUiThread runnableExec }
  }

  // Run computation in Future, deal with results on UI thread
  def <[T](fun: => T, no: Throwable => Unit)(ok: T => Unit) = Future(fun) onComplete {
    case Success(rs) => UITask(ok apply rs).run case Failure(ex) => UITask(no apply ex).run
  }

  // Utils
  def onButtonTap(exec: => Unit) = new OnClickListener { def onClick(view: View) = exec }
  def onTap(listItemTapRunner: Int => Unit): OnItemClickListener = new OnItemClickListener {
    def onItemClick(a: AdapterView[_], view: View, pos: Int, id: Long) = listItemTapRunner(pos)
  }

  def share(exportedTextData: String): Unit = {
    val share = new Intent setAction Intent.ACTION_SEND setType "text/plain"
    me startActivity share.putExtra(Intent.EXTRA_TEXT, exportedTextData)
  }

  def viewMnemonic(view: View) = {
    val recoveryCode = TextUtils.join("\u0020", app.kit.wallet.getKeyChainSeed.getMnemonicCode)
    showForm(negBuilder(dialog_ok, me getString sets_mnemonic, recoveryCode).create)
  }

  abstract class TxProcessor { self =>
    def futureProcess(req: SendRequest)
    def onTxFail(err: Throwable): Unit
    val pay: PayData

    def start(coloredAmount: String) = <(app.kit sign plainRequest(RatesSaver.rates.feeSix), onTxFail) { estimate =>
      // Estimate a fee this tx will have in order to be confirmed within next 6 blocks, then propose chaper one
      val livePerTxFee: MilliSatoshi = estimate.tx.getFee
      val riskyPerTxFee: MilliSatoshi = livePerTxFee / 2

      val inFiatLive = msatInFiatHuman(livePerTxFee)
      val inFiatRisky = msatInFiatHuman(riskyPerTxFee)
      val markedLivePerTxFee = denom.coloredOut(livePerTxFee, denom.sign)
      val markedRiskyPerTxFee = denom.coloredOut(riskyPerTxFee, denom.sign)

      val txtFeeLive = getString(fee_live).format(markedLivePerTxFee, inFiatLive)
      val txtFeeRisky = getString(fee_risky).format(markedRiskyPerTxFee, inFiatRisky)
      val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
      val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]
      val feesOptions = Array(txtFeeRisky.html, txtFeeLive.html)

      def proceed = <(lst.getCheckedItemPosition match {
        // Allow user to choose an economical fee when sending a manual transaction
        case 0 => self futureProcess plainRequest(RatesSaver.rates.feeSix div 2)
        case 1 => self futureProcess plainRequest(RatesSaver.rates.feeSix)
      }, onTxFail)(none)

      val stepFees = getString(step_fees).format(coloredAmount).html
      mkCheckForm(alert => rm(alert)(proceed), none, baseBuilder(stepFees, form), dialog_pay, dialog_cancel)
      lst setAdapter new ArrayAdapter(me, android.R.layout.select_dialog_singlechoice, feesOptions)
      lst.setItemChecked(0, true)
    }

    def plainRequest(selectedFeePerKb: Coin) = {
      val unsignedRequestWithFee = pay.getRequest
      unsignedRequestWithFee.feePerKb = selectedFeePerKb
      app.kit.wallet addLocalInputsToTx unsignedRequestWithFee
      unsignedRequestWithFee
    }

    def txMakeErrorBuilder: PartialFunction[Throwable, Builder] = {
      case _: ExceededMaxTransactionSize => baseBuilder(app getString err_tx_too_large, null)
      case _: CouldNotAdjustDownwards => baseBuilder(app getString err_empty_shrunk, null)

      case notEnough: InsufficientMoneyException =>
        val sending = s"<strong>${denom parsedWithSign pay.cn}</strong>"
        val missing = s"<strong>${denom parsedWithSign notEnough.missing}</strong>"
        val canSend = s"<strong>${denom parsedWithSign app.kit.conf0Balance}</strong>"
        baseTextBuilder(getString(err_not_enough_funds).format(canSend, sending, missing).html)

      case other: Throwable =>
        val info = UncaughtHandler toText other
        negTextBuilder(dialog_ok, info)
    }
  }

  def attemptHostedChannel(request: HostedChannelRequest) = UITask {
    val hnv = HardcodedNodeView(request.ann, app getString ln_ops_start_fund_hosted_channel)
    app.TransData.value = HostedChannelParams(hnv, request.secret)
    me goTo classOf[LNStartFundActivity]
  }
}

class RateManager(val content: View) { me =>
  val satInput = content.findViewById(R.id.inputAmount).asInstanceOf[EditText]
  val fiatInput = content.findViewById(R.id.fiatInputAmount).asInstanceOf[EditText]
  val hintFiatDenom = Utils clickableTextField content.findViewById(R.id.hintFiatDenom)
  val hintDenom = Utils clickableTextField content.findViewById(R.id.hintDenom)

  def result = Try(denom rawString2MSat satInput.getText.toString.noSpaces)
  def setSum(res: TryMSat) = satInput.setText(res map denom.asString getOrElse null)
  def hint(ex: String) = runAnd(me)(hintDenom setText denom.amountInTxt.format(ex).html)

  val fiatListener = new TextChangedWatcher {
    def upd = currentRate.map(perBtc => BigDecimal(fiatInput.getText.toString.noSpaces) / perBtc) map btcBigDecimal2MSat
    def onTextChanged(s: CharSequence, start: Int, before: Int, count: Int) = if (fiatInput.hasFocus) setSum(upd)
  }

  val bitListener = new TextChangedWatcher {
    def upd = fiatInput.setText(result flatMap msatInFiat map formatFiat.format getOrElse null)
    def onTextChanged(s: CharSequence, start: Int, before: Int, count: Int) = if (satInput.hasFocus) upd
  }

  satInput addTextChangedListener bitListener
  fiatInput addTextChangedListener fiatListener
  hintFiatDenom setText fiatNames(fiatCode)
  satInput.requestFocus
}

trait PayData {
  def destination(colored: String): String
  def getRequest: SendRequest
  def cn: Coin
}

case class AddrData(cn: Coin, address: Address) extends PayData {
  def getRequest: SendRequest = if (app.kit.conf0Balance equals cn) emptyWallet(address) else to(address, cn)
  def destination(colored: String) = s"<small>$colored</small><br>" + humanSix(address.toString)
}

case class P2WSHData(cn: Coin, pay2wsh: Script) extends PayData {
  def getRequest = if (app.kit.conf0Balance equals cn) emptyWallet(app.params, pay2wsh) else to(app.params, pay2wsh, cn)
  def destination(colored: String) = s"<small>$colored</small><br>" + app.getString(txs_p2wsh)
}

abstract class TextChangedWatcher extends TextWatcher {
  override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = none
  override def afterTextChanged(editableCharSequence: Editable) = none
}

trait BlocksListener extends PeerDataEventListener {
  def getData(peer: Peer, message: GetDataMessage) = null
  def onChainDownloadStarted(peer: Peer, blocksLeft: Int) = none
  def onPreMessageReceived(peer: Peer, message: Message) = message
}

trait TxTracker
  extends WalletCoinsSentEventListener
    with WalletCoinsReceivedEventListener