package com.lightning.wallet

import android.widget._
import org.bitcoinj.core._
import collection.JavaConverters._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import android.provider.Settings.{System => FontSystem}
import android.view.{Menu, MenuItem, View, ViewGroup}
import scala.util.{Failure, Success, Try}

import android.widget.AbsListView.OnScrollListener.SCROLL_STATE_IDLE
import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import android.text.format.DateUtils.getRelativeTimeSpanString
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import android.content.DialogInterface.BUTTON_POSITIVE
import android.widget.AbsListView.OnScrollListener
import com.lightning.wallet.ln.LNParams.minDepth
import com.lightning.wallet.ln.PaymentRequest
import android.text.format.DateFormat
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat
import android.content.Intent
import android.os.Bundle
import android.net.Uri
import java.util.Date


trait HumanTimeDisplay { me: TimerActivity =>
  val time: Date => String = date => new SimpleDateFormat(timeString) format date
  lazy val bigFont = FontSystem.getFloat(getContentResolver, FontSystem.FONT_SCALE, 1) > 1

  // Should be accessed after activity is initialized
  lazy val timeString = DateFormat is24HourFormat me match {
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

  // Relative or absolute date
  def when(now: Long, date: java.util.Date) = date.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString else time(date)
  }
}

trait ListUpdater extends HumanTimeDisplay { me: TimerActivity =>
  lazy val allTxsWrapper = getLayoutInflater.inflate(R.layout.frag_txs_all, null)
  lazy val toggler = allTxsWrapper.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]
  private[this] var state = SCROLL_STATE_IDLE
  val minLinesNum = 4

  def startListUpdates(adapter: BaseAdapter) =
    list setOnScrollListener new OnScrollListener {
      def onScroll(v: AbsListView, first: Int, visible: Int, total: Int) = none
      def onScrollStateChanged(v: AbsListView, newState: Int) = state = newState
      def maybeUpdate = if (SCROLL_STATE_IDLE == state) adapter.notifyDataSetChanged
      timer.schedule(anyToRunnable(maybeUpdate), 10000, 10000)
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
      val view = if (null == savedView) getLayoutInflater.inflate(viewLine, null) else savedView
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

class BtcActivity extends DataReader with ToolbarActivity with ListUpdater { me =>
  lazy val mnemonicWarn = findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
  lazy val mnemonicInfo = Utils clickableTextField findViewById(R.id.mnemonicInfo)
  lazy val txsConfs = getResources getStringArray R.array.txs_confs
  lazy val feeIncoming = getString(txs_fee_incoming)
  lazy val feeDetails = getString(txs_fee_details)
  lazy val feeAbsent = getString(txs_fee_absent)

  lazy val adapter = new CutAdapter[TxWrap](96, R.layout.frag_tx_btc_line) {
    // BTC line has a wider timestamp section because there is no payment info

    def getItem(position: Int) = visibleItems(position)
    def getHolder(view: View) = new TxViewHolder(view) {

      def fillView(wrap: TxWrap) = {
        val statusImage = if (wrap.tx.getConfidence.getConfidenceType == DEAD) dead
          else if (wrap.tx.getConfidence.getDepthInBlocks >= minDepth) conf1
          else await

        val marking = if (wrap.nativeValue.isPositive) sumIn else sumOut
        val finalSum = if (wrap.nativeValue.isPositive) coin2MSat(wrap.nativeValue)
          else coin2MSat(wrap.fee map wrap.nativeValue.add getOrElse wrap.nativeValue)

        transactWhen setText when(System.currentTimeMillis, wrap.tx.getUpdateTime).html
        transactSum setText marking.format(denom formatted finalSum).html
        transactCircle setImageResource statusImage
      }
    }
  }

  private[this] val lstTracker = new TxTracker {
    override def coinsSent(tx: Transaction) = me runOnUiThread tell(tx)
    override def coinsReceived(tx: Transaction) = me runOnUiThread tell(tx)
    override def txConfirmed(tx: Transaction) = me runOnUiThread titleAndList
    private def titleAndList = wrap(updTitle)(adapter.notifyDataSetChanged)

    def tell(wrap: TxWrap) = if (!wrap.nativeValue.isZero) {
      // Zero means this tx changes nothing in our wallet and
      // thus it is watched or completely foreign

      mnemonicWarn setVisibility View.GONE
      adapter.set(wrap +: adapter.availableItems)
      adapter.notifyDataSetChanged
    }
  }

  def updTitle = setTitle {
    val conf0 = denom.withSign(app.kit.conf1Balance)
    val gap = app.kit.conf0Balance minus app.kit.conf1Balance
    if (gap.isZero) conf0 else s"$conf0 + ${denom formatted gap}"
  }

  def notifySubTitle(sub: String, infoType: Int) = {
    // Here we update not just subtitle but also a title text
    wrap(me runOnUiThread updTitle)(add(sub, infoType).flash.run)
    timer.schedule(delete(infoType), 8000)
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    // Set action bar, main view content, animate title, wire up list events
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_btc)
    wrap(updTitle)(add(me getString constListener.status, Informer.PEER).flash.run)
    wrap(me setDetecting true)(me initNfc state)
    me startListUpdates adapter

    toolbar setOnClickListener onButtonTap {
      showDenominationChooser(app.kit.conf1Balance) { pos =>
        // Rememeber user choice, update list and title text
        app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
        wrap(adapter.notifyDataSetChanged) { denom = denoms apply pos }
        updTitle
      }
    }

    list setAdapter adapter
    list setFooterDividersEnabled false
    list setOnItemClickListener onTap { pos =>
      val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
      val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val outside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]

      val wrap = adapter getItem pos
      val marking = if (wrap.nativeValue.isPositive) sumIn else sumOut
      val confirms = app.plurOrZero(txsConfs, wrap.tx.getConfidence.getDepthInBlocks)
      val outputs = wrap.payDatas(wrap.nativeValue.isPositive).flatMap(_.toOption)
      val humanViews = for (payData <- outputs) yield payData.cute(marking).html

      // Wire up a popup list
      lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, humanViews.toArray)
      lst setOnItemClickListener onTap { position => outputs(position - 1).onClick }
      lst setHeaderDividersEnabled false
      lst addHeaderView detailsWrapper

      outside setOnClickListener onButtonTap {
        val smartbit = "https://testnet.smartbit.com.au/tx/"
        val uri = Uri.parse(smartbit + wrap.tx.getHashAsString)
        me startActivity new Intent(Intent.ACTION_VIEW, uri)
      }

      wrap.fee match {
        case _ if wrap.tx.getConfidence.getConfidenceType == DEAD =>
          mkForm(me negBld dialog_ok, sumOut.format(txsConfs.last).html, lst)

        case _ if wrap.nativeValue.isPositive =>
          val details = feeIncoming.format(confirms)
          mkForm(me negBld dialog_ok, details.html, lst)

        case Some(fee) =>
          val details = feeDetails.format(marking.format(denom withSign fee), confirms)
          mkForm(me negBld dialog_ok, humanFiat(details, fee).html, lst)

        case None =>
          val details = feeAbsent.format(confirms).html
          mkForm(me negBld dialog_ok, details, lst)
      }
    }

    // Wait for transactions list
    <(nativeTransactions, onFail) { txs =>
      app.kit.wallet addCoinsSentEventListener lstTracker
      app.kit.wallet addCoinsReceivedEventListener lstTracker
      app.kit.wallet addTransactionConfidenceEventListener lstTracker
      adapter set txs

      if (txs.isEmpty) {
        // Show user a mnemonic dialog if there is no txs
        mnemonicInfo setText getString(mnemonic_info).html
        mnemonicWarn setVisibility View.VISIBLE
      }
    }

    // Wire up general listeners
    app.kit.wallet addCoinsSentEventListener txTracker
    app.kit.wallet addCoinsReceivedEventListener txTracker
    app.kit.peerGroup addBlocksDownloadedEventListener catchListener
    app.kit.peerGroup addDisconnectedEventListener constListener
    app.kit.peerGroup addConnectedEventListener constListener
  } else me exitTo classOf[MainActivity]

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeTransactionConfidenceEventListener lstTracker
    app.kit.wallet removeCoinsReceivedEventListener lstTracker
    app.kit.wallet removeCoinsSentEventListener lstTracker

    app.kit.wallet removeCoinsSentEventListener txTracker
    app.kit.wallet removeCoinsReceivedEventListener txTracker
    app.kit.peerGroup removeDisconnectedEventListener constListener
    app.kit.peerGroup removeConnectedEventListener constListener
    stopDetecting
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.transactions_ops, menu)
    super.onCreateOptionsMenu(menu)
  }

  override def onOptionsItemSelected(menu: MenuItem) = runAnd(true) {
    if (menu.getItemId == R.id.actionBuyCoins) localBitcoinsAndGlidera
    else if (menu.getItemId == R.id.actionSettings) mkSetsForm
  }

  override def onResume = wrap(super.onResume) {
    app.prefs.edit.putBoolean(AbstractKit.LANDING_LN, false).commit
    checkTransData
  }

  // DATA READING AND BUTTON ACTIONS

  def checkTransData = app.TransData.value match {
    case pr: PaymentRequest => me goTo classOf[LNActivity]

    case uri: BitcoinURI =>
      app.TransData.value = null
      val amt: TryMSat = Try(uri.getAmount)
      sendBtcPopup.set(amt, uri.getAddress)

    case adr: Address =>
      app.TransData.value = null
      sendBtcPopup setAddress adr

    case unusable =>
      println(unusable)
      app.TransData.value = null
  }

  def localBitcoinsAndGlidera = {
    val uri = Uri parse "https://testnet.manu.backend.hamburg/faucet"
    me startActivity new Intent(Intent.ACTION_VIEW, uri)
  }

  def goReceiveBtcAddress(section: View) = {
    app.TransData.value = app.kit.currentAddress
    me goTo classOf[RequestActivity]
  }

  def goQR(top: View) = me goTo classOf[ScanActivity]
  def goLN(top: View) = me goTo classOf[LNActivity]
  def goPay(top: View) = sendBtcPopup

  def toggle(v: View) = {
    // Expand or collapse all txs
    // adapter contains all history

    adapter.switch
    adapter set adapter.availableItems
    adapter.notifyDataSetChanged
  }

  def sendBtcPopup: BtcManager = {
    val content = getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), me getString action_bitcoin_send, content)
    val rateManager = new RateManager(getString(amount_hint_can_send).format(denom withSign app.kit.conf1Balance), content)
    val spendManager = new BtcManager(rateManager)

    def sendAttempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case _ if spendManager.getAddress == null => app toast dialog_address_wrong
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_small

      case ok @ Success(ms) =>
        val processor = new TxProcessor {
          val pay = AddrData(ms, spendManager.getAddress)

          override def processTx(pass: String, feePerKb: Coin) = {
            add(me getString tx_announcing, Informer.BTCEVENT).flash.run
            <(app.kit blockingSend makeTx(pass, feePerKb), onTxFail)(none)
          }

          override def onTxFail(err: Throwable) =
            mkForm(mkChoiceDialog(me delayUI sendBtcPopup.set(ok, pay.address), none,
              dialog_ok, dialog_cancel), messageWhenMakingTx(err), null)
        }

        // Initiate the spending sequence
        rm(alert)(processor.chooseFee)
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(sendAttempt)
    spendManager
  }

  def viewMnemonic(top: View) = passWrap(me getString sets_mnemonic) apply checkPassNotify(doViewMnemonic)
  def nativeTransactions = app.kit.wallet.getRecentTransactions(adapter.max, false).asScala
    .toVector.map(bitcoinjTx2Wrap).filterNot(_.nativeValue.isZero)
}