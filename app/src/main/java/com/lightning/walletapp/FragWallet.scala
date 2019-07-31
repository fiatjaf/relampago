package com.lightning.walletapp

import android.view._
import android.widget._
import org.bitcoinj.core._
import collection.JavaConverters._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.FragWallet._
import com.lightning.walletapp.R.drawable._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.ln.NormalChannel._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.ln.Tools.{none, random, runAnd, wrap}
import com.lightning.walletapp.helper.{ReactLoader, RichCursor}
import android.database.{ContentObserver, Cursor}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import scala.util.{Failure, Success, Try}
import android.os.{Bundle, Handler}

import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import android.support.v4.app.LoaderManager.LoaderCallbacks
import com.lightning.walletapp.lnutils.IconGetter.isTablet
import org.bitcoinj.wallet.SendRequest.childPaysForParent
import com.lightning.walletapp.ln.wire.ChannelReestablish
import android.transition.TransitionManager
import android.support.v4.content.Loader
import android.support.v7.widget.Toolbar
import org.bitcoinj.script.ScriptPattern
import android.support.v4.app.Fragment
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import android.app.AlertDialog
import android.content.Intent
import scodec.bits.ByteVector
import android.net.Uri


object FragWallet {
  var worker: FragWalletWorker = _
  val REDIRECT = "goToLnOpsActivity"
}

class FragWallet extends Fragment {
  override def onCreateView(inf: LayoutInflater, viewGroup: ViewGroup, bundle: Bundle) = inf.inflate(R.layout.frag_view_pager_btc, viewGroup, false)
  override def onViewCreated(view: View, state: Bundle) = if (app.isAlive) worker = new FragWalletWorker(getActivity.asInstanceOf[WalletActivity], view)
  override def onDestroy = wrap(super.onDestroy)(worker.onFragmentDestroy)
  override def onResume = wrap(super.onResume)(worker.host.checkTransData)
}

class FragWalletWorker(val host: WalletActivity, frag: View) extends SearchBar with HumanTimeDisplay { me =>
  import host.{UITask, onButtonTap, showForm, negBuilder, baseBuilder, negTextBuilder, str2View, onTap, onFail, rm}
  import host.{TxProcessor, mkCheckForm, <, mkCheckFormNeutral, getSupportActionBar}

  val lnStatus = frag.findViewById(R.id.lnStatus).asInstanceOf[TextView]
  val lnBalance = frag.findViewById(R.id.lnBalance).asInstanceOf[TextView]
  val lnDetails = frag.findViewById(R.id.lnDetails).asInstanceOf[LinearLayout]

  val fiatRate = frag.findViewById(R.id.fiatRate).asInstanceOf[TextView]
  val fiatBalance = frag.findViewById(R.id.fiatBalance).asInstanceOf[TextView]
  val fiatDetails = frag.findViewById(R.id.fiatDetails).asInstanceOf[LinearLayout]

  val mainWrap = frag.findViewById(R.id.mainWrap).asInstanceOf[LinearLayout]
  val mnemonicWarn = frag.findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
  val itemsList = frag.findViewById(R.id.itemsList).asInstanceOf[ListView]

  val allTxsWrapper = host.getLayoutInflater.inflate(R.layout.frag_toggler, null)
  val toggler = allTxsWrapper.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  val expiryLeft = app.getResources getStringArray R.array.ln_status_expiry
  val txsConfs = app.getResources getStringArray R.array.txs_confs
  val imageMap = Array(await, await, conf1, dead, frozen)

  // LISTENERS

  val blocksTitleListener = new BlocksListener {
    def onBlocksDownloaded(peer: Peer, block: Block, fb: FilteredBlock, left: Int) =
      if (left % blocksPerDay == 0) updTitleTask.run
  }

  val peersListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(peer: Peer, leftPeers: Int) = if (leftPeers < 1) updTitleTask.run
    def onPeerConnected(peer: Peer, leftPeers: Int) = if (leftPeers == 1) updTitleTask.run
  }

  val txsListener = new TxTracker {
    // isGreaterThan check because as of now both listeners are fired on incoming and outgoing txs
    def onCoinsSent(w: Wallet, txj: Transaction, a: Coin, b: Coin) = if (a isGreaterThan b) updBtcItems
    def onCoinsReceived(w: Wallet, txj: Transaction, a: Coin, b: Coin) = if (b isGreaterThan a) updBtcItems
    override def txConfirmed(txj: Transaction) = UITask(adapter.notifyDataSetChanged).run
  }

  // To fight spamming
  private[this] var errorLimit = 5
  val chanListener = new ChannelListener {
    def informOfferClose(chan: NormalChannel, msg: String) = UITask {
      mkCheckFormNeutral(_.dismiss, none, alert => rm(alert)(chan process ChannelManager.CMDLocalShutdown),
        baseBuilder(chan.data.announce.asString.html, msg), dialog_ok, noResource = -1, ln_chan_close)
    }

    override def onSettled(cs: Commitments) =
      if (cs.localSpec.fulfilledIncoming.nonEmpty)
        host stopService host.foregroundServiceIntent

    override def onProcessSuccess = {
      case (chan: NormalChannel, _: HasNormalCommits, remoteError: wire.Error) if errorLimit > 0 =>
        // Peer has sent us an error, display details to user and offer to force-close a channel
        informOfferClose(chan, remoteError.exception.getMessage).run
        errorLimit -= 1

      case (chan: NormalChannel, _: NormalData, cr: ChannelReestablish) if cr.myCurrentPerCommitmentPoint.isEmpty =>
        // Peer now has some incompatible features, display details to user and offer to force-close a channel
        informOfferClose(chan, host getString err_ln_peer_incompatible format chan.data.announce.alias).run
    }

    override def onBecome = {
      case (_, _, prev, SLEEPING) if prev != SLEEPING => updTitleTask.run
      case (_, _, prev, OPEN) if prev != OPEN => updTitleTask.run
    }

    override def onException = {
      case _ \ CMDAddImpossible(rd, code) =>
        // Remove this payment from unsent since it was not accepted by channel
        UITask(host showForm negTextBuilder(dialog_ok, app getString code).create).run
        PaymentInfoWrap failOnUI rd

      case chan \ HTLCHasExpired(_, htlc) =>
        val paymentHash = htlc.add.paymentHash.toHex
        val bld = negTextBuilder(dialog_ok, app.getString(err_ln_expired).format(paymentHash).html)
        UITask(host showForm bld.setCustomTitle(chan.data.announce.asString.html).create).run

      case chan \ internalException =>
        val bld = negTextBuilder(dialog_ok, UncaughtHandler toText internalException)
        UITask(host showForm bld.setCustomTitle(chan.data.announce.asString.html).create).run
    }
  }

  val loaderCallbacks = new LoaderCallbacks[Cursor] {
    def onCreateLoader(id: Int, bn: Bundle) = new ReactLoader[PaymentInfo](host) {
      val consume = (vec: PaymentInfoVec) => runAnd(lnItems = vec map LNWrap)(updPaymentList.run)
      def getCursor = if (lastQuery.isEmpty) PaymentInfoWrap.byRecent else PaymentInfoWrap.byQuery(lastQuery)
      def createItem(rc: RichCursor) = PaymentInfoWrap.toPaymentInfo(rc)
    }

    type LoaderCursor = Loader[Cursor]
    type PaymentInfoVec = Vector[PaymentInfo]
    def onLoaderReset(loaderCursor: LoaderCursor) = none
    def onLoadFinished(loaderCursor: LoaderCursor, c: Cursor) = none
  }

  // UPDATING TITLE

  val lnStateInfo = app.getResources getStringArray R.array.ln_chan_connecting
  val lnStatusOperationalMany = app getString ln_status_operational_many
  val lnStatusOperationalOne = app getString ln_status_operational_one
  val lnEmpty = app getString ln_empty

  val oneBtc = MilliSatoshi(100000000000L)
  val btcSyncInfo = app.getResources getStringArray R.array.info_progress
  val btcStatusOperational = app getString btc_status_operational
  val btcStatusConnecting = app getString btc_status_connecting
  val btcEmpty = app getString btc_empty

  val updTitleTask = UITask {
    val viable = ChannelManager.all.filter(isOpeningOrOperational)
    val online = viable.count(chan => OPEN == chan.state)
    val delta = viable.size - online

    val btcTotalSum = coin2MSat(app.kit.conf0Balance)
    val btcFunds = if (btcTotalSum.amount == 0L) btcEmpty else denom parsedWithSign btcTotalSum
    val lnTotalSum = MilliSatoshi(viable.flatMap(_.getCommits).map(_.reducedRemoteState.inChanMsat).sum)
    val lnFunds = if (lnTotalSum.amount < 1) lnEmpty else denom parsedWithSign lnTotalSum
    val perOneBtcRate = formatFiat.format(msatInFiat(oneBtc) getOrElse 0L)

    val btcSubtitleText =
      if (ChannelManager.currentBlocksLeft.isEmpty) btcStatusConnecting
      else if (ChannelManager.blockDaysLeft <= 1) btcStatusOperational
      else app.plur1OrZero(btcSyncInfo, ChannelManager.blockDaysLeft)

    val lnSubtitleText =
      if (delta == 0 && viable.size == 1) lnStatusOperationalOne
      else if (delta == 0 && viable.size > 1) lnStatusOperationalMany
      else app.plur1OrZero(lnStateInfo, delta)

    lnStatus setText lnSubtitleText.html
    lnBalance setText s"<img src='lnbig'/>$lnFunds".html
    fiatRate setText s"<small>$perOneBtcRate</small>".html
    fiatBalance setText msatInFiatHuman(lnTotalSum + btcTotalSum)
    getSupportActionBar setTitle s"<img src='btcbig'/>$btcFunds".html
    getSupportActionBar setSubtitle btcSubtitleText.html
  }

  // DISPLAYING ITEMS LIST

  var fundTxIds: Set[String] = Set.empty[String]
  var lnItems: Vector[LNWrap] = Vector.empty[LNWrap]
  var btcItems: Vector[BTCWrap] = Vector.empty[BTCWrap]
  var allItems: Vector[ItemWrap] = Vector.empty[ItemWrap]
  val minLinesNum = 4 max IconGetter.scrHeight.ceil.toInt
  var currentCut = minLinesNum

  def updPaymentList = UITask {
    TransitionManager beginDelayedTransition mainWrap
    val delayedWraps = ChannelManager.delayedPublishes map ShowDelayedWrap
    val tempItemWraps = if (isSearching) lnItems else delayedWraps ++ btcItems ++ lnItems
    fundTxIds = ChannelManager.all.collect { case nc: NormalChannel => nc.fundTxId.toHex }.toSet
    allItems = tempItemWraps.sortBy(_.getDate)(Ordering[java.util.Date].reverse) take 48
    adapter.notifyDataSetChanged
    updTitleTask.run

    allTxsWrapper setVisibility viewMap(allItems.size > minLinesNum)
    mnemonicWarn setVisibility viewMap(allItems.isEmpty)
    itemsList setVisibility viewMap(allItems.nonEmpty)
    fiatDetails setVisibility viewMap(!isSearching)
    lnDetails setVisibility viewMap(!isSearching)
  }

  val adapter = new BaseAdapter {
    def getCount = math.min(allItems.size, currentCut)
    def getItem(position: Int) = allItems(position)
    def getItemId(position: Int) = position

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val resource = if (isTablet) R.layout.frag_tx_line_tablet else R.layout.frag_tx_line
      val view = if (null == savedView) host.getLayoutInflater.inflate(resource, null) else savedView
      val holder = if (null == view.getTag) new ViewHolder(view) else view.getTag.asInstanceOf[ViewHolder]
      getItem(position) fillView holder
      view
    }
  }

  class ViewHolder(view: View) {
    val transactCircle = view.findViewById(R.id.transactCircle).asInstanceOf[ImageView]
    val transactWhen = view.findViewById(R.id.transactWhen).asInstanceOf[TextView]
    val transactWhat = view.findViewById(R.id.transactWhat).asInstanceOf[TextView]
    val transactSum = view.findViewById(R.id.transactSum).asInstanceOf[TextView]
    view setTag this
  }

  abstract class ItemWrap {
    def fillView(v: ViewHolder): Unit
    def getDate: java.util.Date
    def generatePopup: Unit
  }

  case class ShowDelayedWrap(stat: ShowDelayed) extends ItemWrap {
    def getDate = new java.util.Date(System.currentTimeMillis + stat.delay)
    def humanSum = denom.coloredIn(stat.amount, new String)
    val txid = stat.commitTx.txid.toHex

    def humanWhen = {
      val now = System.currentTimeMillis
      val blocksAsMsecs = now + 600000L * stat.delay
      val future = new java.util.Date(blocksAsMsecs)
      when(now, future)
    }

    def fillView(holder: ViewHolder) = {
      holder.transactSum setText s"<img src='btc'/>$humanSum".html
      holder.transactWhat setVisibility viewMap(isTablet)
      holder.transactCircle setImageResource await
      holder.transactWhen setText humanWhen
      holder.transactWhat setText txid
    }

    def generatePopup = {
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button] setOnClickListener onButtonTap {
        host startActivity new Intent(Intent.ACTION_VIEW, Uri parse s"https://smartbit.com.au/tx/$txid")
      }

      val inFiat = msatInFiatHuman(stat.amount)
      val base = app.getString(btc_pending_title)
      val humanFee = denom.coloredOut(stat.fee, denom.sign)
      val paidFeePct = stat.fee.amount / (stat.amount.amount / 100D)
      val title = base.format(humanWhen, humanSum, inFiat, humanFee, paidFeePct)
      showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)
    }
  }

  case class LNWrap(info: PaymentInfo) extends ItemWrap {
    val getDate = new java.util.Date(info.stamp)

    def fillView(holder: ViewHolder) = {
      val humanSum = if (info.isLooper) denom.coloredP2WSH(info.firstSum, new String)
        else if (info.incoming == 1) denom.coloredIn(info.firstSum, new String)
        else denom.coloredOut(info.firstSum, new String)

      holder.transactCircle setImageResource imageMap(info.status)
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactWhat setVisibility viewMap(isTablet || isSearching)
      holder.transactWhat setText getDescription(info.description).html
      holder.transactSum setText s"<img src='ln'/>$humanSum".html
    }

    def generatePopup = {
      val humanStatus = info.incoming -> info.status match {
        case _ \ FROZEN => s"<strong>${app getString ln_state_frozen}</strong>"
        case 0 \ FAILURE => s"<strong>${app getString ln_state_fail_out}</strong>"
        case 1 \ FAILURE => s"<strong>${app getString ln_state_fail_in}</strong>"
        case _ \ SUCCESS => s"<strong>${app getString ln_state_success}</strong>"
        case _ => s"<strong>${app getString ln_state_wait}</strong>"
      }

      val inFiat = msatInFiatHuman(info.firstSum)
      val retry = if (info.pr.isFresh) dialog_retry else -1
      val rd = emptyRD(info.pr, info.firstMsat, useCache = false, airLeft = 0)
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_ln_details, null)
      val paymentDetails = detailsWrapper.findViewById(R.id.paymentDetails).asInstanceOf[TextView]
      val paymentRequest = detailsWrapper.findViewById(R.id.paymentRequest).asInstanceOf[Button]
      val paymentProof = detailsWrapper.findViewById(R.id.paymentProof).asInstanceOf[Button]
      val paymentDebug = detailsWrapper.findViewById(R.id.paymentDebug).asInstanceOf[Button]
      lazy val serializedPR = PaymentRequest write info.pr

      paymentRequest setOnClickListener onButtonTap(host share serializedPR)
      paymentDetails setText getDescription(info.description).html

      if (info.status == SUCCESS) {
        paymentRequest setVisibility View.GONE
        paymentProof setVisibility View.VISIBLE
        paymentProof setOnClickListener onButtonTap {
          // Signed payment request along with a preimage is sufficient proof of payment
          host share app.getString(ln_proof).format(serializedPR, info.preimage.toHex)
        }
      }

      PaymentInfoWrap.acceptedPayments get rd.pr.paymentHash foreach { rd1 =>
        val routingPath = for (usedHop <- rd1.usedRoute) yield usedHop.humanDetails
        val errors = PaymentInfo.errors.getOrElse(rd.pr.paymentHash, Vector.empty).reverse.map(_.toString) mkString "\n==\n"
        val receiverInfo = s"Payee node: ${rd1.pr.nodeId.toString}, Expiry: ${rd1.pr.adjustedMinFinalCltvExpiry} blocks"
        val debugInfo = ("Your wallet" +: routingPath :+ receiverInfo mkString "\n-->\n") + s"\n\n$errors"
        paymentDebug setOnClickListener onButtonTap(host share debugInfo)
        paymentDebug setVisibility View.VISIBLE
      }

      def outgoingTitle = {
        val fee = MilliSatoshi(info.lastMsat - info.firstMsat)
        val paidFeePercent = fee.amount / (info.firstMsat / 100D)
        val sentHuman = if (info.isLooper) denom.coloredP2WSH(info.firstSum, denom.sign) else denom.coloredOut(info.firstSum, denom.sign)
        val title = app.getString(ln_outgoing_title).format(humanStatus, sentHuman, inFiat, denom.coloredOut(fee, denom.sign), paidFeePercent)
        val expiryBlocksLeftPart = app.plur1OrZero(expiryLeft, info.lastExpiry - broadcaster.currentHeight)
        if (info.status == WAITING) s"$expiryBlocksLeftPart<br>$title" else title
      }

      info.incoming -> rd.pr.fallbackAddress -> rd.pr.amount match {
        case 0 \ Some(adr) \ Some(amount) if info.lastExpiry == 0 && info.status == FAILURE =>
          // Payment was failed without even trying because wallet is offline or no suitable routes were found
          mkCheckFormNeutral(_.dismiss, none, neutral = onChain(adr, amount), baseBuilder(app.getString(ln_outgoing_title_no_fee)
            .format(humanStatus, denom.coloredOut(info.firstSum, denom.sign), inFiat).html, detailsWrapper), dialog_ok, -1, dialog_pay_onchain)

        case 0 \ _ \ _ if info.lastExpiry == 0 =>
          // This is not a failure yet, don't care about on-chain
          showForm(alertDialog = negBuilder(neg = dialog_ok, title = app.getString(ln_outgoing_title_no_fee)
            .format(humanStatus, denom.coloredOut(info.firstSum, denom.sign), inFiat).html, detailsWrapper).create)

        case 0 \ Some(adr) \ Some(amount) if info.status == FAILURE =>
          // Offer a fallback on-chain address along with off-chain retry
          mkCheckFormNeutral(_.dismiss, doSendOffChain(rd), neutral = onChain(adr, amount),
            baseBuilder(outgoingTitle.html, detailsWrapper), dialog_ok, retry, dialog_pay_onchain)

        case 0 \ _ \ _ if info.status == FAILURE =>
          // Allow off-chain retry only, no on-chain fallback options since no embedded address is present
          mkCheckForm(_.dismiss, doSendOffChain(rd), baseBuilder(outgoingTitle.html, detailsWrapper), dialog_ok, retry)

        case _ =>
          val incomingTitle = app.getString(ln_incoming_title).format(humanStatus, denom.coloredIn(info.firstSum, denom.sign), inFiat)
          if (info.incoming == 0 || info.isLooper) showForm(negBuilder(dialog_ok, title = outgoingTitle.html, body = detailsWrapper).create)
          else if (info.incoming == 1 && info.status != WAITING) showForm(negBuilder(dialog_ok, incomingTitle.html, detailsWrapper).create)
          else host PRQR info.pr
      }
    }
  }

  case class BTCWrap(wrap: TxWrap) extends ItemWrap {
    private[this] def txDepth = wrap.tx.getConfidence.getDepthInBlocks
    private[this] def txDead = DEAD == wrap.tx.getConfidence.getConfidenceType
    private[this] val txid = wrap.tx.getHashAsString
    val getDate = wrap.tx.getUpdateTime

    def fillView(holder: ViewHolder) = {
      val humanSum = wrap.visibleValue.isPositive match {
        case _ if fundTxIds contains txid => denom.coloredP2WSH(-wrap.visibleValue, new String)
        case false => denom.coloredOut(-wrap.visibleValue, new String)
        case true => denom.coloredIn(wrap.visibleValue, new String)
      }

      val status = if (txDead) dead else if (txDepth >= minDepth) conf1 else await
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactSum setText s"<img src='btc'/>$humanSum".html
      holder.transactWhat setVisibility viewMap(isTablet)
      holder.transactCircle setImageResource status
      holder.transactWhat setText txid
    }

    def generatePopup = {
      val confs = app.plur1OrZero(txsConfs, txDepth)
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val humanValues = wrap.directedScriptPubKeysWithValueTry(wrap.visibleValue.isPositive) collect {
        case Success(channelFunding \ value) if channelFunding.isSentToP2WSH => P2WSHData(value, channelFunding)
        case Success(pks \ value) if !ScriptPattern.isOpReturn(pks) => AddrData(value, pks getToAddress app.params)
      } collect {
        case contract: P2WSHData => contract destination denom.coloredP2WSH(contract.cn, denom.sign)
        case incoming: AddrData if wrap.visibleValue.isPositive => incoming destination denom.coloredIn(incoming.cn, denom.sign)
        case outgoingPayment: AddrData => outgoingPayment destination denom.coloredOut(outgoingPayment.cn, denom.sign)
      }

      detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button] setOnClickListener onButtonTap {
        host startActivity new Intent(Intent.ACTION_VIEW, Uri parse s"https://smartbit.com.au/tx/$txid")
      }

      val views = new ArrayAdapter(host, R.layout.frag_top_tip, R.id.titleTip, humanValues.map(_.html).toArray)
      val lst = host.getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
      lst setHeaderDividersEnabled false
      lst addHeaderView detailsWrapper
      lst setAdapter views
      lst setDivider null

      val header = wrap.fee match {
        case _ if wrap.visibleValue.isPositive =>
          val inFiat = msatInFiatHuman(wrap.visibleValue)
          val receivedHumanAmount = denom.coloredIn(wrap.visibleValue, denom.sign)
          app.getString(btc_incoming_title).format(confs, receivedHumanAmount, inFiat)

        case Some(fee) =>
          // This is an outgoing tx with fee
          val inFiat = msatInFiatHuman(-wrap.visibleValue)
          val paidFeePercent = fee.value / (-wrap.visibleValue.value / 100D)
          val sentHumanAmount = denom.coloredOut(-wrap.visibleValue, denom.sign)
          app.getString(btc_outgoing_title).format(confs, sentHumanAmount, inFiat,
            denom.coloredOut(fee, denom.sign), paidFeePercent)

        case None =>
          // Should never happen but whatever
          val inFiat = msatInFiatHuman(-wrap.visibleValue)
          val humanAmount = denom.coloredOut(-wrap.visibleValue, denom.sign)
          app.getString(btc_outgoing_title_no_fee).format(confs, humanAmount, inFiat)
      }

      // Check if CPFP can be applied: enough value to handle the fee, not dead yet
      if (wrap.valueDelta.isLessThan(RatesSaver.rates.feeSix) || txDepth > 0) showForm(negBuilder(dialog_ok, header.html, lst).create)
      else mkCheckForm(_.dismiss, boostIncoming(wrap), baseBuilder(header.html, lst), dialog_ok, dialog_boost)
    }
  }

  // WORKER EVENT HANDLERS

  def onFragmentDestroy = {
    app.kit.wallet removeTransactionConfidenceEventListener txsListener
    app.kit.peerGroup removeBlocksDownloadedEventListener blocksTitleListener
    app.kit.peerGroup removeDisconnectedEventListener peersListener
    app.kit.peerGroup removeConnectedEventListener peersListener
    app.kit.wallet removeCoinsReceivedEventListener txsListener
    app.kit.wallet removeCoinsSentEventListener txsListener
    ChannelManager detachListener chanListener
  }

  // LN SEND / RECEIVE

  def receive(chansWithRoutes: Map[Channel, PaymentRoute],
              maxCanReceive: MilliSatoshi, minCanReceive: MilliSatoshi,
              title: View, desc: String)(onDone: RoutingData => Unit) = {

    val baseHint = app.getString(amount_hint_can_receive).format(denom parsedWithSign maxCanReceive)
    val content = host.getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
    val inputDescription = content.findViewById(R.id.inputDescription).asInstanceOf[EditText]
    val rateManager = new RateManager(content) hint baseHint
    val bld = baseBuilder(title, content)

    def makeNormalRequest(sum: MilliSatoshi) = {
      val mostViableChannels = chansWithRoutes.keys.toVector
        .filter(chan => chan.estimateCanReceive >= sum.amount) // In principle can receive an amount
        .sortBy(chan => if (chan.isHosted) 1 else 0) // Hosted channels are pushed to the back of the queue
        .sortBy(_.estimateCanReceive) // First use channels with the smallest balance but still able to receive
        .take(4) // Limit number of channels to ensure QR code is always readable

      val preimage = ByteVector.view(random getBytes 32)
      val extraRoutes = mostViableChannels map chansWithRoutes
      val description = inputDescription.getText.toString.take(72).trim
      PaymentInfoWrap.recordRoutingDataWithPr(extraRoutes, sum, preimage, description)
    }

    def recAttempt(alert: AlertDialog) = rateManager.result match {
      case Success(ms) if maxCanReceive < ms => app toast dialog_sum_big
      case Success(ms) if minCanReceive > ms => app toast dialog_sum_small
      case Failure(reason) => app toast dialog_sum_small

      case Success(ms) => rm(alert) {
        // Requests without amount are not allowed
        <(makeNormalRequest(ms), onFail)(onDone)
        app toast dialog_pr_making
      }
    }

    def setMax(alert: AlertDialog) = rateManager setSum Try(maxCanReceive)
    mkCheckFormNeutral(recAttempt, none, setMax, bld, dialog_ok, dialog_cancel, dialog_max)
    if (maxCanReceive == minCanReceive) setMax(null) // Since user can't set anything else
    inputDescription setText desc
  }

  abstract class OffChainSender(pr: PaymentRequest) {
    val maxCanSend = MilliSatoshi(ChannelManager.estimateAIRCanSend)
    val rd = emptyRD(pr, firstMsat = pr.msatOrMin.amount, useCache = true, airLeft = 0)
    val baseContent = host.getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
    val baseHint = app.getString(amount_hint_can_send).format(denom parsedWithSign maxCanSend)
    val rateManager = new RateManager(baseContent) hint baseHint

    def getTitle: View
    def displayPaymentForm: Unit
    def onUserAcceptSend(rd: RoutingData): Unit

    def sendAttempt(alert: AlertDialog) = rateManager.result match {
      case Success(ms) if maxCanSend < ms => app toast dialog_sum_big
      case Success(ms) if pr.amount.exists(_ > ms) => app toast dialog_sum_small
      case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small
      case Failure(emptyAmount) => app toast dialog_sum_small

      case Success(ms) => rm(alert) {
        val attempts = ChannelManager.all.count(isOperational)
        val rd1 = rd.copy(firstMsat = ms.amount, airLeft = attempts)
        onUserAcceptSend(rd1)
      }
    }

    pr.fallbackAddress -> pr.amount match {
      case Some(adr) \ Some(amount) if amount > maxCanSend && amount < app.kit.conf0Balance =>
        val failureMessage = app getString err_ln_not_enough format s"<strong>${denom parsedWithSign amount}</strong>"
        // We have operational channels but can't fulfill this off-chain, yet have enough funds in our on-chain wallet so offer fallback option
        mkCheckFormNeutral(_.dismiss, none, onChain(adr, amount), baseBuilder(getTitle, failureMessage.html), dialog_ok, -1, dialog_pay_onchain)

      case _ \ Some(amount) if amount > maxCanSend =>
        val failureMessage = app getString err_ln_not_enough format s"<strong>${denom parsedWithSign amount}</strong>"
        // Either this payment request contains no fallback address or we don't have enough funds on-chain at all
        showForm(negBuilder(dialog_ok, getTitle, failureMessage.html).create)

      case _ =>
        for (amount <- pr.amount) rateManager setSum Try(amount)
        // We can pay this off-chain, show payment form
        displayPaymentForm
    }
  }

  def standardOffChainSend(pr: PaymentRequest) = new OffChainSender(pr) {
    def displayPaymentForm = mkCheckForm(sendAttempt, none, baseBuilder(getTitle, baseContent), dialog_pay, dialog_cancel)
    def getTitle = str2View(app.getString(ln_send_title).format(Utils getDescription pr.description).html)
    def onUserAcceptSend(rd: RoutingData) = doSendOffChain(rd)
  }

  def doSendOffChain(rd: RoutingData): Unit = {
    val sendableEither = ChannelManager.checkIfSendable(rd)
    val accChanOpt = ChannelManager.accumulatorChanOpt(rd)

    sendableEither -> accChanOpt match {
      case Left(_ \ SENDABLE_AIR) \ Some(acc) => <(offerAir(acc, rd), onFail)(none)
      case Left(unsendableAirNotPossible \ _) \ _ => app toast unsendableAirNotPossible
      case _ => PaymentInfoWrap addPendingPayment rd
    }
  }

  def offerAir(toChan: Channel, origEmptyRD: RoutingData) = {
    val origEmptyRD1 = origEmptyRD.copy(airLeft = origEmptyRD.airLeft - 1)
    val deltaAmountToSend = origEmptyRD1.withMaxOffChainFeeAdded - math.max(toChan.estimateCanSend, 0L)
    val amountCanRebalance = ChannelManager.airCanSendInto(toChan).reduceOption(_ max _) getOrElse 0L
    require(deltaAmountToSend > 0, "Accumulator already has enough money for a final payment")
    require(amountCanRebalance > 0, "No channel is able to send funds into accumulator")

    val Some(_ \ extraHops) = channelAndHop(toChan)
    val finalAmount = MilliSatoshi(deltaAmountToSend min amountCanRebalance)
    val rbRD = PaymentInfoWrap.recordRoutingDataWithPr(Vector(extraHops),
      finalAmount, ByteVector(random getBytes 32), REBALANCING)

    val listener = new ChannelListener { self =>
      override def outPaymentAccepted(rd: RoutingData) =
        if (rd.pr.paymentHash != rbRD.pr.paymentHash)
          ChannelManager detachListener self

      override def onSettled(cs: Commitments) = {
        val isOK = cs.localSpec.fulfilledOutgoing.exists(_.paymentHash == rbRD.pr.paymentHash)
        if (isOK) runAnd(ChannelManager detachListener self) { UITask(me doSendOffChain origEmptyRD1).run }
      }
    }

    ChannelManager attachListener listener
    UITask(me doSendOffChain rbRD).run
  }

  // BTC SEND / BOOST

  def onChain(adr: String, amount: MilliSatoshi)(alert: AlertDialog) = rm(alert) {
    // This code only gets executed when user taps a button to pay on-chain
    sendBtcPopup(app.TransData toBitcoinUri adr) setSum Try(amount)
  }

  def sendBtcPopup(uri: BitcoinURI): RateManager = {
    val minMsatAmountTry = Try(uri.getAmount).filter(org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT.isLessThan).map(coin2MSat)
    val minMsatAmount = minMsatAmountTry getOrElse coin2MSat(org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT)
    val baseHint = app.getString(amount_hint_can_send).format(denom parsedWithSign app.kit.conf0Balance)
    val content = host.getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
    val rateManager = new RateManager(content) hint baseHint

    val maxCanSend = MilliSatoshi(app.kit.conf0Balance.value * 1000L)
    def useMax(alert: AlertDialog) = rateManager setSum Try(maxCanSend)
    def sendAttempt(alert: AlertDialog): Unit = rateManager.result match {
      case Success(small) if small < minMsatAmount => app toast dialog_sum_small
      case Failure(probablyEmptySum) => app toast dialog_sum_small

      case Success(ms) =>
        val txProcessor = new TxProcessor {
          val pay = AddrData(ms, uri.getAddress)
          def futureProcess(unsignedRequest: SendRequest) = app.kit blockSend app.kit.sign(unsignedRequest).tx
          def onTxFail(err: Throwable): Unit = mkCheckForm(_.dismiss, none, txMakeErrorBuilder(err), dialog_ok, -1)
        }

        val coloredAmount = denom.coloredOut(txProcessor.pay.cn, denom.sign)
        val coloredExplanation = txProcessor.pay destination coloredAmount
        rm(alert)(txProcessor start coloredExplanation)
    }

    def sendOffChainAttempt(alert: AlertDialog) = rm(alert) {
      // Bitcoin URI may contain an embedded LN invoice so user need to have an off-chain option
      <(app.TransData recordValue uri.getLightningRequest, onFail)(_ => host.checkTransData)
    }

    val title = app getString btc_send_title format humanSix(uri.getAddress.toString)
    if (uri.getLightningRequest == null) mkCheckFormNeutral(sendAttempt, none, useMax, baseBuilder(title.html, content), dialog_next, dialog_cancel, dialog_max)
    else mkCheckFormNeutral(sendAttempt, none, sendOffChainAttempt, baseBuilder(title.html, content), dialog_next, dialog_cancel, dialog_pay_offchain)
    rateManager setSum minMsatAmountTry
    rateManager
  }

  def boostIncoming(wrap: TxWrap) = {
    val newFee = RatesSaver.rates.feeSix div 2
    val current = denom.coloredIn(wrap.valueDelta, denom.sign)
    val boost = denom.coloredIn(wrap.valueDelta minus newFee, denom.sign)
    // Unlike normal transaction this one uses a whole half of current feeSix
    val userWarn = baseBuilder(app.getString(boost_details).format(current, boost).html, null)
    mkCheckForm(_.dismiss, <(replace, onError)(none), userWarn, dialog_cancel, dialog_boost)

    def replace: Unit = {
      if (wrap.tx.getConfidence.getDepthInBlocks > 0) return
      if (DEAD == wrap.tx.getConfidence.getConfidenceType) return
      wrap.makeHidden

      // Parent transaction hiding must happen before child is broadcasted
      val unsigned = childPaysForParent(app.kit.wallet, wrap.tx, newFee)
      app.kit blockSend app.kit.sign(unsigned).tx
    }

    def onError(err: Throwable) = {
      // Make an old tx visible again
      wrap.tx setMemo null
      onFail(err)
    }
  }

  def updBtcItems = {
    val rawTxs = app.kit.wallet.getRecentTransactions(24, false)
    val wraps = for (txnj <- rawTxs.asScala.toVector) yield new TxWrap(txnj)
    btcItems = for (wrap <- wraps if wrap.isVisible) yield BTCWrap(wrap)
    updPaymentList.run
  }

  def react = android.support.v4.app.LoaderManager.getInstance(host).restartLoader(1, null, loaderCallbacks).forceLoad
  val observer = new ContentObserver(new Handler) { override def onChange(fromSelf: Boolean) = if (!fromSelf) react }
  host.getContentResolver.registerContentObserver(db sqlPath PaymentTable.table, true, observer)
  host setSupportActionBar frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  host.timer.schedule(adapter.notifyDataSetChanged, 10000, 10000)
  Utils clickableTextField frag.findViewById(R.id.mnemonicInfo)
  lnDetails setOnClickListener onButtonTap(host goOps null)

  toggler setOnClickListener onButtonTap {
    val newImg = if (currentCut > minLinesNum) ic_explode_24dp else ic_implode_24dp
    currentCut = if (currentCut > minLinesNum) minLinesNum else allItems.size
    toggler setImageResource newImg
    adapter.notifyDataSetChanged
  }

  itemsList setOnItemClickListener onTap { pos =>
    // Different popups depending on transaction type
    adapter.getItem(pos).generatePopup
  }

  itemsList setFooterDividersEnabled false
  itemsList addFooterView allTxsWrapper
  itemsList setAdapter adapter

  app.kit.wallet addTransactionConfidenceEventListener txsListener
  app.kit.peerGroup addBlocksDownloadedEventListener blocksTitleListener
  app.kit.peerGroup addDisconnectedEventListener peersListener
  app.kit.peerGroup addConnectedEventListener peersListener
  app.kit.wallet addCoinsReceivedEventListener txsListener
  app.kit.wallet addCoinsSentEventListener txsListener
  ChannelManager attachListener chanListener
  runAnd(react)(updBtcItems)
}