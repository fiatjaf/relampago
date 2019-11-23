package com.lightning.walletapp

import android.view._
import android.widget._
import org.bitcoinj.core._
import collection.JavaConverters._
import com.lightning.walletapp.ln._
import org.bitcoinj.core.listeners._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.FragWallet._
import com.lightning.walletapp.R.drawable._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import com.lightning.walletapp.ln.Tools.{none, random, runAnd, wrap}
import com.lightning.walletapp.lnutils.JsonHttpUtils.{queue, to}
import com.lightning.walletapp.helper.{ReactLoader, RichCursor}
import fr.acinq.bitcoin.{MilliSatoshi, MilliSatoshiLong}
import android.database.{ContentObserver, Cursor}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import scala.util.{Failure, Success, Try}
import android.os.{Bundle, Handler}

import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
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
import org.bitcoinj.uri.BitcoinURI
import android.app.AlertDialog
import scodec.bits.ByteVector


object FragWallet {
  var worker: FragWalletWorker = _
  val REDIRECT = "goToLnOpsActivity"
  val RECEIVE = "openReceiveChooser"
}

class FragWallet extends Fragment {
  override def onCreateView(inf: LayoutInflater, viewGroup: ViewGroup, bundle: Bundle) = inf.inflate(R.layout.frag_view_pager_btc, viewGroup, false)
  override def onViewCreated(view: View, state: Bundle) = if (app.isAlive) worker = new FragWalletWorker(getActivity.asInstanceOf[WalletActivity], view)
  override def onDestroy = wrap(super.onDestroy)(worker.onFragmentDestroy)
  override def onResume = wrap(super.onResume)(worker.host.checkTransData)
}

class FragWalletWorker(val host: WalletActivity, frag: View) extends SearchBar with HumanTimeDisplay { me =>
  import host.{UITask, onButtonTap, showForm, negBuilder, baseBuilder, negTextBuilder, str2View, onTap, onFail}
  import host.{TxProcessor, mkCheckForm, <, mkCheckFormNeutral, getSupportActionBar, rm}

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
  val lnStatusExpiry = app.getResources getStringArray R.array.ln_status_expiry
  val txsConfs = app.getResources getStringArray R.array.txs_confs
  val iconDict = Array(await, await, conf1, dead, unknown)

  val blocksTitleListener = new BlocksListener {
    def onBlocksDownloaded(peer: Peer, block: Block, fb: FilteredBlock, left: Int) =
    // Limit for performance reasons, we don't need faster updates anyway
      if (left % blocksPerDay == 0) updTitleTask.run
  }

  val peersListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(peer: Peer, leftPeers: Int) = if (leftPeers < 1) updTitleTask.run
    def onPeerConnected(peer: Peer, leftPeers: Int) = if (leftPeers == 1) updTitleTask.run
  }

  val txsListener = new TxTracker with TransactionConfidenceEventListener {
    // isGreaterThan check because as of now both listeners are fired on incoming and outgoing txs
    def onCoinsSent(w: Wallet, txj: Transaction, a: Coin, b: Coin) = if (a isGreaterThan b) updBtcItems
    def onCoinsReceived(w: Wallet, txj: Transaction, a: Coin, b: Coin) = if (b isGreaterThan a) updBtcItems

    def onTransactionConfidenceChanged(w: Wallet, txj: Transaction) =
      if (txj.getConfidence.getDepthInBlocks == minDepth)
        UITask(adapter.notifyDataSetChanged).run
  }

  val loaderCallbacks = new LoaderCallbacks[Cursor] {
    def onCreateLoader(id: Int, bn: Bundle) = new ReactLoader[PaymentInfo](host) {
      val consume = (payments: PaymentInfoVec) => runAnd(lnItems = payments map LNWrap)(updPaymentList.run)
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
  val lnStatusHostedSuspended = app getString ln_status_hosted_suspended
  val lnStatusOperationalMany = app getString ln_status_operational_many
  val lnStatusOperationalOne = app getString ln_status_operational_one
  val lnEmpty = app getString ln_empty

  val oneBtc = MilliSatoshi(100000000000L)
  val btcSyncInfo = app.getResources getStringArray R.array.info_progress
  val btcStatusOperational = app getString btc_status_operational
  val btcStatusConnecting = app getString btc_status_connecting
  val btcEmpty = app getString btc_empty

  val updTitleTask = UITask {
    val suspended = ChannelManager.all.filter(_.isHosted).filterNot(isOperational)
    val allViable = ChannelManager.all.filter(isOpeningOrOperational)
    val online = allViable.count(chan => OPEN == chan.state)
    val delta = allViable.size - online

    val btcTotalSum = coin2MSat(app.kit.conf0Balance)
    val lnTotalSum = MilliSatoshi(allViable.map(_.refundableMsat).sum)
    val btcFunds = if (btcTotalSum.amount < 1) btcEmpty else denom parsedWithSign btcTotalSum
    val lnFunds = if (lnTotalSum.amount < 1) lnEmpty else denom parsedWithSign lnTotalSum
    val perOneBtcRate = formatFiat.format(msatInFiat(oneBtc) getOrElse 0L)

    val btcSubtitleText =
      if (ChannelManager.currentBlocksLeft.isEmpty) btcStatusConnecting
      else if (ChannelManager.blockDaysLeft <= 1) btcStatusOperational
      else app.plur1OrZero(btcSyncInfo, ChannelManager.blockDaysLeft)

    val lnSubtitleText =
      if (suspended.nonEmpty) lnStatusHostedSuspended
      else if (delta == 0 && allViable.size == 1) lnStatusOperationalOne
      else if (delta == 0 && allViable.size > 1) lnStatusOperationalMany
      else app.plur1OrZero(lnStateInfo, delta)

    lnStatus setText lnSubtitleText.html
    lnBalance setText s"<img src='lnbig'/>$lnFunds".html
    fiatRate setText s"<small>$perOneBtcRate</small>".html
    fiatBalance setText msatInFiatHuman(lnTotalSum + btcTotalSum)
    getSupportActionBar setTitle s"<img src='btcbig'/>$btcFunds".html
    getSupportActionBar setSubtitle btcSubtitleText.html
  }

  // DISPLAYING ITEMS LIST

  var errorLimit = 5
  var fundTxIds = Set.empty[String]
  var lnItems = Vector.empty[LNWrap]
  var btcItems = Vector.empty[BTCWrap]
  var allItems = Vector.empty[ItemWrap]
  var sentHostedPreimages = Map.empty[ByteVector, Long]
  val minLinesNum = 4 max IconGetter.scrHeight.ceil.toInt
  var currentCut = minLinesNum

  val chanListener = new ChannelListener {
    def informOfferClose(chan: Channel, message: String, natRes: Int) = UITask {
      val bld = baseBuilder(title = chan.data.announce.asString.html, body = message)
      def onAccepted(alert: AlertDialog) = rm(alert)(chan process ChannelManager.CMDLocalShutdown)
      if (errorLimit > 0) mkCheckFormNeutral(_.dismiss, none, onAccepted, bld, dialog_ok, noResource = -1, natRes)
      errorLimit -= 1
    }

    override def onProcessSuccess = {
      // Hosted channel provider sent us an error, let user know
      case (chan: HostedChannel, _: HostedCommits, remoteError: wire.Error) =>
        informOfferClose(chan, ChanErrorCodes.translateTag(remoteError).getMessage, natRes = -1).run

      // Let user manually choose if remotely proposed override should be applied if chan is suspended
      case (chan: HostedChannel, hc: HostedCommits, so: wire.StateOverride) if chan.state == SUSPENDED =>
        proposeOverride(CMDHostedStateOverride(so), chan, hc).run

      // Notify user that preimage has been revealed to hosted channel
      case (_: HostedChannel, _: HostedCommits, _: CMDFulfillHtlc) =>
        updPaymentList.run

      // Peer has sent us an error, offer user to force-close this channel
      case (chan: NormalChannel, _: HasNormalCommits, remoteError: wire.Error) =>
        informOfferClose(chan, remoteError.exception.getMessage, ln_chan_close).run

      // Peer now has some incompatible features, display details to user and offer to force-close a channel
      case (chan: NormalChannel, _: NormalData, cr: ChannelReestablish) if cr.myCurrentPerCommitmentPoint.isEmpty =>
        informOfferClose(chan, app.getString(err_ln_peer_incompatible).format(chan.data.announce.alias), ln_chan_close).run
    }

    override def onBecome = {
      case (_, _, fromState, CLOSING) if fromState != CLOSING => updPaymentList.run
      case (_, _, fromState, toState) if fromState != toState => updTitleTask.run
    }

    override def onException = {
      case _ \ CMDAddImpossible(rd, code) =>
        // Route has been chosen and sent to selected channel but then exception was thrown in `sendAdd`
        // first try to use the rest of available routes and finally inform user if no more routes left
        ChannelManager.sendEither(useFirstRoute(rd.routes.tail, rd), me informNoRoutesMatched code)

      case chan \ internalException =>
        val bld = negTextBuilder(dialog_ok, UncaughtHandler toText internalException)
        UITask(host showForm bld.setCustomTitle(chan.data.announce.asString.html).create).run
    }
  }

  def proposeOverride(cmd: CMDHostedStateOverride, chan: HostedChannel, hc: HostedCommits) = UITask {
    val currentBalance = s"<strong>${denom.parsedWithSign(chan.estCanSendMsat.toTruncatedMsat).html}</strong>"
    val proposedBalance = s"<strong>${denom.parsedWithSign(hc.newLocalBalanceMsat(cmd.so).toTruncatedMsat).html}</strong>"
    val hostedOverrideDetails = app.getString(ln_hosted_override_warn).format(currentBalance, proposedBalance).html
    val bld = host.baseTextBuilder(hostedOverrideDetails).setCustomTitle(chan.data.announce.asString.html)
    mkCheckForm(alert => rm(alert)(chan process cmd), none, bld, dialog_ok, dialog_cancel)
  }

  def informNoRoutesMatched(code: Int)(rd: RoutingData) = {
    val builder = negTextBuilder(dialog_ok, app getString code)
    UITask(host showForm builder.create).run
    PaymentInfoWrap failOnUI rd
  }

  def updPaymentList = UITask {
    TransitionManager beginDelayedTransition mainWrap
    val delayedWraps = ChannelManager.delayedPublishes map ShowDelayedWrap
    val tempItemWraps = if (isSearching) lnItems else delayedWraps ++ btcItems ++ lnItems
    fundTxIds = ChannelManager.all.collect { case normalChannel: NormalChannel => normalChannel.fundTxId.toHex }.toSet
    sentHostedPreimages = ChannelManager.all.map(_.data).collect { case hc: HostedCommits => hc.sentPreimages }.flatten.toMap
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
      val holder = if (null == view.getTag) ViewHolder(view) else view.getTag.asInstanceOf[ViewHolder]
      // Reset bg to transparent each time because LN slot may mark a revealed preimage
      view setBackgroundColor 0x00000000
      getItem(position) fillView holder
      view
    }
  }

  case class ViewHolder(view: View) {
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
      val useExplorerButton = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]
      useExplorerButton setOnClickListener onButtonTap(host browse s"https://smartbit.com.au/tx/$txid")

      val inFiat = msatInFiatHuman(stat.amount)
      val base = app.getString(btc_pending_title)
      val humanFee = denom.coloredOut(stat.fee, denom.sign)
      val paidFeePct = stat.fee.amount / (stat.amount.amount / 100D)
      val title = base.format(humanWhen, humanSum, inFiat, humanFee, paidFeePct)
      showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)
    }
  }

  case class LNWrap(info: PaymentInfo) extends ItemWrap {
    // Blocks left until expiration if hosted preimage is revealed
    def hostedLeft = sentHostedPreimages.get(info.preimage)
    val getDate = new java.util.Date(info.stamp)

    def fillView(holder: ViewHolder) = {
      if (hostedLeft.isDefined) holder.view.setBackgroundColor(Denomination.yellowHighlight)
      val humanAmount = if (info.isLooper) denom.coloredP2WSH(info.firstSum, new String)
        else if (info.incoming == 1) denom.coloredIn(info.firstSum, new String)
        else denom.coloredOut(info.firstSum, new String)

      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactWhat setVisibility viewMap(isTablet || isSearching)
      holder.transactWhat setText getDescription(info.description).html
      holder.transactSum setText s"<img src='ln'/>$humanAmount".html
      holder.transactCircle setImageResource iconDict(info.status)
    }

    def generatePopup = {
      val humanStatus = info.status match {
        case FAILURE if 0 == info.incoming => s"<strong>${app getString ln_state_fail_out}</strong>"
        case FAILURE if 1 == info.incoming => s"<strong>${app getString ln_state_fail_in}</strong>"
        case SUCCESS => s"<strong>${app getString ln_state_success}</strong>"
        case FROZEN => s"<strong>${app getString ln_state_unknown}</strong>"
        case _ => s"<strong>${app getString ln_state_wait}</strong>"
      }

      val inFiat = msatInFiatHuman(info.firstSum)
      val newRD = app.emptyRD(info.pr, info.firstMsat, useCache = false)
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

      PaymentInfoWrap.acceptedPayments get newRD.pr.paymentHash foreach { rd1 =>
        val routingPath = for (usedHop <- rd1.usedRoute) yield usedHop.humanDetails
        val failures = PaymentInfo.errors(newRD.pr.paymentHash) map { case fail \ _ => fail.toString } mkString "\n==\n"
        val receiverInfo = s"Payee node ID: ${rd1.pr.nodeId.toString}, Expiry: ${rd1.pr.adjustedMinFinalCltvExpiry} blocks"
        val debugInfo = ("Your wallet" +: routingPath :+ receiverInfo mkString "\n-->\n") + s"\n\n$failures"
        paymentDebug setOnClickListener onButtonTap(host share debugInfo)
        paymentDebug setVisibility View.VISIBLE
      }

      def outgoingTitle = {
        val fee = MilliSatoshi(info.lastMsat - info.firstMsat)
        val paidFeePercent = fee.amount / (info.firstMsat / 100D)
        val sentHuman = if (info.isLooper) denom.coloredP2WSH(info.firstSum, denom.sign) else denom.coloredOut(info.firstSum, denom.sign)
        app.getString(ln_outgoing_title).format(humanStatus, sentHuman, inFiat, denom.coloredOut(fee, denom.sign), paidFeePercent)
      }

      info.incoming -> hostedLeft match {
        case 0 \ _ if info.lastExpiry == 0 =>
          // This payment has not been tried yet, could be a failure because offline or no routes found, do not allow to retry it
          val title = app.getString(ln_outgoing_title_no_fee).format(humanStatus, denom.coloredOut(info.firstSum, denom.sign), inFiat)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

        case 0 \ _ if info.status == FAILURE =>
          // This payment has been tried and failed, allow to retry this one unless its payment request has expired already
          val show = mkCheckForm(_.dismiss, doSendOffChain(newRD), baseBuilder(outgoingTitle.html, detailsWrapper), dialog_ok, _: Int)
          if (info.pr.isFresh) show(dialog_retry) else show(-1)

        case 0 \ _ =>
          // This payment is SUCCESSFUL or WAITING or UNKNOWN, show blocks until expiry in a latter case
          val expiryBlocksLeft = app.plur1OrZero(lnStatusExpiry, info.lastExpiry - broadcaster.currentHeight)
          val title = if (info.status == WAITING) s"$expiryBlocksLeft<br><br>$outgoingTitle" else outgoingTitle
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

        case 1 \ Some(blocksUntilExpiry) if info.isLooper && info.status == WAITING =>
          // This is a reflexive payment, we are receiving through a hosted channel and preimage is revealed
          val expiryBlocksLeft = app.plur1OrZero(lnStatusExpiry, blocksUntilExpiry - broadcaster.currentHeight)
          val title = s"$expiryBlocksLeft<br><br>${app getString ln_hosted_preimage_revealed}<br><br>$outgoingTitle"
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

        case 1 \ Some(blocksUntilExpiry) if info.status == WAITING =>
          // This is a ordinary payment, we are receiving through a hosted channel and preimage is revealed
          val expiryBlocksLeft = app.plur1OrZero(lnStatusExpiry, blocksUntilExpiry - broadcaster.currentHeight)
          val incomingTitle = app.getString(ln_incoming_title).format(humanStatus, denom.coloredIn(info.firstSum, denom.sign), inFiat)
          val title = s"$expiryBlocksLeft<br><br>${app getString ln_hosted_preimage_revealed}<br><br>$incomingTitle"
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

        case 1 \ _ =>
          // This payment is SUCCESSFUL or FAILED or UNKNOWN or WAITING, send user to QR page in a latter case
          val incomingTitle = app.getString(ln_incoming_title).format(humanStatus, denom.coloredIn(info.firstSum, denom.sign), inFiat)
          if (info.isLooper) showForm(alertDialog = negBuilder(dialog_ok, title = outgoingTitle.html, body = detailsWrapper).create)
          else if (info.status != WAITING) showForm(negBuilder(dialog_ok, incomingTitle.html, detailsWrapper).create)
          else host PRQR info.pr
      }
    }
  }

  case class BTCWrap(wrap: TxWrap) extends ItemWrap {
    def txDead = DEAD == wrap.tx.getConfidence.getConfidenceType
    def txDepth = wrap.tx.getConfidence.getDepthInBlocks
    val getDate = wrap.tx.getUpdateTime
    val txid = wrap.tx.getHashAsString

    def fillView(holder: ViewHolder) = {
      val humanAmount = if (fundTxIds contains txid) denom.coloredP2WSH(-wrap.visibleValue, new String)
      else if (wrap.visibleValue.isPositive) denom.coloredIn(wrap.visibleValue, new String)
      else denom.coloredOut(-wrap.visibleValue, new String)

      val status = if (txDead) dead else if (txDepth >= minDepth) conf1 else await
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactSum setText s"<img src='btc'/>$humanAmount".html
      holder.transactWhat setVisibility viewMap(isTablet)
      holder.transactCircle setImageResource status
      holder.transactWhat setText txid
    }

    def generatePopup = {
      val confs = if (txDead) txsConfs.last else app.plur1OrZero(txsConfs, txDepth)
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val humanValues = wrap.directedScriptPubKeysWithValueTry(wrap.visibleValue.isPositive) collect {
        case Success(channelFunding \ value) if channelFunding.isSentToP2WSH => P2WSHData(value, channelFunding)
        case Success(pks \ value) if !ScriptPattern.isOpReturn(pks) => AddrData(value, pks getToAddress app.params)
      } collect {
        case contract: P2WSHData => contract destination denom.coloredP2WSH(contract.cn, denom.sign)
        case incoming: AddrData if wrap.visibleValue.isPositive => incoming destination denom.coloredIn(incoming.cn, denom.sign)
        case outgoingPayment: AddrData => outgoingPayment destination denom.coloredOut(outgoingPayment.cn, denom.sign)
      }

      val useExplorerButton = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]
      useExplorerButton setOnClickListener onButtonTap(host browse s"https://smartbit.com.au/tx/$txid")
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
        .filter(_.estCanReceiveMsat >= sum.amount) // In principle can receive an amount
        .sortBy(_.estCanReceiveMsat) // First use channels with the smallest balance but still able to receive
        .sortBy(chan => if (chan.isHosted) 1 else 0) // Hosted channels are pushed to the back of the queue
        .take(4) // Limit number of channels to ensure QR code is always readable

      val preimage = ByteVector.view(random getBytes 32)
      val extraRoutes = mostViableChannels map chansWithRoutes
      val description = inputDescription.getText.toString.take(72).trim
      PaymentInfoWrap.recordRoutingDataWithPr(extraRoutes, sum, preimage, description)
    }

    def recAttempt(alert: AlertDialog) = rateManager.result match {
      case Success(ms) if maxCanReceive < ms => app quickToast dialog_sum_big
      case Success(ms) if minCanReceive > ms => app quickToast dialog_sum_small
      case Failure(reason) => app quickToast dialog_sum_small

      case Success(ms) => rm(alert) {
        // Requests without amount are not allowed
        <(makeNormalRequest(ms), onFail)(onDone)
      }
    }

    def setMax(alert: AlertDialog) = rateManager setSum Try(maxCanReceive)
    mkCheckFormNeutral(recAttempt, none, setMax, bld, dialog_ok, dialog_cancel, dialog_max)
    if (maxCanReceive == minCanReceive) setMax(null) // Since user can't set anything else
    inputDescription setText desc
  }

  abstract class OffChainSender(val maxCanSend: MilliSatoshi, val minCanSend: MilliSatoshi) {
    val baseContent = host.getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
    val baseHint = app.getString(amount_hint_can_send).format(denom parsedWithSign maxCanSend)
    val rateManager = new RateManager(baseContent) hint baseHint

    def getTitle: View
    def displayPaymentForm: Unit
    def onUserAcceptSend(ms: MilliSatoshi): Unit

    def sendAttempt(alert: AlertDialog) = rateManager.result match {
      case Success(ms) if maxCanSend < ms => app quickToast dialog_sum_big
      case Success(ms) if ms < minCanSend => app quickToast dialog_sum_small
      case Failure(emptyAmount) => app quickToast dialog_sum_small
      case Success(ms) => rm(alert)(this onUserAcceptSend ms)
    }
  }

  def standardOffChainSend(pr: PaymentRequest) =
    new OffChainSender(maxCanSend = ChannelManager.estimateAIRCanSend.millisatoshi, minCanSend = pr.msatOrMin) {
      def displayPaymentForm = mkCheckForm(sendAttempt, none, baseBuilder(getTitle, baseContent), dialog_pay, dialog_cancel)
      def getTitle = str2View(app.getString(ln_send_title).format(Utils getDescription pr.description).html)

      def onUserAcceptSend(ms: MilliSatoshi) = {
        val rd = app.emptyRD(pr, firstMsat = ms.amount, useCache = true)
        val rd1 = rd.copy(airLeft = ChannelManager.all count isOperational)
        doSendOffChain(rd1)
      }

      pr.fallbackAddress -> pr.amount match {
        case Some(adr) \ Some(amount) if amount > maxCanSend && amount < app.kit.conf0Balance =>
          def sendOnChain: Unit = sendBtcPopup(app.TransData toBitcoinUri adr) setSum Try(amount)
          val failureMsg = app getString err_ln_not_enough format s"<strong>${denom parsedWithSign amount}</strong>"
          // We have operational channels but can't fulfill this off-chain, yet have enough funds in our on-chain wallet so offer fallback option
          mkCheckFormNeutral(_.dismiss, none, alert => rm(alert)(sendOnChain), baseBuilder(getTitle, failureMsg.html), dialog_ok, -1, dialog_pay_onchain)

        case _ \ Some(amount) if amount > maxCanSend =>
          val failureMsg = app getString err_ln_not_enough format s"<strong>${denom parsedWithSign amount}</strong>"
          // Either this payment request contains no fallback address or we don't have enough funds on-chain
          showForm(negBuilder(dialog_ok, getTitle, failureMsg.html).create)

        case _ =>
          // We can pay this request off-chain
          // show payment form and set a default sum is possible
          for (amount <- pr.amount) rateManager setSum Try(amount)
          displayPaymentForm
      }
    }

  def lnurlPayOffChainSend(domain: String, payReq: PayRequest) = {
    new OffChainSender(maxCanSend = math.min(ChannelManager.estimateAIRCanSend, payReq.maxSendable).millisatoshi, minCanSend = payReq.minSendable.millisatoshi) {
      def displayPaymentForm = mkCheckFormNeutral(sendAttempt, none, visitSite, baseBuilder(getTitle, baseContent), dialog_ok, dialog_cancel, dialog_info)
      def visitSite(alert: AlertDialog) = host.browse(s"https://$domain")

      def getTitle = {
        val content = s"<strong><big>$domain</big></strong><br><br>${payReq.metaDataTextPlain take 72}"
        host.updateView2Blue(oldView = str2View(new String), app.getString(ln_send_title) format content)
      }

      def onUserAcceptSend(ms: MilliSatoshi) = {
        // Issue second level call and send payment
        host toast ln_url_payreq_processing

        def convert(raw: String) = {
          val prf = to[PayRequestFinal](raw)
          require(prf.descriptionHash == payReq.metaDataHash, s"Metadata hash mismatch, original=${payReq.metaDataHash}, provided=${prf.descriptionHash}")
          require(prf.paymentRequest.amount.contains(ms), s"Payment amount mismatch, requested=$ms, provided=${prf.paymentRequest.amount}")
          prf
        }

        def send(prf: PayRequestFinal) = {
          val rd = app.emptyRD(prf.paymentRequest, firstMsat = ms.toLong, useCache = true)
          val rd1 = rd.copy(airLeft = ChannelManager.all count isOperational)
          UITask(me doSendOffChain rd1).run
        }

        queue.map(_ => payReq.requestFinal(ms).body)
          .map(LNUrl.guardResponse).map(convert)
          .foreach(send, onFail)
      }

      if (maxCanSend < minCanSend) {
        val failureMessage = app getString err_ln_not_enough format s"<strong>${denom parsedWithSign minCanSend}</strong>"
        // We can send less off-chain than minimum amount they are willing to receive as specified in lnurl pay request
        showForm(negBuilder(dialog_ok, getTitle, failureMessage.html).create)
      } else {
        if (maxCanSend == minCanSend) rateManager setSum Try(minCanSend)
        // Equal min/max is a special case when payment sum is strict
        displayPaymentForm
      }
    }
  }

  def doSendOffChain(rd: RoutingData): Unit = {
    if (ChannelManager.currentBlocksLeft.isEmpty) host toast err_ln_chain_wait
    val sendableDirectlyOrAlternatives = ChannelManager.checkIfSendable(rd)

    val accumulatorChannel = ChannelManager.all
      .filter(chan => isOperational(chan) && channelAndHop(chan).nonEmpty) // Can in principle be used for receiving
      .filter(chan => chan.estCanReceiveMsat + chan.estCanSendMsat >= maxAcceptableFee(rd.firstMsat, hops = 3) + rd.firstMsat)
      .sortBy(_.estCanReceiveMsat).headOption // The one closest to needed amount, meaning as few/low AIR transfers as possible

    sendableDirectlyOrAlternatives -> accumulatorChannel match {
      case Left(_ \ SENDABLE_AIR) \ Some(accum) => <(startAIR(accum, rd), onFail)(none)
      case Left(unsendableAirImpossible \ _) \ _ => app quickToast unsendableAirImpossible
      case _ => PaymentInfoWrap addPendingPayment rd
    }
  }

  def startAIR(toChan: Channel, origEmptyRD: RoutingData) = {
    val rd1 = origEmptyRD.copy(airLeft = origEmptyRD.airLeft - 1)
    val deltaAmountToSend = maxAcceptableFee(rd1.firstMsat, hops = 3) + rd1.firstMsat - math.max(toChan.estCanSendMsat, 0L)
    val amountCanRebalance = ChannelManager.airCanSendInto(targetChan = toChan).reduceOption(_ max _) getOrElse 0L
    require(deltaAmountToSend > 0, "Accumulator already has enough money for a final payment + fees")
    require(amountCanRebalance > 0, "No channel is able to send funds into accumulator")

    val Some(_ \ extraHop) = channelAndHop(toChan)
    val finalAmount = MilliSatoshi(amount = deltaAmountToSend min amountCanRebalance)
    val rbRD = PaymentInfoWrap.recordRoutingDataWithPr(Vector(extraHop), finalAmount, ByteVector(random getBytes 32), REBALANCING)

    val listener = new ChannelListener {
      override def outPaymentAccepted(rd: RoutingData) =
      // User may send a different payment while AIR is active, halt AIR if that happens
      // also this is desired happen when AIR fails and then user sends a different payment
        if (rd.pr.paymentHash != rbRD.pr.paymentHash) ChannelManager detachListener this

      override def onSettled(cs: Commitments) = {
        val isOK = cs.localSpec.fulfilledOutgoing.exists(_.paymentHash == rbRD.pr.paymentHash)
        // Same accumulator will be chosen next time because its balance would be more attractive
        if (isOK) host.timer.schedule(me doSendOffChain rd1, 250L)
        if (isOK) ChannelManager detachListener this
      }
    }

    ChannelManager attachListener listener
    UITask(me doSendOffChain rbRD).run
  }

  // BTC SEND / BOOST

  def sendBtcPopup(uri: BitcoinURI): RateManager = {
    val minMsatAmountTry = Try(uri.getAmount).filter(org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT.isLessThan).map(coin2MSat)
    val minMsatAmount = minMsatAmountTry getOrElse coin2MSat(org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT)
    val baseHint = app.getString(amount_hint_can_send).format(denom parsedWithSign app.kit.conf0Balance)
    val content = host.getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
    val rateManager = new RateManager(content) hint baseHint

    def sendAttempt(alert: AlertDialog): Unit = rateManager.result match {
      case Success(small) if small < minMsatAmount => app quickToast dialog_sum_small
      case Failure(probablyEmptySum) => app quickToast dialog_sum_small

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

    def useMax(alert: AlertDialog) = rateManager setSum Try(app.kit.conf0Balance)
    val title = app.getString(btc_send_title).format(Utils humanSix uri.getAddress.toString).html
    mkCheckFormNeutral(sendAttempt, none, useMax, baseBuilder(title, content), dialog_next, dialog_cancel, dialog_max)
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
      // Parent transaction hiding must happen before child is broadcasted
      wrap.makeHidden

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

  def reg(chan: Channel) = {
    ChannelManager.all +:= chan
    chan.listeners = ChannelManager.operationalListeners
    ChannelManager attachListener chanListener
    updTitleTask.run
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