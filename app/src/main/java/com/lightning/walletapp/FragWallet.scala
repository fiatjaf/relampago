package com.lightning.walletapp

import spray.json._
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
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import android.os.{Bundle, Handler}
import scala.util.{Failure, Success, Try}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import android.content.{DialogInterface, Intent}
import android.database.{ContentObserver, Cursor}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi}
import com.lightning.walletapp.helper.{ReactLoader, RichCursor}
import org.bitcoinj.core.Transaction.{MIN_NONDUST_OUTPUT => MIN}
import com.lightning.walletapp.ln.Tools.{none, random, runAnd, wrap}
import com.lightning.walletapp.ln.wire.{ChannelReestablish, LightningMessage, OpenChannel}

import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import android.support.v4.app.LoaderManager.LoaderCallbacks
import com.lightning.walletapp.lnutils.IconGetter.isTablet
import org.bitcoinj.wallet.SendRequest.childPaysForParent
import android.transition.TransitionManager
import android.support.v4.content.Loader
import fr.acinq.bitcoin.Crypto.PublicKey
import android.support.v7.widget.Toolbar
import org.bitcoinj.script.ScriptPattern
import android.support.v4.app.Fragment
import android.app.AlertDialog
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
  import host.{UITask, onButtonTap, showForm, negBuilder, baseBuilder, negTextBuilder, str2View, onTap, onFail}
  import host.{TxProcessor, mkCheckForm, rm, <, mkCheckFormNeutral}

  val fiatRate = frag.findViewById(R.id.fiatRate).asInstanceOf[TextView]
  val fiatBalance = frag.findViewById(R.id.fiatBalance).asInstanceOf[TextView]
  val fiatDetails = frag.findViewById(R.id.fiatDetails).asInstanceOf[LinearLayout]

  val mainWrap = frag.findViewById(R.id.mainWrap).asInstanceOf[LinearLayout]
  val customTitle = frag.findViewById(R.id.customTitle).asInstanceOf[TextView]
  val mnemonicWarn = frag.findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
  val itemsList = frag.findViewById(R.id.itemsList).asInstanceOf[ListView]

  val allTxsWrapper = host.getLayoutInflater.inflate(R.layout.frag_toggler, null)
  val toggler = allTxsWrapper.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  val imageMap = Array(await, await, conf1, dead, frozen)
  val oneBtc = MilliSatoshi(100000000000L)
  val updTitleTask = UITask(updTitle)

  val paymentStates = app.getResources getStringArray R.array.ln_payment_states
  val expiryLeft = app.getResources getStringArray R.array.ln_status_expiry
  val syncOps = app.getResources getStringArray R.array.info_progress
  val txsConfs = app.getResources getStringArray R.array.txs_confs
  val lnTitleOutNoFee = app getString ln_outgoing_title_no_fee
  val statusOperational = app getString btc_status_operational
  val statusConnecting = app getString btc_status_connecting
  val lnTitleOut = app getString ln_outgoing_title
  val lnTitleIn = app getString ln_incoming_title
  val btcEmpty = app getString btc_empty
  val lnEmpty = app getString ln_empty
  val lnProof = app getString ln_proof

  // LISTENERS

  val blocksTitleListener = new BlocksListener {
    def onBlocksDownloaded(peer: Peer, block: Block, fb: FilteredBlock, left: Int) =
      if (left % broadcaster.blocksPerDay == 0) updTitleTask.run
  }

  val peersListener = new PeerConnectedEventListener with PeerDisconnectedEventListener {
    def onPeerDisconnected(connectedPeer: Peer, currentPeerCount: Int) = updTitleTask.run
    def onPeerConnected(connectedPeer: Peer, currentPeerCount: Int) = updTitleTask.run
  }

  val txsListener = new TxTracker {
    // isGreaterThan check because as of now both listeners are fired on incoming and outgoing txs
    def onCoinsSent(w: Wallet, txj: Transaction, a: Coin, b: Coin) = if (a isGreaterThan b) updBtcItems
    def onCoinsReceived(w: Wallet, txj: Transaction, a: Coin, b: Coin) = if (b isGreaterThan a) updBtcItems
    override def txConfirmed(txj: Transaction) = UITask(adapter.notifyDataSetChanged).run
  }

  // To fight spamming
  private[this] var errorLimit = 5
  private[this] var lastOpenMsecStamp = 0L
  private[this] val connectionListener = new ConnectionListener {
    override def onMessage(nodeId: PublicKey, incomingMessage: LightningMessage) = incomingMessage match {
      case open: OpenChannel if System.currentTimeMillis > lastOpenMsecStamp + 10000L && open.channelFlags == 0.toByte =>
        // We only consider incoming channels if they are private and last message arrived more than 10 seconds ago

        lastOpenMsecStamp = System.currentTimeMillis
        ConnectionManager.connections get nodeId foreach { worker =>
          val incomingChannel = app getString ln_ops_start_fund_incoming_channel
          val hardcodedNodeView = HardcodedNodeView(worker.ann, incomingChannel)
          app.TransData.value = IncomingChannelParams(hardcodedNodeView, open)
          host goTo classOf[LNStartFundActivity]
        }

      case _ =>
    }
  }

  val chanListener = new ChannelListener {
    def informOfferClose(chan: Channel, message: String) = UITask {
      val bld = baseBuilder(chan.data.announce.asString.html, message)
      def close(alert: AlertDialog) = rm(alert)(chan process ChannelManager.CMDLocalShutdown)
      mkCheckFormNeutral(_.dismiss, none, close, bld, dialog_ok, -1, ln_chan_close)
    }

    override def settled(cs: Commitments) =
      if (cs.localCommit.spec.fulfilled.nonEmpty)
        host stopService host.awaitServiceIntent

    override def onProcessSuccess = {
      case (chan, _: HasCommitments, remoteError: wire.Error) if errorLimit > 0 =>
        // Remote peer has sent us an error, display details to user and offer force-close
        informOfferClose(chan, remoteError.exception.getMessage).run
        errorLimit -= 1

      case (chan, _: NormalData, cr: ChannelReestablish) if cr.myCurrentPerCommitmentPoint.isEmpty =>
        // Remote peer was OK but now has incompatible features, display details to user and offer force-close
        val msg = host getString err_ln_peer_incompatible format chan.data.announce.workingAddress.toString
        informOfferClose(chan, msg).run
    }

    override def onException = {
      case _ \ CMDAddImpossible(rd, code) =>
        // Remove this payment from unsent since it was not accepted by channel
        UITask(host showForm negTextBuilder(dialog_ok, app getString code).create).run
        PaymentInfoWrap failOnUI rd

      case chan \ HTLCHasExpired(_, htlc) =>
        val paymentHash = htlc.add.paymentHash.toString
        val bld = negTextBuilder(dialog_ok, app.getString(err_ln_expired).format(paymentHash).html)
        UITask(host showForm bld.setCustomTitle(chan.data.announce.asString.html).create).run

      case _ \ internal =>
        val stackTrace = UncaughtHandler toText internal
        val bld = negTextBuilder(dialog_ok, stackTrace)
        UITask(host showForm bld.create).run
    }
  }

  val loaderCallbacks: LoaderCallbacks[Cursor] = new LoaderCallbacks[Cursor] {
    def onCreateLoader(id: Int, bn: Bundle) = new ReactLoader[PaymentInfo](host) {
      val consume = (vec: InfoVec) => runAnd(lnItems = vec map LNWrap)(updPaymentList.run)
      def getCursor = if (lastQuery.isEmpty) bag.byRecent else bag byQuery lastQuery
      def createItem(rc: RichCursor) = bag toPaymentInfo rc
    }

    type LoaderCursor = Loader[Cursor]
    type InfoVec = Vector[PaymentInfo]
    def onLoaderReset(loaderCursor: LoaderCursor) = none
    def onLoadFinished(loaderCursor: LoaderCursor, c: Cursor) = none
  }

  toggler setOnClickListener onButtonTap {
    val newImg = if (currentCut > minLinesNum) ic_explode_24dp else ic_implode_24dp
    currentCut = if (currentCut > minLinesNum) minLinesNum else allItems.size
    toggler setImageResource newImg
    adapter.notifyDataSetChanged
  }

  // UPDATING TITLE

  def updTitle = {
    val lnTotal = ChannelManager.notClosingOrRefunding map { chan =>
      // Here we seek a total refundable amount which is not affected by on-chain fee changes
      chan.hasCsOr(data => Commitments.latestRemoteCommit(data.commitments).spec.toRemoteMsat, 0L)
    }

    val lnTotalSum = MilliSatoshi(lnTotal.sum)
    val btcTotalSum = coin2MSat(app.kit.conf0Balance)
    val lnFunds = if (lnTotalSum.amount < 1) lnEmpty else denom parsedWithSign lnTotalSum
    val btcFunds = if (btcTotalSum.isZero) btcEmpty else denom parsedWithSign btcTotalSum
    val perBtcRate = formatFiat format msatInFiat(oneBtc).getOrElse(0L)
    fiatBalance setText msatInFiatHuman(lnTotalSum + btcTotalSum)
    fiatRate setText s"<small>$perBtcRate</small>".html

    val subtitleText =
      if (app.kit.peerGroup.numConnectedPeers < 1) statusConnecting
      else if (ChannelManager.currentBlocksLeft < broadcaster.blocksPerDay) statusOperational
      else app.plurOrZero(syncOps, ChannelManager.currentBlocksLeft / broadcaster.blocksPerDay)

    customTitle setText s"""
      <img src="btc"/><strong>$btcFunds</strong><br>
      <img src="ln"/><strong>$lnFunds</strong><br>
      $subtitleText<img src="none"/>""".html
  }

  // DISPLAYING ITEMS LIST

  val minLinesNum = 5
  var currentCut = minLinesNum
  var lnItems = Vector.empty[LNWrap]
  var btcItems = Vector.empty[BTCWrap]
  var allItems = Vector.empty[ItemWrap]
  var fundTxIds = Set.empty[String]

  def updPaymentList = UITask {
    TransitionManager beginDelayedTransition mainWrap
    val delayedWraps = ChannelManager.delayedPublishes map ShowDelayedWrap
    val tempItems = if (isSearching) lnItems else delayedWraps ++ btcItems ++ lnItems
    allItems = tempItems.sortBy(_.getDate)(Ordering[java.util.Date].reverse) take 48
    fundTxIds = ChannelManager.all.map(_.fundTxId.toString).toSet
    adapter.notifyDataSetChanged
    updTitle

    allTxsWrapper setVisibility viewMap(allItems.size > minLinesNum)
    mnemonicWarn setVisibility viewMap(allItems.isEmpty)
    itemsList setVisibility viewMap(allItems.nonEmpty)
    fiatDetails setVisibility viewMap(!isSearching)
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
    val getDate = new java.util.Date(System.currentTimeMillis + stat.delay)
    def humanSum = denom.coloredIn(stat.amount, new String)
    val id = stat.commitTx.txid.toString

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
      holder.transactWhat setText id
    }

    def generatePopup = {
      val inFiat = msatInFiatHuman(stat.amount)
      val base = app.getString(btc_pending_title)
      val paidFeePercent = stat.fee.amount / (stat.amount.amount / 100D)
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val viewTxOutside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]
      val viewShareBody = detailsWrapper.findViewById(R.id.viewShareBody).asInstanceOf[Button]
      val title = base.format(humanWhen, humanSum, inFiat, denom.coloredOut(stat.fee, denom.sign), paidFeePercent)
      showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

      viewTxOutside setOnClickListener onButtonTap {
        val uri = Uri parse s"https://smartbit.com.au/tx/$id"
        host startActivity new Intent(Intent.ACTION_VIEW, uri)
      }

      viewShareBody setOnClickListener onButtonTap {
        val rawTx = fr.acinq.bitcoin.Transaction write stat.txn
        host share rawTx.toString
      }
    }
  }

  case class LNWrap(info: PaymentInfo) extends ItemWrap {
    val getDate = new java.util.Date(info.stamp)

    def fillView(holder: ViewHolder) = {
      val humanSum = info.incoming == 1 match {
        case true => denom.coloredIn(info.firstSum, new String)
        case false => denom.coloredOut(info.firstSum, new String)
      }

      holder.transactCircle setImageResource imageMap(info.status)
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactWhat setVisibility viewMap(isTablet || isSearching)
      holder.transactWhat setText getDescription(info.description).html
      holder.transactSum setText s"<img src='ln'/>$humanSum".html
    }

    def generatePopup = {
      val inFiat = msatInFiatHuman(info.firstSum)
      val retry = if (info.pr.isFresh) dialog_retry else -1
      val rd = emptyRD(info.pr, info.firstMsat, useCache = false)
      val humanStatus = s"<strong>${paymentStates apply info.status}</strong>"
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
          // Signed payment request along with a preimage is a proof
          host share lnProof.format(serializedPR, info.preimage.toString)
        }
      }

      for {
        responses <- PaymentInfo.errors.get(rd.pr.paymentHash)
        history = responses.reverse.map(_.toString) mkString "\n==\n"
        rd1 <- PaymentInfoWrap.inFlightPayments.get(rd.pr.paymentHash)
        routingPath = for (usedPaymentHop <- rd1.usedRoute) yield usedPaymentHop.humanDetails
        receiver = s"Payee: ${rd1.pr.nodeId.toString}, Expiry: ${rd1.pr.adjustedMinFinalCltvExpiry} blocks"
        fullDebugData = ("Your wallet" +: routingPath :+ receiver mkString "\n-->\n") + s"\n\n$history"
        _ = paymentDebug setOnClickListener onButtonTap(host share fullDebugData)
      } paymentDebug setVisibility View.VISIBLE

      def outgoingTitle = {
        val fee = MilliSatoshi(info.lastMsat - info.firstMsat)
        val paidFeePercent = fee.amount / (info.firstMsat / 100D)
        val amountSentHuman = denom.coloredOut(info.firstSum, denom.sign)
        val feeHuman = denom.coloredOut(fee, denom.sign)

        val title = lnTitleOut.format(humanStatus, amountSentHuman, inFiat, feeHuman, paidFeePercent)
        val expiryBlocksLeftPart = app.plurOrZero(expiryLeft, info.lastExpiry - broadcaster.currentHeight)
        if (info.status == WAITING) s"$expiryBlocksLeftPart<br>$title" else title
      }

      info.incoming -> rd.pr.fallbackAddress -> rd.pr.amount match {
        case 0 \ Some(adr) \ Some(amount) if info.lastMsat == 0 && info.lastExpiry == 0 && info.status == FAILURE =>
          // Payment was failed without even trying because wallet is offline or no suitable payment routes were found on Olympus server
          val bld = baseBuilder(lnTitleOutNoFee.format(humanStatus, denom.coloredOut(info.firstSum, denom.sign), inFiat).html, detailsWrapper)
          mkCheckFormNeutral(_.dismiss, none, onChain(adr, amount, rd.pr.paymentHash), bld, dialog_ok, -1, dialog_pay_onchain)

        case 0 \ _ \ _ if info.lastMsat == 0 && info.lastExpiry == 0 =>
          // Payment has not been tried yet because an wallet is offline
          val amountSentHuman = denom.coloredOut(info.firstSum, denom.sign)
          val title = lnTitleOutNoFee.format(humanStatus, amountSentHuman, inFiat)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

        case 0 \ Some(adr) \ Some(amount) =>
          // Offer a fallback on-chain address along with off-chain retry if payment was not successfull
          if (info.status != FAILURE) showForm(negBuilder(dialog_ok, outgoingTitle.html, detailsWrapper).create)
          else mkCheckFormNeutral(_.dismiss, doSendOffChain(rd), onChain(adr, amount, rd.pr.paymentHash),
            baseBuilder(outgoingTitle.html, detailsWrapper), dialog_ok, retry, dialog_pay_onchain)

        case 0 \ _ \ _ =>
          // Allow off-chain retry only, no on-chain fallback
          if (info.status != FAILURE) showForm(negBuilder(dialog_ok, outgoingTitle.html, detailsWrapper).create)
          else mkCheckForm(_.dismiss, doSendOffChain(rd), baseBuilder(outgoingTitle.html, detailsWrapper), dialog_ok, retry)

        case 1 \ _ \ _ =>
          val amountReceivedHuman = denom.coloredIn(info.firstSum, denom.sign)
          val title = lnTitleIn.format(humanStatus, amountReceivedHuman, inFiat)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)
      }
    }
  }

  case class BTCWrap(wrap: TxWrap) extends ItemWrap {
    private[this] def txDepth = wrap.tx.getConfidence.getDepthInBlocks
    private[this] def txDead = DEAD == wrap.tx.getConfidence.getConfidenceType
    private[this] val id = wrap.tx.getHashAsString
    val getDate = wrap.tx.getUpdateTime

    def fillView(holder: ViewHolder) = {
      val humanSum = wrap.visibleValue.isPositive match {
        case _ if fundTxIds contains id => denom.coloredP2WSH(-wrap.visibleValue, new String)
        case false => denom.coloredOut(-wrap.visibleValue, new String)
        case true => denom.coloredIn(wrap.visibleValue, new String)
      }

      val status = if (txDead) dead else if (txDepth >= minDepth) conf1 else await
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactSum setText s"<img src='btc'/>$humanSum".html
      holder.transactWhat setVisibility viewMap(isTablet)
      holder.transactCircle setImageResource status
      holder.transactWhat setText id
    }

    def generatePopup = {
      val confs = app.plurOrZero(txsConfs, txDepth)
      val detailsWrapper = host.getLayoutInflater.inflate(R.layout.frag_tx_btc_details, null)
      val viewTxOutside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]
      val viewShareBody = detailsWrapper.findViewById(R.id.viewShareBody).asInstanceOf[Button]

      val humanValues = wrap.directedScriptPubKeysWithValueTry(wrap.visibleValue.isPositive) collect {
        case Success(channelFunding \ value) if channelFunding.isSentToP2WSH => P2WSHData(value, channelFunding)
        case Success(pks \ value) if !ScriptPattern.isOpReturn(pks) => AddrData(value, pks getToAddress app.params)
      } collect {
        case contract: P2WSHData => contract destination denom.coloredP2WSH(contract.cn, denom.sign)
        case incoming: AddrData if wrap.visibleValue.isPositive => incoming destination denom.coloredIn(incoming.cn, denom.sign)
        case outgoingPayment: AddrData => outgoingPayment destination denom.coloredOut(outgoingPayment.cn, denom.sign)
      }

      viewTxOutside setOnClickListener onButtonTap {
        val uri = Uri parse s"https://smartbit.com.au/tx/$id"
        host startActivity new Intent(Intent.ACTION_VIEW, uri)
      }

      viewShareBody setOnClickListener onButtonTap { host share BinaryData(wrap.tx.unsafeBitcoinSerialize).toString }
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
    ConnectionManager.listeners -= connectionListener
    for (c <- ChannelManager.all) c.listeners -= chanListener
    app.kit.wallet removeTransactionConfidenceEventListener txsListener
    app.kit.peerGroup removeBlocksDownloadedEventListener blocksTitleListener
    app.kit.peerGroup removeDisconnectedEventListener peersListener
    app.kit.peerGroup removeConnectedEventListener peersListener
    app.kit.wallet removeCoinsReceivedEventListener txsListener
    app.kit.wallet removeCoinsSentEventListener txsListener
  }

  // LN SEND / RECEIVE

  def receive(chansWithRoutes: Map[Channel, PaymentRoute], maxCanReceive: MilliSatoshi,
              title: View, defDescr: String = new String)(onDone: RoutingData => Unit) = {

    val baseHint = app.getString(amount_hint_can_receive).format(denom parsedWithSign maxCanReceive)
    val content = host.getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
    val inputDescription = content.findViewById(R.id.inputDescription).asInstanceOf[EditText]
    val rateManager = new RateManager(content) hint baseHint

    def makeRequest(sum: MilliSatoshi, preimage: BinaryData) = {
      val pr = PaymentRequest(chainHash, amount = Some(sum), paymentHash = Crypto sha256 preimage,
        nodePrivateKey, inputDescription.getText.toString.trim, Some(app.kit.currentAddress.toString),
        chansWithRoutes.filterKeys(chan => estimateCanReceiveCapped(chan) >= sum.amount).values.toVector)

      val rd = emptyRD(pr, sum.amount, useCache = true)
      db.change(PaymentTable.newVirtualSql, rd.queryText, pr.paymentHash)
      db.change(PaymentTable.newSql, pr.toJson, preimage, 1, HIDDEN,
        System.currentTimeMillis, pr.description, pr.paymentHash,
        sum.amount, 0L, 0L, NOCHANID)

      rd
    }

    def recAttempt(alert: AlertDialog) = rateManager.result match {
      case Success(ms) if maxCanReceive < ms => app toast dialog_sum_big
      case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small
      case Failure(reason) => app toast dialog_sum_small

      case Success(ms) => rm(alert) {
        // Requests without amount are not allowed for now
        <(makeRequest(ms, random getBytes 32), onFail)(onDone)
        app toast dialog_pr_making
      }
    }

    inputDescription setText defDescr
    mkCheckFormNeutral(recAttempt, none, _ => rateManager setSum Try(maxCanReceive),
      baseBuilder(title, content), dialog_ok, dialog_cancel, dialog_max)
  }

  def sendPayment(pr: PaymentRequest) = {
    // At this point we know we have some channels, check if they are good
    val hasOpeningChans = ChannelManager.notClosingOrRefunding.exists(isOpening)
    val maxLocalSend = ChannelManager.all.filter(isOperational).map(estimateCanSend)

    if (!pr.isFresh) app toast dialog_pr_expired
    else if (PaymentRequest.prefixes(chainHash) != pr.prefix) app toast err_general
    else if (hasOpeningChans && maxLocalSend.isEmpty) onFail(app getString err_ln_still_opening)
    else if (maxLocalSend.isEmpty) app toast ln_no_open_chans
    else {

      val description = getDescription(pr.description)
      val maxCappedSend = MilliSatoshi(pr.amount.map(_.amount * 2 min maxHtlcValueMsat) getOrElse maxHtlcValueMsat min maxLocalSend.max)
      val baseContent = host.getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false).asInstanceOf[LinearLayout]
      val baseHint = app.getString(amount_hint_can_send).format(denom parsedWithSign maxCappedSend)
      val baseTitle = str2View(app.getString(ln_send_title).format(description).html)
      val rateManager = new RateManager(baseContent) hint baseHint

      val relayLink = new JointNode(pr.nodeId) {
        override def onDataUpdated = UITask(changeText).run

        def canShowGuaranteedDeliveryHint(relayable: MilliSatoshi) =
          (pr.amount.isEmpty && relayable >= maxCappedSend) || // No sum asked and we can deliver max amount
            pr.amount.exists(asked => relayable >= asked) || // We definitely can deliver an asked amount
            JointNode.hasRelayPeerOnly // We only have a relay node as peer

        def changeText = best match {
          case Some(relayable \ chanBalInfo) if canShowGuaranteedDeliveryHint(relayable) =>
            val finalDeliverable = if (relayable >= maxCappedSend) maxCappedSend else relayable
            host.updateView2Blue(baseTitle, app getString ln_send_title_guaranteed format description)
            rateManager hint app.getString(amount_hint_can_deliver).format(denom parsedWithSign finalDeliverable)

          case _ =>
            // Set a base hint back again
            rateManager hint baseHint
        }
      }

      def sendAttempt(alert: AlertDialog) = rateManager.result -> relayLink.best match {
        case Success(ms) \ Some(relayable \ _) if relayable < ms && JointNode.hasRelayPeerOnly => app toast dialog_sum_big
        case Success(ms) \ _ if minHtlcValue > ms || pr.amount.exists(_ > ms) => app toast dialog_sum_small
        case Success(ms) \ _ if maxCappedSend < ms => app toast dialog_sum_big
        case Failure(emptyAmount) \ _ => app toast dialog_sum_small

        case Success(ms) \ Some(relayable \ chanBalInfo) if ms <= relayable => rm(alert) {
          // We have obtained a (Joint -> payee) route out of band so we can use it right away
          val rd = emptyRD(pr, ms.amount, useCache = true) plusOutOfBandRoute Vector(chanBalInfo.hop)
          me doSendOffChain rd
        }

        case Success(ms) \ _ => rm(alert) {
          // A usual send without out-of-band paths added
          val rd = emptyRD(pr, ms.amount, useCache = true)
          me doSendOffChain rd
        }
      }

      pr.fallbackAddress -> pr.amount match {
        case Some(adr) \ Some(amount) if amount > maxCappedSend && amount < app.kit.conf0Balance =>
          // We have channels but can't fulfill this off-chain, yet have enough funds in our on-chain wallet
          val notEnoughFundsMessage = app getString err_ln_not_enough format denom.coloredP2WSH(amount, denom.sign)
          mkCheckFormNeutral(none, none, onChain(adr, amount, pr.paymentHash), baseBuilder(baseTitle, notEnoughFundsMessage.html),
            dialog_ok, -1, dialog_pay_onchain)

        case _ \ Some(amount) if amount > maxCappedSend =>
          // Either request contains no fallback address or we don't have enough funds on-chain
          val msg = app getString err_ln_not_enough format denom.coloredP2WSH(amount, denom.sign)
          showForm(negBuilder(dialog_ok, baseTitle, msg.html).create)

        case _ =>
          // We can afford to pay this off-chain
          val bld = baseBuilder(baseTitle, baseContent)
          val killOpt = for (jointChannel <- JointNode.relayPeerChans.headOption) yield relayLink start jointChannel.data.announce
          bld setOnDismissListener new DialogInterface.OnDismissListener { def onDismiss(dlg: DialogInterface) = for (off <- killOpt) off.run }
          for (amountrequestedByPayee <- pr.amount) rateManager setSum Try(amountrequestedByPayee)
          mkCheckForm(sendAttempt, none, bld, dialog_pay, dialog_cancel)
      }
    }
  }

  def doSendOffChain(rd: RoutingData) = {
    // Inform if all local channels are offline and some waiting is about to happen
    val atLeastOneIsOnline = ChannelManager.notClosingOrRefunding.exists(_.state == OPEN)
    if (!atLeastOneIsOnline) app toast ln_chan_offline

    ChannelManager.checkIfSendable(rd) match {
      case Left(sanityCheckErr) => onFail(sanityCheckErr)
      case _ => PaymentInfoWrap addPendingPayment rd
    }
  }

  // BTC SEND / BOOST

  def onChain(adr: String, amount: MilliSatoshi, hash: BinaryData)(alert: AlertDialog) = rm(alert) {
    // Hide off-chain payment once on-chain is sent so user does not see the same on/off-chain records
    // this code only gets executed when user taps a button to pay on-chain
    val fallback = Address.fromString(app.params, adr)

    sendBtcPopup(fallback) { _ =>
      PaymentInfoWrap.updStatus(HIDDEN, hash)
      PaymentInfoWrap.uiNotify
    } setSum Try(amount)
  }

  def sendBtcPopup(addr: Address)(and: Transaction => Unit): RateManager = {
    val baseHint = app.getString(amount_hint_can_send).format(denom parsedWithSign app.kit.conf0Balance)
    val form = host.getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val addressData = form.findViewById(R.id.addressData).asInstanceOf[TextView]
    val rateManager = new RateManager(form) hint baseHint

    def sendAttempt(alert: AlertDialog): Unit = rateManager.result match {
      case Success(ms) if MIN isGreaterThan ms => app toast dialog_sum_small
      case Failure(probablyEmptySum) => app toast dialog_sum_small

      case Success(ms) =>
        val txProcessor = new TxProcessor {
          def futureProcess(unsignedReq: SendRequest) = and(app.kit blockSend app.kit.sign(unsignedReq).tx)
          def onTxFail(err: Throwable) = showForm(negBuilder(dialog_ok, txMakeError(err), null).create)
          val pay = AddrData(ms, addr)
        }

        val coloredAmount = denom.coloredOut(txProcessor.pay.cn, denom.sign)
        val coloredExplanation = txProcessor.pay destination coloredAmount
        rm(alert)(txProcessor start coloredExplanation)
    }

    val bld = baseBuilder(app.getString(btc_send_title).html, form)
    mkCheckForm(sendAttempt, none, bld, dialog_next, dialog_cancel)
    addressData setText humanSix(addr.toString)
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

  host.timer.schedule(adapter.notifyDataSetChanged, 10000, 10000)
  host setSupportActionBar frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  host.getContentResolver.registerContentObserver(db sqlPath PaymentTable.table, true, observer)
  itemsList setOnItemClickListener onTap { position => adapter.getItem(position).generatePopup }
  itemsList setFooterDividersEnabled false
  itemsList addFooterView allTxsWrapper
  itemsList setAdapter adapter

  ConnectionManager.listeners += connectionListener
  for (c <- ChannelManager.all) c.listeners += chanListener
  Utils clickableTextField frag.findViewById(R.id.mnemonicInfo)
  app.kit.wallet addTransactionConfidenceEventListener txsListener
  app.kit.peerGroup addBlocksDownloadedEventListener blocksTitleListener
  app.kit.peerGroup addDisconnectedEventListener peersListener
  app.kit.peerGroup addConnectedEventListener peersListener
  app.kit.wallet addCoinsReceivedEventListener txsListener
  app.kit.wallet addCoinsSentEventListener txsListener
  runAnd(react)(updBtcItems)
}