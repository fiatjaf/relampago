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
import android.support.v4.content.{ContextCompat, Loader}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi}
import org.bitcoinj.core.Transaction.{MIN_NONDUST_OUTPUT => MIN}
import com.lightning.walletapp.ln.Tools.{none, random, runAnd, wrap}
import com.lightning.walletapp.helper.{AwaitService, ReactLoader, RichCursor}
import com.lightning.walletapp.ln.wire.{ChannelReestablish, LightningMessage, OpenChannel}

import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import org.bitcoinj.core.listeners.PeerDisconnectedEventListener
import org.bitcoinj.core.listeners.PeerConnectedEventListener
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import android.support.v4.app.LoaderManager.LoaderCallbacks
import com.lightning.walletapp.lnutils.IconGetter.isTablet
import org.bitcoinj.wallet.SendRequest.childPaysForParent
import android.transition.TransitionManager
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
  val toolbar = frag.findViewById(R.id.toolbar).asInstanceOf[Toolbar]

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
          val hnv = HardcodedNodeView(worker.ann, StartNodeView.incomingChannel)
          app.TransData.value = IncomingChannelParams(hnv, open)
          host goTo classOf[LNStartFundActivity]
        }

      case _ =>
    }
  }

  val chanListener = new ChannelListener {
    def informOfferClose(chan: Channel, message: String) = UITask {
      val bld = baseBuilder(chan.data.announce.toString.html, message)
      def close(alert: AlertDialog) = rm(alert)(chan process ChannelManager.CMDLocalShutdown)
      mkCheckFormNeutral(alert => rm(alert)(none), none, close, bld, dialog_ok, -1, ln_chan_close)
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
        PaymentInfoWrap.unsentPayments -= rd.pr.paymentHash
        PaymentInfoWrap failOnUI rd

      case chan \ HTLCHasExpired(_, htlc) =>
        val paymentHash = htlc.add.paymentHash.toString
        val bld = negTextBuilder(dialog_ok, app.getString(err_ln_expired).format(paymentHash).html)
        UITask(host showForm bld.setCustomTitle(chan.data.announce.toString.html).create).run

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
    val lnTotal = for {
      chan <- ChannelManager.notClosingOrRefunding
      commit <- chan(Commitments.latestRemoteCommit)
    } yield commit.spec.toRemoteMsat

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

  def updPaymentList = UITask {
    TransitionManager beginDelayedTransition mainWrap
    val delayedWraps = ChannelManager.delayedPublishes map ShowDelayedWrap
    val tempItems = if (isSearching) lnItems else delayedWraps ++ btcItems ++ lnItems
    allItems = tempItems.sortBy(_.getDate)(Ordering[java.util.Date].reverse) take 48
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
      holder.transactWhat setText btc_refunding
      holder.transactWhen setText humanWhen
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
        val parentCommitTxid = stat.commitTx.txid.toString
        val uri = s"https://smartbit.com.au/tx/$parentCommitTxid"
        host startActivity new Intent(Intent.ACTION_VIEW, Uri parse uri)
      }

      viewShareBody setOnClickListener onButtonTap {
        val rawTx = fr.acinq.bitcoin.Transaction write stat.txn
        host share rawTx.toString
      }
    }
  }

  case class LNWrap(info: PaymentInfo) extends ItemWrap {
    val getDate: java.util.Date = new java.util.Date(info.stamp)

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

      info.incoming -> onChainRunnable(rd.pr) match {
        case 0 \ Some(runnable) if info.lastMsat == 0 && info.lastExpiry == 0 && info.status == FAILURE =>
          // Payment was failed without even trying because wallet is offline or no suitable payment routes were found
          val bld = baseBuilder(lnTitleOutNoFee.format(humanStatus, denom.coloredOut(info.firstSum, denom.sign), inFiat).html, detailsWrapper)
          mkCheckFormNeutral(alert => rm(alert)(none), none, alert => rm(alert)(runnable.run), bld, dialog_ok, -1, dialog_pay_onchain)

        case 0 \ _ if info.lastMsat == 0 && info.lastExpiry == 0 =>
          // Payment has not been tried yet because an wallet is offline
          val amountSentHuman = denom.coloredOut(info.firstSum, denom.sign)
          val title = lnTitleOutNoFee.format(humanStatus, amountSentHuman, inFiat)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)

        case 0 \ Some(runnable) =>
          val bld = baseBuilder(outgoingTitle.html, detailsWrapper)
          def useOnchain(alert: AlertDialog) = rm(alert)(runnable.run)
          // Offer a fallback onchain address if payment was not successfull
          if (info.status != FAILURE) showForm(negBuilder(dialog_ok, outgoingTitle.html, detailsWrapper).create)
          else mkCheckFormNeutral(alert => rm(alert)(none), doSend(rd), useOnchain, bld, dialog_ok, retry, dialog_pay_onchain)

        case 0 \ None =>
          val bld = baseBuilder(outgoingTitle.html, detailsWrapper)
          // Only allow user to retry this payment while using excluded nodes and channels but not an onchain option
          if (info.status != FAILURE) showForm(negBuilder(dialog_ok, outgoingTitle.html, detailsWrapper).create)
          else mkCheckForm(alert => rm(alert)(none), doSend(rd), bld, dialog_ok, retry)

        case 1 \ _ =>
          val amountReceivedHuman = denom.coloredIn(info.firstSum, denom.sign)
          val title = lnTitleIn.format(humanStatus, amountReceivedHuman, inFiat)
          showForm(negBuilder(dialog_ok, title.html, detailsWrapper).create)
      }
    }
  }

  case class BTCWrap(wrap: TxWrap) extends ItemWrap {
    val getDate: java.util.Date = wrap.tx.getUpdateTime
    private[this] def txDepth = wrap.tx.getConfidence.getDepthInBlocks
    private[this] def txDead = DEAD == wrap.tx.getConfidence.getConfidenceType

    def fillView(holder: ViewHolder) = {
      val humanSum = wrap.visibleValue.isPositive match {
        case true => denom.coloredIn(wrap.visibleValue, new String)
        case false => denom.coloredOut(-wrap.visibleValue, new String)
      }

      val status = if (txDead) dead else if (txDepth >= minDepth) conf1 else await
      holder.transactWhen setText when(System.currentTimeMillis, getDate).html
      holder.transactSum setText s"<img src='btc'/>$humanSum".html
      holder.transactWhat setText wrap.tx.getHashAsString
      holder.transactWhat setVisibility viewMap(isTablet)
      holder.transactCircle setImageResource status
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
        val uri = s"https://smartbit.com.au/tx/" + wrap.tx.getHashAsString
        host startActivity new Intent(Intent.ACTION_VIEW, Uri parse uri)
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
      else mkCheckForm(alert => rm(alert)(none), boostIncoming(wrap), baseBuilder(header.html, lst), dialog_ok, dialog_boost)
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

  def receive(chansWithRoutes: Map[Channel, PaymentRoute], maxCanReceive: MilliSatoshi) = {
    val baseHint = app.getString(amount_hint_can_receive).format(denom parsedWithSign maxCanReceive)
    val content = host.getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
    val rateManager = new RateManager(content) hint baseHint

    def makeRequest(sum: MilliSatoshi, preimg: BinaryData) = {
      val info = content.findViewById(R.id.inputDescription).asInstanceOf[EditText].getText.toString.trim
      val routes = chansWithRoutes.filterKeys(channel => estimateCanReceiveCapped(channel) >= sum.amount).values.toVector
      val pr = PaymentRequest(chainHash, Some(sum), Crypto sha256 preimg, nodePrivateKey, info, Some(app.kit.currentAddress.toString), routes)
      val rd = emptyRD(pr, sum.amount, useCache = true)

      db.change(PaymentTable.newVirtualSql, params = rd.queryText, rd.pr.paymentHash)
      db.change(PaymentTable.newSql, pr.toJson, preimg, 1, HIDDEN, System.currentTimeMillis,
        pr.description, rd.pr.paymentHash, sum.amount, 0L, 0L, NOCHANID)

      app.TransData.value = pr
      host goTo classOf[RequestActivity]
      host.awaitServiceIntent.setAction(AwaitService.SHOW_AMOUNT)
      host.awaitServiceIntent.putExtra(AwaitService.SHOW_AMOUNT, denom asString sum)
      ContextCompat.startForegroundService(host, host.awaitServiceIntent)
    }

    def recAttempt(alert: AlertDialog) = rateManager.result match {
      case Success(ms) if maxCanReceive < ms => app toast dialog_sum_big
      case Success(ms) if minHtlcValue > ms => app toast dialog_sum_small
      case Failure(reason) => app toast dialog_sum_small

      case Success(ms) => rm(alert) {
        // Requests without amount are not allowed for now
        <(makeRequest(ms, random getBytes 32), onFail)(none)
        app toast dialog_pr_making
      }
    }

    def useMax(alert: AlertDialog) = rateManager setSum Try(maxCanReceive)
    val bld = baseBuilder(title = app.getString(ln_receive_title).html, content)
    mkCheckFormNeutral(recAttempt, none, useMax, bld, dialog_ok, dialog_cancel, dialog_max)
  }

  def sendPayment(pr: PaymentRequest) =
    if (PaymentRequest.prefixes(chainHash) != pr.prefix) app toast err_general
    else if (!pr.isFresh) app toast dialog_pr_expired
    else {

      // This fetches normal channels which MAY be offline currently
      val openingChannels = ChannelManager.notClosingOrRefunding.filter(isOpening)
      val operationalChannels = ChannelManager.notClosingOrRefunding.filter(isOperational)
      if (operationalChannels.isEmpty && openingChannels.nonEmpty) onFail(app getString err_ln_still_opening)
      else if (operationalChannels.isEmpty) app toast ln_no_open_chans
      else {

        val runnableOpt = onChainRunnable(pr)
        val description = getDescription(pr.description)
        val maxLocalSend = operationalChannels.map(estimateCanSend).max
        val maxCappedSend = MilliSatoshi(pr.amount.map(_.amount * 2 min maxHtlcValueMsat) getOrElse maxHtlcValueMsat min maxLocalSend)
        val baseContent = host.getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false).asInstanceOf[LinearLayout]
        val baseHint = app.getString(amount_hint_can_send).format(denom parsedWithSign maxCappedSend)
        val baseTitle = str2View(app.getString(ln_send_title).format(description).html)
        val rateManager = new RateManager(baseContent) hint baseHint
        val bld = baseBuilder(baseTitle, baseContent)

        val relayLink = new JointNode(pr.nodeId) {
          override def onDataUpdated = UITask(changeText).run
          def canShowGuaranteedDeliveryHint(relayable: MilliSatoshi) =
            (pr.amount.isEmpty && relayable >= maxCappedSend) || // No sum asked and we can deliver max amount
              pr.amount.exists(asked => relayable >= asked) || // We definitely can deliver an asked amount
              JointNode.hasRelayPeerOnly // We only have a relay node as peer

          def changeText = best match {
            case Some(relayable \ chanBalInfo) if canShowGuaranteedDeliveryHint(relayable) =>
              val finalDeliverable = if (relayable >= maxCappedSend) maxCappedSend else relayable
              val guaranteedDeliveryTitle = app.getString(ln_send_title_guaranteed).format(description).html
              rateManager hint app.getString(amount_hint_can_deliver).format(denom parsedWithSign finalDeliverable)
              baseTitle.findViewById(R.id.titleTip).asInstanceOf[TextView] setText guaranteedDeliveryTitle
              baseTitle setBackgroundColor ContextCompat.getColor(host, R.color.ln)

            case _ =>
              // Set a base hint back again
              rateManager hint baseHint
          }
        }

        def sendAttempt(alert: AlertDialog) = (rateManager.result, relayLink.best) match {
          case Success(ms) \ Some(relayable \ _) if relayable < ms && JointNode.hasRelayPeerOnly => app toast dialog_sum_big
          case Success(ms) \ _ if minHtlcValue > ms || pr.amount.exists(_ > ms) => app toast dialog_sum_small
          case Success(ms) \ _ if maxCappedSend < ms => app toast dialog_sum_big
          case Failure(emptyAmount) \ _ => app toast dialog_sum_small

          case Success(ms) \ Some(relayable \ chanBalInfo) if ms <= relayable => rm(alert) {
            // We have obtained a (Joint -> payee) route out of band so we can use it right away
            val rd = emptyRD(pr, ms.amount, useCache = true) plusOutOfBandRoute Vector(chanBalInfo.hop)
            me doSend rd
          }

          case Success(ms) \ _ => rm(alert) {
            me doSend emptyRD(pr, ms.amount, useCache = true)
            // Inform if channels are offline and some wait will happen
            val isOnline = operationalChannels.exists(_.state == OPEN)
            if (!isOnline) app toast ln_chan_offline
          }
        }

        for (askedSum <- pr.amount) rateManager setSum Try(askedSum)
        val killOpt = for (rep <- JointNode.relayPeerReports.headOption) yield relayLink start rep.chan.data.announce
        bld setOnDismissListener new DialogInterface.OnDismissListener { def onDismiss(dialog: DialogInterface) = for (off <- killOpt) off.run }
        mkCheckFormNeutral(sendAttempt, none, alert => rm(alert) { for (onChain <- runnableOpt) onChain.run }, bld, dialog_pay, dialog_cancel,
          if (pr.amount.exists(askedSum => maxCappedSend >= askedSum) || runnableOpt.isEmpty) -1 else dialog_pay_onchain)
      }
    }

  def doSend(rd: RoutingData) =
    ChannelManager.checkIfSendable(rd) match {
      case Left(sanityCheckErr) => onFail(sanityCheckErr)
      case _ => PaymentInfoWrap addPendingPayment rd
    }

  // BTC SEND / BOOST

  def onChainRunnable(pr: PaymentRequest) =
    for (adr <- pr.fallbackAddress) yield UITask {
      // This code does not get executed right away
      // but only when user taps a button to pay on-chain
      val fallback = Address.fromString(app.params, adr)
      val tryMSat = Try(pr.amount.get)

      sendBtcPopup(fallback) { txj =>
        // Hide off-chain payment once on-chain is sent
        // so user does not see the same on/off-chain record
        PaymentInfoWrap.updStatus(HIDDEN, pr.paymentHash)
        PaymentInfoWrap.uiNotify
      } setSum tryMSat
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
          def futureProcess(unsignedSendRequest: SendRequest) = and(app.kit blockSend app.kit.sign(unsignedSendRequest).tx)
          def onTxFail(err: Throwable) = mkCheckForm(alert => rm(alert)(retry), none, baseBuilder(txMakeError(err), null), dialog_retry, dialog_cancel)
          def retry = sendBtcPopup(addr)(and)
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
    mkCheckForm(alert => rm(alert)(none), <(replace, onError)(none), userWarn, dialog_cancel, dialog_boost)

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
  host.timer.schedule(adapter.notifyDataSetChanged, 10000, 10000)
  host setSupportActionBar toolbar

  toolbar setOnClickListener onButtonTap {
    // View current balance status and guranteed deliveries
    if (!isSearching) host goTo classOf[WalletStatusActivity]
  }

  itemsList setOnItemClickListener onTap {
    pos => adapter.getItem(pos).generatePopup
  }

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