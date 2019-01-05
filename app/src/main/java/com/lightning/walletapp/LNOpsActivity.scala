package com.lightning.walletapp

import spray.json._
import android.widget._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import fr.acinq.bitcoin.{BinaryData, Satoshi}
import org.bitcoinj.core.{Block, FilteredBlock, Peer}
import android.view.{Menu, MenuItem, View, ViewGroup}
import com.lightning.walletapp.ln.Tools.{none, wrap, runAnd}
import com.lightning.walletapp.ln.{Channel, ChannelData, RefundingData}
import com.lightning.walletapp.lnutils.IconGetter.scrWidth
import com.lightning.walletapp.lnutils.PaymentTable
import com.lightning.walletapp.helper.RichCursor
import android.support.v7.widget.Toolbar
import org.bitcoinj.script.ScriptBuilder
import android.content.Intent
import android.os.Bundle
import android.net.Uri
import java.util.Date


class LNOpsActivity extends TimerActivity with HumanTimeDisplay { me =>
  lazy val localChanCache = for (channel <- ChannelManager.all if me canDisplay channel.data) yield channel
  lazy val chanActions = for (txt <- getResources getStringArray R.array.ln_chan_actions_list) yield txt.html
  lazy val presentChans = app.getResources getStringArray R.array.ln_chan_present
  lazy val gridView = findViewById(R.id.gridView).asInstanceOf[GridView]
  lazy val otherState = getString(ln_info_status_other)
  lazy val fundingInfo = getString(ln_info_funding)
  lazy val host = me

  val adapter = new BaseAdapter {
    def getItem(position: Int) = localChanCache(position)
    def getItemId(chanPosition: Int) = chanPosition
    def getCount = localChanCache.size

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val card = if (null == savedView) getLayoutInflater.inflate(R.layout.chan_card, null) else savedView
      val holder = if (null == card.getTag) new ViewHolder(card) else card.getTag.asInstanceOf[ViewHolder]
      holder fillView getItem(position)
      card
    }
  }

  val becomeListener = new ChannelListener {
    override def onBecome: PartialFunction[Transition, Unit] = {
      case anyStateChange => UITask(adapter.notifyDataSetChanged).run
    }
  }

  val blocksListener = new BlocksListener {
    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) =
      if (left < 1) UITask(adapter.notifyDataSetChanged).run
  }

  class ViewHolder(view: View) {
    val extraInfo = view.findViewById(R.id.extraInfo).asInstanceOf[View]
    val baseBar = view.findViewById(R.id.baseBar).asInstanceOf[ProgressBar]
    val overBar = view.findViewById(R.id.overBar).asInstanceOf[ProgressBar]
    val extraInfoText = view.findViewById(R.id.extraInfoText).asInstanceOf[TextView]
    val addressAndKey = view.findViewById(R.id.addressAndKey).asInstanceOf[TextView]
    val stateAndConnectivity = view.findViewById(R.id.stateAndConnectivity).asInstanceOf[TextView]
    def setExtraInfo(text: CharSequence) = wrap(extraInfo setVisibility View.VISIBLE)(extraInfoText setText text)
    def setExtraInfo(resource: Int) = wrap(extraInfo setVisibility View.VISIBLE)(extraInfoText setText resource)

    val wrappers =
      view.findViewById(R.id.refundableAmount).asInstanceOf[View] ::
        view.findViewById(R.id.paymentsInFlight).asInstanceOf[View] ::
        view.findViewById(R.id.paymentsReceived).asInstanceOf[View] ::
        view.findViewById(R.id.totalCapacity).asInstanceOf[View] ::
        view.findViewById(R.id.paymentsSent).asInstanceOf[View] ::
        view.findViewById(R.id.fundingDepth).asInstanceOf[View] ::
        view.findViewById(R.id.canReceive).asInstanceOf[View] ::
        view.findViewById(R.id.startedAt).asInstanceOf[View] ::
        view.findViewById(R.id.refundFee).asInstanceOf[View] ::
        view.findViewById(R.id.closedAt).asInstanceOf[View] ::
        view.findViewById(R.id.canSend).asInstanceOf[View] ::
        baseBar :: overBar :: Nil

    val refundableAmountText = view.findViewById(R.id.refundableAmountText).asInstanceOf[TextView]
    val paymentsInFlightText = view.findViewById(R.id.paymentsInFlightText).asInstanceOf[TextView]
    val paymentsReceivedText = view.findViewById(R.id.paymentsReceivedText).asInstanceOf[TextView]
    val totalCapacityText = view.findViewById(R.id.totalCapacityText).asInstanceOf[TextView]
    val paymentsSentText = view.findViewById(R.id.paymentsSentText).asInstanceOf[TextView]
    val fundingDepthText = view.findViewById(R.id.fundingDepthText).asInstanceOf[TextView]
    val canReceiveText = view.findViewById(R.id.canReceiveText).asInstanceOf[TextView]
    val startedAtText = view.findViewById(R.id.startedAtText).asInstanceOf[TextView]
    val refundFeeText = view.findViewById(R.id.refundFeeText).asInstanceOf[TextView]
    val closedAtText = view.findViewById(R.id.closedAtText).asInstanceOf[TextView]
    val canSendText = view.findViewById(R.id.canSendText).asInstanceOf[TextView]
    baseBar setMax 1000
    overBar setMax 1000
    view setTag this

    def visibleExcept(gone: Int*) =
      for (textWrapper <- wrappers) {
        val isGone = gone contains textWrapper.getId
        textWrapper setVisibility viewMap(!isGone)
      }

    def fillView(chan: Channel) = {
      val state = stateStatusColor(chan)
      val connect = connectivityStatusColor(chan)
      val stateConnect = s"<strong>$state</strong><br>$connect"
      addressAndKey setText chan.data.announce.toString.html
      stateAndConnectivity setText stateConnect.html
      extraInfo setVisibility View.GONE

      chan { cs =>
        val fundTxId = Commitments fundingTxid cs
        val capacity = cs.commitInput.txOut.amount
        val started = me time new Date(cs.startedAt)
        val breakFee = Satoshi(cs.reducedRemoteState.myFeeSat)
        val txDepth \ _ = LNParams.broadcaster.getStatus(fundTxId)
        val canReceiveMsat = estimateCanReceive(chan)
        val canSendMsat = estimateCanSend(chan)

        val threshold = math.max(cs.remoteParams.minimumDepth, LNParams.minDepth)
        val valueInFlight = Satoshi(inFlightHtlcs(chan).map(_.add.amount.amount).sum / 1000L)
        val refundable = Satoshi(Commitments.latestRemoteCommit(cs).spec.toRemoteMsat / 1000L)
        val valueReceived = Satoshi(getStat(cs.channelId, 1) / 1000L)
        val valueSent = Satoshi(getStat(cs.channelId, 0) / 1000L)

        val barCanSend = cs.remoteCommit.spec.toRemoteMsat / capacity.amount
        val barCanReceive = barCanSend + canReceiveMsat / capacity.amount

        // For incoming chans reserveAndFee is reserve only since fee is zero
        val reserveAndFee = breakFee.amount + cs.remoteParams.channelReserveSatoshis
        val barLocalReserve = math.min(barCanSend, reserveAndFee * 1000L / capacity.amount)

        baseBar setProgress barCanSend.toInt
        baseBar setSecondaryProgress barCanReceive.toInt
        overBar setProgress barLocalReserve.toInt

        startedAtText setText started.html
        fundingDepthText setText fundingInfo.format(txDepth, threshold)
        canReceiveText setText denom.parsedWithSign(Satoshi(canReceiveMsat) / 1000L).html
        canSendText setText denom.parsedWithSign(Satoshi(canSendMsat) / 1000L).html
        refundableAmountText setText denom.parsedWithSign(refundable).html
        totalCapacityText setText denom.parsedWithSign(capacity).html

        paymentsInFlightText setText sumOrNothing(valueInFlight).html
        paymentsReceivedText setText sumOrNothing(valueReceived).html
        paymentsSentText setText sumOrNothing(valueSent).html
        refundFeeText setText sumOrNothing(breakFee).html

        chan.data match {
          case _: WaitFundingDoneData =>
            visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
              R.id.closedAt, R.id.paymentsSent, R.id.paymentsReceived, R.id.paymentsInFlight)

          case remoteWait: WaitBroadcastRemoteData =>
            if (remoteWait.fail.isDefined) setExtraInfo(text = remoteWait.fail.get.report.html)
            visibleExcept(R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive, R.id.closedAt,
              R.id.paymentsSent, R.id.paymentsReceived, R.id.paymentsInFlight)

          case norm: NormalData if isOperational(chan) =>
            if (txDepth > 6 && channelAndHop(chan).isEmpty) setExtraInfo(resource = ln_info_no_receive)
            if (norm.unknownSpend.isDefined) setExtraInfo(resource = ln_info_unknown_spend)
            visibleExcept(R.id.fundingDepth, R.id.closedAt)

          case _: NormalData | _: NegotiationsData =>
            setExtraInfo(resource = ln_info_coop_attempt)
            visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend,
              R.id.canReceive, R.id.refundFee, R.id.fundingDepth, R.id.closedAt)

          case cd: ClosingData =>
            setExtraInfo(text = me closedBy cd)
            val closeDate = new Date(cd.closedAt)
            closedAtText setText time(closeDate).html

            visibleExcept(R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
              if (cd.mutualClose.isEmpty) -1 else R.id.refundFee, R.id.fundingDepth,
              R.id.paymentsInFlight)

          case _ =>
            visibleExcept(R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
              R.id.refundFee, R.id.closedAt, R.id.paymentsSent, R.id.fundingDepth,
              R.id.paymentsReceived, R.id.paymentsInFlight)
        }

        // MENU PART

        def warnAndMaybeClose(channelClosureWarning: String) =
          mkCheckForm(alert => rm(alert)(chan process ChannelManager.CMDLocalShutdown),
            none, baseTextBuilder(channelClosureWarning.html), dialog_ok, dialog_cancel)

        view setOnClickListener onButtonTap {
          val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
          val alert = showForm(negBuilder(dialog_cancel, chan.data.announce.toString.html, lst).create)
          lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, me menu chan.data)
          lst setDividerHeight 0
          lst setDivider null

          def proceedCoopCloseOrWarn(informAndClose: => Unit) = rm(alert) {
            val isOperationalOrFunding = isOperational(chan) || isOpening(chan)
            if (isOperationalOrFunding && inFlightHtlcs(chan).isEmpty) informAndClose // Mutual closing is possible
            else if (isOperationalOrFunding) warnAndMaybeClose(me getString ln_chan_close_inflight_details)
            else warnAndMaybeClose(me getString ln_chan_force_details)
          }

          def closeToWallet = {
            // Simple case: send refunding transaction to this wallet
            warnAndMaybeClose(me getString ln_chan_close_confirm_local)
          }

          def closeToAddress = app.getBufferTry map app.toAddress foreach { address =>
            val text = me getString ln_chan_close_confirm_address format humanSix(address.toString)
            val customShutdown = CMDShutdown apply Some(ScriptBuilder.createOutputScript(address).getProgram)
            mkCheckForm(alert => rm(alert)(chan process customShutdown), none, baseTextBuilder(text.html),
              dialog_ok, dialog_cancel)
          }

          def viewClosing = chan.data match {
            case cd: ClosingData => urlIntent(cd.bestClosing.commitTx.txid.toString)
            case _ => Tools log "Attempting to view a closing tx of not yet closed channel"
          }

          lst setOnItemClickListener onTap {
            case 0 => urlIntent(fundTxId.toString)
            case 1 => share(chan.data.asInstanceOf[HasCommitments].toJson.toString)
            case 2 => proceedCoopCloseOrWarn(informAndClose = closeToAddress)
            case 3 => proceedCoopCloseOrWarn(informAndClose = closeToWallet)
            case 4 => viewClosing
          }
        }
      }
    }
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.lnops, menu)
    true
  }

  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionAddNodeId) me share LNParams.nodePublicKey.toString
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.peerGroup removeBlocksDownloadedEventListener blocksListener
    for (chan <- localChanCache) chan.listeners -= becomeListener
  }

  def INIT(s: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_ln_ops
    me initToolbar findViewById(R.id.toolbar).asInstanceOf[Toolbar]
    getSupportActionBar setSubtitle app.plurOrZero(presentChans, localChanCache.size)
    wrap(gridView setAdapter adapter)(getSupportActionBar setTitle action_ln_details)
    app.kit.peerGroup addBlocksDownloadedEventListener blocksListener
    for (chan <- localChanCache) chan.listeners += becomeListener
    gridView setNumColumns math.round(scrWidth / 2.4).toInt
  } else me exitTo classOf[MainActivity]

  // UTILS

  def stateStatusColor(c: Channel) = (c.data, c.state) match {
    case (_: NormalData, OPEN) if isOperational(c) => me getString ln_info_status_open
    case (_: NormalData, _) if !isOperational(c) => me getString ln_info_status_shutdown
    case _ \ WAIT_FUNDING_DONE => me getString ln_info_status_opening
    case _ \ NEGOTIATIONS => me getString ln_info_status_negotiations
    case _ => otherState format c.state
  }

  def connectivityStatusColor(c: Channel) =
    ConnectionManager.connections get c.data.announce.nodeId match {
      case Some(w) if w.socket.isConnected => me getString ln_info_state_online
      case _ => me getString ln_info_state_offline
    }

  def closedBy(cd: ClosingData) =
    if (cd.remoteCommit.nonEmpty) me getString ln_info_close_remote
    else if (cd.nextRemoteCommit.nonEmpty) me getString ln_info_close_remote
    else if (cd.mutualClose.nonEmpty) me getString ln_info_close_coop
    else me getString ln_info_close_local

  def canDisplay(chanData: ChannelData) = chanData match {
    case ref: RefundingData => ref.remoteLatestPoint.isDefined
    case _ => true
  }

  def menu(chanData: ChannelData) = chanData match {
    case _: ClosingData => chanActions.patch(2, Nil, 2)
    case _: RefundingData => chanActions take 2
    case _ => chanActions take 4
  }

  def sumOrNothing(sats: Satoshi) = sats match {
    case Satoshi(0L) => me getString ln_info_nothing
    case _ => denom parsedWithSign sats
  }

  def getStat(chanId: BinaryData, direction: Int) = {
    // Direction = 0 = untgoing = lastMast, = 1 = incoming = firstMsat
    val cursor = LNParams.db.select(PaymentTable.selectStatSql, chanId, direction)
    RichCursor(cursor) headTry { case RichCursor(с1) => с1 getLong direction } getOrElse 0L
  }

  def urlIntent(txid: String) =
    host startActivity new Intent(Intent.ACTION_VIEW,
      Uri parse s"https://smartbit.com.au/tx/$txid")
}