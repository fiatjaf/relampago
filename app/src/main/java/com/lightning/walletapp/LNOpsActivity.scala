package com.lightning.walletapp

import android.widget._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.NormalChannel._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.ln.{ChannelData, RefundingData}
import com.lightning.walletapp.ln.Tools.{none, runAnd, wrap}
import org.bitcoinj.core.{Block, FilteredBlock, Peer}
import android.view.{Menu, MenuItem, View, ViewGroup}

import com.lightning.walletapp.lnutils.IconGetter.scrWidth
import com.lightning.walletapp.lnutils.PaymentTable
import com.lightning.walletapp.helper.RichCursor
import android.support.v7.widget.Toolbar
import fr.acinq.bitcoin.Satoshi
import android.content.Intent
import scodec.bits.ByteVector
import android.os.Bundle
import android.net.Uri
import java.util.Date


class LNOpsActivity extends TimerActivity with HumanTimeDisplay { me =>
  lazy val displayedChans = for (channel <- ChannelManager.all if me canDisplayData channel.data) yield channel
  lazy val normalChanActions = for (txt <- getResources getStringArray R.array.ln_normal_chan_actions) yield txt.html
  lazy val barStatus = app.getResources getStringArray R.array.ln_chan_ops_status
  lazy val gridView = findViewById(R.id.gridView).asInstanceOf[GridView]
  lazy val host = me

  val adapter = new BaseAdapter {
    def getItem(position: Int) = displayedChans(position)
    def getItemId(chanPosition: Int) = chanPosition
    def getCount = displayedChans.size

    def getView(position: Int, savedView: View, parent: ViewGroup) = getItem(position) match { case chan =>
      val card = if (null == savedView) getLayoutInflater.inflate(R.layout.chan_card, null) else savedView

      val cardView = Tuple3(chan, chan.getCommits, card.getTag) match {
        case (chan: NormalChannel, Some(nc: NormalCommits), view: NormalViewHolder) => view.fill(chan, nc)
        case (chan: NormalChannel, Some(nc: NormalCommits), null) => new NormalViewHolder(card).fill(chan, nc)
        case _ => throw new RuntimeException
      }

      // Remember generated view
      // used for performance reasons
      card setTag cardView
      card
    }
  }

  val eventsListener = new ChannelListener with BlocksListener {
    override def onBecome: PartialFunction[Transition, Unit] = { case _ => UITask(adapter.notifyDataSetChanged).run }
    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = if (left < 1) UITask(adapter.notifyDataSetChanged).run
  }

  class NormalViewHolder(view: View) { self =>
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
        view.findViewById(R.id.totalPayments).asInstanceOf[View] ::
        view.findViewById(R.id.totalCapacity).asInstanceOf[View] ::
        view.findViewById(R.id.fundingDepth).asInstanceOf[View] ::
        view.findViewById(R.id.canReceive).asInstanceOf[View] ::
        view.findViewById(R.id.startedAt).asInstanceOf[View] ::
        view.findViewById(R.id.refundFee).asInstanceOf[View] ::
        view.findViewById(R.id.closedAt).asInstanceOf[View] ::
        view.findViewById(R.id.canSend).asInstanceOf[View] ::
        baseBar :: overBar :: Nil

    val totalPaymentsText = view.findViewById(R.id.totalPaymentsText).asInstanceOf[TextView]
    val refundableAmountText = view.findViewById(R.id.refundableAmountText).asInstanceOf[TextView]
    val paymentsInFlightText = view.findViewById(R.id.paymentsInFlightText).asInstanceOf[TextView]
    val totalCapacityText = view.findViewById(R.id.totalCapacityText).asInstanceOf[TextView]
    val fundingDepthText = view.findViewById(R.id.fundingDepthText).asInstanceOf[TextView]
    val canReceiveText = view.findViewById(R.id.canReceiveText).asInstanceOf[TextView]
    val startedAtText = view.findViewById(R.id.startedAtText).asInstanceOf[TextView]
    val refundFeeText = view.findViewById(R.id.refundFeeText).asInstanceOf[TextView]
    val closedAtText = view.findViewById(R.id.closedAtText).asInstanceOf[TextView]
    val canSendText = view.findViewById(R.id.canSendText).asInstanceOf[TextView]
    baseBar setMax 1000
    overBar setMax 1000

    def visibleExcept(gone: Int*) =
      for (textWrapper <- wrappers) {
        val isGone = gone contains textWrapper.getId
        textWrapper setVisibility viewMap(!isGone)
      }

    def fill(chan: NormalChannel, cs: NormalCommits) = {
      val forceCloseFee = Satoshi(cs.reducedRemoteState.myFeeSat)
      val started = me time new Date(cs.startedAt)
      val connect = connectivityStatusColor(chan)
      val currentState = stateStatusColor(chan)

      val capacity = cs.commitInput.txOut.amount
      val canReceiveMsat = chan.estimateCanReceive
      val barCanSend = cs.remoteCommit.spec.toRemoteMsat / capacity.amount
      val barCanReceive = barCanSend + canReceiveMsat / capacity.amount

      // For incoming chans reserveAndFee is reserve only since fee is zero
      val reserveAndFee = forceCloseFee.amount + cs.remoteParams.channelReserveSatoshis
      val barLocalReserve = math.min(barCanSend, reserveAndFee * 1000L / capacity.amount)
      val fundingDepth \ fundingIsDead = LNParams.broadcaster.getStatus(chan.fundTxId)
      val threshold = math.max(cs.remoteParams.minimumDepth, LNParams.minDepth)

      baseBar setProgress barCanSend.toInt
      baseBar setSecondaryProgress barCanReceive.toInt
      overBar setProgress barLocalReserve.toInt

      extraInfo setVisibility View.GONE
      startedAtText setText started.html
      addressAndKey setText chan.data.announce.asString.html
      totalPaymentsText setText getStat(cs.channelId).toString
      stateAndConnectivity setText s"<strong>$currentState</strong><br>$connect".html
      fundingDepthText setText getString(ln_mofn).format(fundingDepth, threshold).html
      // All amounts are in MilliSatoshi, but we convert them to Satoshi and / 1000 to erase trailing msat remainders
      paymentsInFlightText setText sumOrNothing(Satoshi(chan.inFlightHtlcs.toList.map(_.add.amountMsat).sum) / 1000L).html
      refundableAmountText setText sumOrNothing(Satoshi(cs.localSpec.toLocalMsat) / 1000L).html
      canSendText setText denom.parsedWithSign(Satoshi(chan.estimateCanSend) / 1000L).html
      canReceiveText setText denom.parsedWithSign(Satoshi(canReceiveMsat) / 1000L).html
      totalCapacityText setText denom.parsedWithSign(capacity).html
      refundFeeText setText sumOrNothing(forceCloseFee).html

      chan.data match {
        case norm: NormalData if isOperational(chan) =>
          // We only can display one item so sort them by increasing importance
          val extraRoute = channelAndHop(chan) map { case _ \ route => route } getOrElse Vector.empty
          val isIncomingFeeTooHigh = extraRoute.nonEmpty && LNParams.isFeeBreach(extraRoute, msat = 1000000000L)
          if (isIncomingFeeTooHigh) setExtraInfo(text = me getString ln_info_high_fee format extraRoute.head.feeBreakdown)
          // In Turbo channels we will often have an OPEN state with NormalData and zeroconf
          if (norm.unknownSpend.isDefined) setExtraInfo(resource = ln_info_unknown_spend)
          if (fundingIsDead) setExtraInfo(resource = ln_info_funding_lost)

          // Relevant for Turbo channels: show funding depth until it reaches threshold
          val fundingDepthCondition = if (fundingDepth < threshold) -1 else R.id.fundingDepth
          visibleExcept(gone = fundingDepthCondition, R.id.closedAt)

        case _: NormalData | _: NegotiationsData =>
          setExtraInfo(resource = ln_info_coop_attempt)
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend,
            R.id.canReceive, R.id.refundFee, R.id.fundingDepth, R.id.closedAt)

        case wait: WaitBroadcastRemoteData =>
          if (fundingIsDead) setExtraInfo(resource = ln_info_funding_lost)
          if (wait.fundingError.isDefined) setExtraInfo(text = wait.fundingError.get)
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend,
            R.id.canReceive, R.id.closedAt, R.id.paymentsInFlight,
            R.id.totalPayments)

        case _: WaitFundingDoneData =>
          if (fundingIsDead) setExtraInfo(resource = ln_info_funding_lost)
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend,
            R.id.canReceive, R.id.closedAt, R.id.paymentsInFlight,
            R.id.totalPayments)

        case cd: ClosingData =>
          setExtraInfo(text = me closedBy cd)
          val closeDate = new Date(cd.closedAt)
          closedAtText setText time(closeDate).html

          // Show breaking fee if this is NOT a mutual closing
          val refundFeeCondition = if (cd.mutualClose.isEmpty) -1 else R.id.refundFee
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
            refundFeeCondition, R.id.fundingDepth, R.id.paymentsInFlight)

        case _ =>
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend,
            R.id.canReceive, R.id.refundFee, R.id.closedAt, R.id.fundingDepth,
            R.id.paymentsInFlight, R.id.totalPayments)
      }

      // MENU PART

      view setOnClickListener onButtonTap {
        val currentChanActions = chan.data match {
          // Unknown spend may be our own future commit, don't allow force-closing here
          case norm: NormalData if norm.unknownSpend.isDefined => normalChanActions take 1
          // Remote funding may not be visible yet, channel will be removed automatically later
          case _: WaitBroadcastRemoteData => normalChanActions take 1
          // Spending current commit here would be a channel breach
          case _: RefundingData => normalChanActions take 1
          // No reason to close an already closed channel
          case _: ClosingData => normalChanActions take 1
          case _ => normalChanActions
        }

        val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val alert = showForm(negBuilder(dialog_cancel, chan.data.announce.asString.html, lst).create)
        lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, currentChanActions)
        lst setDividerHeight 0
        lst setDivider null

        lst setOnItemClickListener onTap { pos =>
          def warnAndMaybeClose(channelClosureWarning: String) = {
            val bld = baseTextBuilder(channelClosureWarning.html).setCustomTitle(chan.data.announce.asString.html)
            mkCheckForm(alert => rm(alert)(chan process ChannelManager.CMDLocalShutdown), none, bld, dialog_ok, dialog_cancel)
          }

          rm(alert) {
            val htlcBlock = chan.inFlightHtlcs.nonEmpty
            val canCoopClose = isOpeningOrOperational(chan)
            val url = s"https://smartbit.com.au/tx/" + chan.fundTxId.toHex
            if (0 == pos) host startActivity new Intent(Intent.ACTION_VIEW, Uri parse url)
            else if (1 == pos && canCoopClose && htlcBlock) warnAndMaybeClose(me getString ln_chan_close_inflight_details)
            else if (1 == pos && canCoopClose) warnAndMaybeClose(me getString ln_chan_close_confirm_local)
            else if (1 == pos) warnAndMaybeClose(me getString ln_chan_force_details)
          }
        }
      }

      self
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
    app.kit.peerGroup removeBlocksDownloadedEventListener eventsListener
    for (chan <- displayedChans) chan.listeners -= eventsListener
  }

  def INIT(s: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_ln_ops
    me initToolbar findViewById(R.id.toolbar).asInstanceOf[Toolbar]
    getSupportActionBar setSubtitle app.plur1OrZero(barStatus, displayedChans.size)
    getSupportActionBar setTitle action_ln_details

    gridView setAdapter adapter
    gridView setNumColumns math.round(scrWidth / 2.4).toInt
    app.kit.peerGroup addBlocksDownloadedEventListener eventsListener
    for (chan <- displayedChans) chan.listeners += eventsListener
  } else me exitTo classOf[MainActivity]

  // UTILS

  def stateStatusColor(c: Channel) = (c.data, c.state) match {
    case (_: HostedCommits, OPEN) => me getString ln_info_status_open
    case (_: HostedCommits, SUSPENDED) => me getString ln_info_status_suspended
    case (_: NormalData, OPEN) if isOperational(c) => me getString ln_info_status_open
    case (_: NormalData, _) if !isOperational(c) => me getString ln_info_status_shutdown
    case (_: HasNormalCommits, WAIT_FUNDING_DONE) => me getString ln_info_status_opening
    case (_: HasNormalCommits, NEGOTIATIONS) => me getString ln_info_status_negotiations
    case _ => me getString ln_info_status_other format c.state
  }

  def canDisplayData(some: ChannelData) = some match {
    case ref: RefundingData => ref.remoteLatestPoint.isDefined
    case _: HasNormalCommits => true
    case _: HostedCommits => true
    case _ => false
  }

  def sumOrNothing(amt: Satoshi) = amt match {
    case Satoshi(0L) => getString(ln_info_nothing)
    case _ => denom parsedWithSign amt
  }

  def connectivityStatusColor(chan: Channel) =
    ConnectionManager.workers get chan.data.announce.nodeId match {
      case Some(w) if w.sock.isConnected => me getString ln_info_state_online
      case _ => me getString ln_info_state_offline
    }

  def closedBy(cd: ClosingData) =
    if (cd.remoteCommit.nonEmpty) me getString ln_info_close_remote
    else if (cd.nextRemoteCommit.nonEmpty) me getString ln_info_close_remote
    else if (cd.mutualClose.nonEmpty) me getString ln_info_close_coop
    else me getString ln_info_close_local

  def getStat(chanId: ByteVector) = {
    val cursor = LNParams.db.select(PaymentTable.selectPaymentNumSql, chanId)
    RichCursor(cursor) headTry { case RichCursor(c1) => c1 getLong 0 } getOrElse 0L
  }
}