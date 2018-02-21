package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.LNParams.broadcaster.txStatus
import android.widget.RadioGroup.OnCheckedChangeListener
import info.hoang8f.android.segmented.SegmentedGroup
import com.lightning.wallet.ln.LNParams.DepthAndDead
import me.relex.circleindicator.CircleIndicator
import android.os.Bundle
import java.util.Date

import android.support.v4.app.{Fragment, FragmentStatePagerAdapter}
import android.widget.{Button, RadioButton, RadioGroup}
import android.view.{LayoutInflater, View, ViewGroup}
import com.lightning.wallet.ln.Tools.{none, wrap}
import com.lightning.wallet.lnutils.StorageWrap
import fr.acinq.bitcoin.{MilliSatoshi, Satoshi}


class LNOpsActivity extends TimerActivity { me =>
  lazy val chanPager = findViewById(R.id.chanPager).asInstanceOf[android.support.v4.view.ViewPager]
  lazy val chanPagerIndicator = findViewById(R.id.chanPagerIndicator).asInstanceOf[CircleIndicator]
  lazy val viewFilter = findViewById(R.id.viewFilter).asInstanceOf[SegmentedGroup]
  lazy val typeAlive = findViewById(R.id.typeAlive).asInstanceOf[RadioButton]
  lazy val typeAll = findViewById(R.id.typeAll).asInstanceOf[RadioButton]
  var cachedDisplayedChannels = Vector.empty[Channel]

  val slidingFragmentAdapter =
    new FragmentStatePagerAdapter(getSupportFragmentManager) {
      def getItem(itemPosition: Int) = bundledFrag(itemPosition)
      def getCount = cachedDisplayedChannels.size
    }

  def bundledFrag(pos: Int) = {
    val frag = new ChanDetailsFrag
    val arguments: Bundle = new Bundle
    arguments.putInt("position", pos)
    frag setArguments arguments
    frag
  }

  def updateCountView = {
    typeAlive setText getString(ln_ops_type_alive).format(app.ChannelManager.notClosing.size).html
    typeAll setText getString(ln_ops_type_all).format(app.ChannelManager.all.size).html
  }

  def reloadCacheAndView = {
    val aliveOnly = viewFilter.getCheckedRadioButtonId == R.id.typeAlive
    app.prefs.edit.putBoolean(AbstractKit.CHAN_VIEW_ALIVE, aliveOnly).commit
    if (aliveOnly) cachedDisplayedChannels = app.ChannelManager.notClosingOrRefunding
    else cachedDisplayedChannels = app.ChannelManager.all.filter(_.state != REFUNDING)
    chanPager setAdapter slidingFragmentAdapter
    chanPagerIndicator setViewPager chanPager
    updateCountView
  }

  def INIT(s: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_ops)
    viewFilter setOnCheckedChangeListener new OnCheckedChangeListener {
      def onCheckedChanged(rg: RadioGroup, int: Int) = reloadCacheAndView
    }

    val aliveOnly = app.prefs.getBoolean(AbstractKit.CHAN_VIEW_ALIVE, true)
    if (aliveOnly) viewFilter check R.id.typeAlive else viewFilter check R.id.typeAll
  } else me exitTo classOf[MainActivity]
}

class ChanDetailsFrag extends Fragment with HumanTimeDisplay { me =>
  override def onCreateView(i: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    i.inflate(R.layout.frag_view_pager_chan, vg, false)

  lazy val inFlightPayments = getResources getStringArray R.array.ln_in_flight_payments
  lazy val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  lazy val txsConfs = getResources getStringArray R.array.txs_confs
  lazy val host = getActivity.asInstanceOf[LNOpsActivity]
  import host.UITask

  lazy val basic = getString(ln_ops_chan_basic)
  lazy val negotiations = getString(ln_ops_chan_negotiations)
  lazy val unilateralClosing = getString(ln_ops_chan_unilateral_closing)
  lazy val bilateralClosing = getString(ln_ops_chan_bilateral_closing)
  lazy val statusLeft = getString(ln_ops_chan_unilateral_status_left)
  lazy val refundStatus = getString(ln_ops_chan_refund_status)
  lazy val amountStatus = getString(ln_ops_chan_amount_status)
  lazy val commitStatus = getString(ln_ops_chan_commit_status)

  val humanStatus: DepthAndDead => String = {
    case cfs \ false => app.plurOrZero(txsConfs, cfs)
    case _ \ true => txsConfs.last
    case _ => txsConfs.head
  }

  var whenDestroy: Runnable = new Runnable { def run = none }
  override def onDestroy = wrap(super.onDestroy)(whenDestroy.run)

  override def onViewCreated(view: View, state: Bundle) = {
    val lnOpsAction = view.findViewById(R.id.lnOpsAction).asInstanceOf[Button]
    val lnOpsDescription = Utils clickableTextField view.findViewById(R.id.lnOpsDescription)
    val chan = host.cachedDisplayedChannels(getArguments getInt "position")
    val started = me time new Date(chan(_.startedAt).get)
    val capacity = chan(_.commitInput.txOut.amount).get
    val alias = chan.data.announce.alias take 64

    def closeOnClick(title: Int) = lnOpsAction setOnClickListener host.onButtonTap {
      // First closing attempt will be a cooperative one, the second try will be uncooperative
      host.passWrap(getString(title).html) apply host.checkPass { pass => chan process CMDShutdown }
    }

    def manageOther = UITask {
      // Just show basic channel info here since we don't know the specifics about this one
      lnOpsDescription setText basic.format(chan.state, started, coloredIn(capacity), alias)
      lnOpsAction setVisibility View.GONE
    }

    def manageFunding(wait: WaitFundingDoneData) = UITask {
      val openStatus = humanStatus(LNParams.broadcaster txStatus wait.fundingTx.txid)
      val threshold = math.max(wait.commitments.remoteParams.minimumDepth, LNParams.minDepth)
      lnOpsDescription setText getString(ln_ops_chan_opening).format(chan.state, started,
        coloredIn(capacity), alias, app.plurOrZero(txsConfs, threshold),
        wait.fundingTx.txid.toString, openStatus).html

      // Initialize button
      lnOpsAction setVisibility View.VISIBLE
      lnOpsAction setText action_ln_close
      closeOnClick(ln_chan_close_details)
    }

    def manageOpen = UITask {
      val canReceiveMsat = estimateCanReceive(chan)
      val canSpendMsat = estimateTotalCanSend(chan)
      val canReceive = MilliSatoshi(canReceiveMsat)
      val canSpend = MilliSatoshi(canSpendMsat)

      val hasUpdate = StorageWrap.getUpd(chan).isDefined
      val nodeId = humanNode(chan.data.announce.nodeId.toString, "<br>")
      val inFlight = app.plurOrZero(inFlightPayments, inFlightOutgoingHtlcs(chan).size)
      val canSpendHuman = if (canSpendMsat < 0L) coloredOut(canSpend) else coloredIn(canSpend)
      val canReceiveHuman = if (canReceiveMsat < 0L) coloredOut(canReceive) else coloredIn(canReceive)
      val canReceiveFinal = if (hasUpdate) canReceiveHuman else sumOut format getString(ln_ops_chan_receive_wait)
      lnOpsDescription setText getString(ln_ops_chan_open).format(chan.state, started, coloredIn(capacity),
        canSpendHuman, canReceiveFinal, alias, inFlight, nodeId).html

      // Initialize button
      lnOpsAction setVisibility View.VISIBLE
      lnOpsAction setText action_ln_close
      closeOnClick(ln_chan_close_details)
    }

    def manageNegotiations = UITask {
      val refundable = Satoshi(myBalanceMsat(chan) / 1000L)
      val inFlight = app.plurOrZero(inFlightPayments, inFlightOutgoingHtlcs(chan).size)
      lnOpsDescription setText negotiations.format(chan.state, started, coloredIn(capacity),
        coloredIn(refundable), alias, inFlight).html

      // Initialize button
      lnOpsAction setVisibility View.VISIBLE
      lnOpsAction setText action_ln_force
      closeOnClick(ln_chan_force_details)
    }

    def manageClosing(data: ClosingData) = UITask {
      // Show the best current closing with most confirmations
      // since multiple different closings may be present at once
      val closed = me time new Date(data.closedAt)
      lnOpsAction setVisibility View.GONE
      // If alive becomes closing
      host.updateCountView

      val best = data.closings maxBy {
        case Left(mutualTx) => txStatus(mutualTx.txid) match { case cfs \ _ => cfs }
        case Right(info) => txStatus(info.commitTx.txid) match { case cfs \ _ => cfs }
      }

      best match {
        case Left(mutualTx) =>
          val refundable = Satoshi(myBalanceMsat(chan) / 1000L)
          val status = humanStatus apply txStatus(mutualTx.txid)
          val myFee = coloredOut(capacity - mutualTx.allOutputsAmount)
          val mutualView = commitStatus.format(mutualTx.txid.toString, status, myFee)
          lnOpsDescription setText bilateralClosing.format(chan.state, started, closed,
            coloredIn(capacity), coloredIn(refundable), alias, mutualView).html

        case Right(info) =>
          val tier12View = info.getState collect {
            case ShowDelayed(_ \ true \ _, _, fee, amt) =>
              val deadDetails = amountStatus.format(denom formatted amt + fee, coloredOut apply fee)
              getString(ln_ops_chan_unilateral_status_dead).format(deadDetails, coloredIn apply amt)

            case ShowReady(_, fee, amt) =>
              val doneDetails = amountStatus.format(denom formatted amt + fee, coloredOut apply fee)
              getString(ln_ops_chan_unilateral_status_done).format(doneDetails, coloredIn apply amt)

            case show @ ShowDelayed(_ \ false \ _, _, fee, amt) if show.isPublishable =>
              // This fails if input is spent by our peer, happens when we publish a revoked commit
              val doneDetails = amountStatus.format(denom formatted amt + fee, coloredOut apply fee)
              getString(ln_ops_chan_unilateral_status_done).format(doneDetails, coloredIn apply amt)

            case ShowDelayed(_ \ false \ left, _, fee, amt) =>
              val leftDetails = amountStatus.format(denom formatted amt + fee, coloredOut apply fee)
              statusLeft.format(app.plurOrZero(blocksLeft, left), leftDetails, coloredIn apply amt)
          } take 2 mkString "<br><br>"

          val status = humanStatus apply txStatus(info.commitTx.txid)
          val commitFee = coloredOut(capacity - info.commitTx.allOutputsAmount)
          val commitView = commitStatus.format(info.commitTx.txid.toString, status, commitFee)
          val refundsView = if (tier12View.isEmpty) new String else refundStatus + tier12View
          lnOpsDescription setText unilateralClosing.format(chan.state, started, closed,
            coloredIn(capacity), alias, commitView + refundsView).html
      }
    }

    val chanListener = new ChannelListener {
      // Updates UI accordingly to current chan state

      override def onBecome = {
        case (_, waitFunding: WaitFundingDoneData, _, _) => manageFunding(waitFunding).run
        case (_, close: ClosingData, _, _) if close.closings.nonEmpty => manageClosing(close).run
        case (_, _: NormalData, _, _) if isOperational(chan) => manageOpen.run
        case (_, _: NegotiationsData, _, _) => manageNegotiations.run
        case (_, _: NormalData, _, _) => manageNegotiations.run
        case anythingElse => manageOther.run
      }

      override def onProcess = {
        case (_, _, _: CMDBestHeight) =>
          // Need to update UI on each block
          nullOnBecome(chan)
      }
    }

    // Wire up a local listener
    whenDestroy = UITask(chan.listeners -= chanListener)
    wrap(chanListener nullOnBecome chan)(chan.listeners += chanListener)
  }
}