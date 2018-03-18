package com.lightning.wallet

import com.lightning.wallet.ln._
import me.relex.circleindicator._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.LNParams.broadcaster.getStatus
import com.lightning.wallet.ln.LNParams.DepthAndDead
import fr.acinq.bitcoin.MilliSatoshi
import android.widget.Button
import android.os.Bundle
import java.util.Date

import android.support.v4.app.{Fragment, FragmentStatePagerAdapter}
import android.view.{LayoutInflater, View, ViewGroup}
import com.lightning.wallet.ln.Tools.{none, wrap}


class LNOpsActivity extends TimerActivity { me =>
  def resetIndicator = UITask(chanPagerIndicator setViewPager chanPager)
  def INIT(s: Bundle) = if (app.isAlive) fillViewPager else me exitTo classOf[MainActivity]
  lazy val chanPager = findViewById(R.id.chanPager).asInstanceOf[android.support.v4.view.ViewPager]
  lazy val chanPagerIndicator = findViewById(R.id.chanPagerIndicator).asInstanceOf[CircleIndicator]
  lazy val localChanCache = for (c <- app.ChannelManager.all if c.state != REFUNDING) yield c

  val slidingFragmentAdapter =
    new FragmentStatePagerAdapter(getSupportFragmentManager) {
      def getItem(itemPosition: Int) = bundledFrag(itemPosition)
      def getCount = localChanCache.size
    }

  val colors = new IndicatorColorProvider {
    def getColor(position: Int) = localChanCache(position) match {
      case channel if isOperational(channel) => R.drawable.green_radius
      case channel if isOpening(channel) => R.drawable.yellow_radius
      case _ => R.drawable.white_radius
    }
  }

  def bundledFrag(pos: Int) = {
    val frag = new ChanDetailsFrag
    val arguments: Bundle = new Bundle
    arguments.putInt("position", pos)
    frag setArguments arguments
    frag
  }

  def fillViewPager = {
    setContentView(R.layout.activity_ln_ops)
    chanPagerIndicator.colorProvider = colors
    chanPager setAdapter slidingFragmentAdapter
    resetIndicator.run
  }
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
  lazy val nothingYet = getString(ln_ops_chan_receive_wait)

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

    val chan = host.localChanCache(getArguments getInt "position")
    val nodeId = humanNode(chan.data.announce.nodeId.toString, "<br>")
    val started = me time new Date(chan(_.startedAt).get)
    val capacity = chan(_.commitInput.txOut.amount).get
    val alias = chan.data.announce.alias take 64

    lnOpsAction setOnClickListener host.onButtonTap {
      // First closing attempt will be a cooperative one while the second attempt will always be an uncooperative one
      val msg = isOperationalOpen(chan) match { case true => ln_chan_close_details case false => ln_chan_force_details }
      val dlg = host.mkChoiceDialog(chan process CMDShutdown, none, dialog_ok, dialog_cancel)
      host showForm dlg.setMessage(getString(msg).html).create
    }

    def manageOther = UITask {
      // Just show basic channel info here since we don't know the specifics about this one
      lnOpsDescription setText basic.format(chan.state, started, coloredIn(capacity), alias)
      lnOpsAction setVisibility View.GONE
    }

    def manageFunding(wait: WaitFundingDoneData) = UITask {
      val fundingTxId = Commitments fundingTxid wait.commitments
      val threshold = math.max(wait.commitments.remoteParams.minimumDepth, LNParams.minDepth)
      lnOpsDescription setText getString(ln_ops_chan_opening).format(chan.state, alias, started,
        coloredIn(capacity), app.plurOrZero(txsConfs, threshold), fundingTxId.toString,
        humanStatus(LNParams.broadcaster getStatus fundingTxId), nodeId).html

      // Initialize button
      lnOpsAction setVisibility View.VISIBLE
      lnOpsAction setText action_ln_close
    }

    def manageOpen = UITask {
      val canReceive = MilliSatoshi apply estimateCanReceive(chan)
      val canSpend = MilliSatoshi apply estimateTotalCanSend(chan)

      val inFlight = app.plurOrZero(inFlightPayments, inFlightOutgoingHtlcs(chan).size)
      val canSpendHuman = if (canSpend.amount < 0L) coloredOut(canSpend) else coloredIn(canSpend)
      val canReceiveHuman = if (canReceive.amount < 0L) coloredOut(canReceive) else coloredIn(canReceive)
      val canReceiveFinal = if (channelAndHop(chan).isDefined) canReceiveHuman else sumOut format nothingYet
      lnOpsDescription setText getString(ln_ops_chan_open).format(chan.state, alias, started,
        coloredIn(capacity), canSpendHuman, canReceiveFinal, inFlight, nodeId).html

      // Initialize button
      lnOpsAction setVisibility View.VISIBLE
      lnOpsAction setText action_ln_close
    }

    def manageNegotiations = UITask {
      val refundable = MilliSatoshi apply estimateTotalCanSend(chan)
      val inFlight = app.plurOrZero(inFlightPayments, inFlightOutgoingHtlcs(chan).size)
      lnOpsDescription setText negotiations.format(chan.state, alias, started,
        coloredIn(capacity), coloredIn(refundable), inFlight).html

      // Initialize button
      lnOpsAction setVisibility View.VISIBLE
      lnOpsAction setText action_ln_force
    }

    def manageClosing(close: ClosingData) = UITask {
      // Show the best current closing with most confirmations
      // since multiple different closings may be present at once
      val closed = me time new Date(close.closedAt)
      lnOpsAction setVisibility View.GONE

      val best = close.closings maxBy {
        case Left(mutualTx) => getStatus(mutualTx.txid) match { case cfs \ _ => cfs }
        case Right(info) => getStatus(info.commitTx.txid) match { case cfs \ _ => cfs }
      }

      best match {
        case Left(mutualTx) =>
          val status = humanStatus apply getStatus(mutualTx.txid)
          val myFee = coloredOut(capacity - mutualTx.allOutputsAmount)
          val refundable = MilliSatoshi apply estimateTotalCanSend(chan)
          val mutualView = commitStatus.format(mutualTx.txid.toString, status, myFee)
          lnOpsDescription setText bilateralClosing.format(chan.state, alias, started,
            closed, coloredIn(capacity), coloredIn(refundable), mutualView).html

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

          val status = humanStatus apply getStatus(info.commitTx.txid)
          val commitFee = coloredOut(capacity - info.commitTx.allOutputsAmount)
          val commitView = commitStatus.format(info.commitTx.txid.toString, status, commitFee)
          val refundsView = if (tier12View.isEmpty) new String else refundStatus + tier12View
          lnOpsDescription setText unilateralClosing.format(chan.state, getStartedBy(close),
            alias, started, closed, coloredIn(capacity), commitView + refundsView).html
      }
    }

    def getStartedBy(c: ClosingData) = getString {
      val byThem = c.remoteCommit.nonEmpty || c.nextRemoteCommit.nonEmpty
      if (byThem) ln_ops_unilateral_peer else ln_ops_unilateral_you
    }

    val detailsListener = new ChannelListener {
      // Updates chan details to current chan state
      // must be removed once activity is finished

      override def onBecome = {
        case (_, waitFunding: WaitFundingDoneData, _, _) => manageFunding(waitFunding).run
        case (_, close: ClosingData, _, _) if close.closings.nonEmpty => manageClosing(close).run
        case (_, _: NormalData, _, _) if isOperational(chan) => manageOpen.run
        case (_, _: NegotiationsData, _, _) => manageNegotiations.run
        case (_, _: NormalData, _, _) => manageNegotiations.run
        case otherwise => manageOther.run
      }

      override def onProcess = {
        case (_, _, _: CMDBestHeight) =>
          // Need to update UI on each block
          nullOnBecome(chan)
      }
    }

    val transitionListener = new ChannelListener {
      // Updates circle indicator to current chan state
      // must also be removed once activity is finished

      override def onBecome = {
        case (_, _, from, CLOSING) if from != CLOSING => host.resetIndicator.run
        case (_, _, OFFLINE | WAIT_FUNDING_DONE, OPEN) => host.resetIndicator.run
      }
    }

    val listeners = Vector(transitionListener, detailsListener)
    wrap(chan.listeners ++= listeners)(detailsListener nullOnBecome chan)
    whenDestroy = UITask(chan.listeners --= listeners)
  }
}