package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.R.string._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.Utils.{denom, coloredOut, coloredIn, app, humanNode}
import android.support.v4.app.{Fragment, FragmentStatePagerAdapter}
import android.view.{LayoutInflater, View, ViewGroup}
import com.lightning.wallet.ln.Tools.{none, wrap}

import com.lightning.wallet.ln.LNParams.broadcaster.txStatus
import com.lightning.wallet.ln.LNParams.DepthAndDead
import me.relex.circleindicator.CircleIndicator
import android.widget.Button
import android.os.Bundle
import java.util.Date


class LNOpsActivity extends TimerActivity { me =>
  lazy val chanPager = findViewById(R.id.chanPager).asInstanceOf[android.support.v4.view.ViewPager]
  lazy val chanPagerIndicator = findViewById(R.id.chanPagerIndicator).asInstanceOf[CircleIndicator]

  lazy val slidingFragmentAdapter =
    new FragmentStatePagerAdapter(getSupportFragmentManager) {
      def getItem(itemPosition: Int) = bundledFrag(itemPosition)
      def getCount = app.ChannelManager.all.size
    }

  def bundledFrag(pos: Int) = {
    val frag = new ChanDetailsFrag
    val arguments: Bundle = new Bundle
    arguments.putInt("position", pos)
    frag setArguments arguments
    frag
  }
  
  def INIT(s: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_ops)
    chanPager setAdapter slidingFragmentAdapter
    chanPagerIndicator setViewPager chanPager
  } else me exitTo classOf[MainActivity]
}

class ChanDetailsFrag extends Fragment with HumanTimeDisplay { me =>
  override def onCreateView(i: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    i.inflate(R.layout.frag_view_pager_chan, vg, false)

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
    val chan = app.ChannelManager.all(getArguments getInt "position")
    val started = me time new Date(chan(_.startedAt).get)
    val alias = chan.data.announce.alias take 64

    def closeOnClick(title: Int) = lnOpsAction setOnClickListener host.onButtonTap {
      // First closing attempt will be a cooperative one, the second try will be uncooperative
      host.passWrap(host getString title) apply host.checkPass { pass => chan process CMDShutdown }
    }

    def manageOther = UITask {
      // Just show basic channel info here since we don't know the specifics
      lnOpsDescription setText basic.format(chan.state, started, alias)
      lnOpsAction setVisibility View.GONE
    }

    def manageFunding(wait: WaitFundingDoneData) = UITask {
      val openStatus = humanStatus(LNParams.broadcaster txStatus wait.fundingTx.txid)
      val threshold = math.max(wait.commitments.remoteParams.minimumDepth, LNParams.minDepth)
      val capacity = coloredIn(wait.commitments.commitInput.txOut.amount)

      // Set funding explanations
      lnOpsDescription setText getString(ln_ops_chan_opening).format(chan.state, started, alias,
        capacity, app.plurOrZero(txsConfs, threshold), wait.fundingTx.txid.toString, openStatus).html

      // Initialize button
      lnOpsAction setText ln_chan_close
      lnOpsAction setVisibility View.VISIBLE
      closeOnClick(ln_chan_close_details)
    }

    def manageOpen = UITask {
      val humanChannel = humanNode(chan(_.channelId).get.toString, "<br>")
      lnOpsDescription setText getString(ln_ops_chan_open).format(chan.state, started,
        alias, humanNode(chan.data.announce.nodeId.toString, "<br>"), humanChannel).html

      // Initialize button
      lnOpsAction setText ln_chan_close
      lnOpsAction setVisibility View.VISIBLE
      closeOnClick(ln_chan_close_details)
    }

    def manageNegotiations = UITask {
      // Don't show stopped timestamp yet since this has no closing data
      lnOpsDescription setText negotiations.format(chan.state, started, alias).html

      // Initialize button
      lnOpsAction setText ln_force_close
      lnOpsAction setVisibility View.VISIBLE
      closeOnClick(ln_force_close)
    }

    def manageClosing(data: ClosingData) = UITask {
      // Show the best closing with most confirmations
      // since multiple different closings may be present

      data.closings maxBy {
        case Left(mutualTx) => txStatus(mutualTx.txid) match { case cfs \ _ => cfs }
        case Right(info) => txStatus(info.commitTx.txid) match { case cfs \ _ => cfs }
      } match {
        case Left(mutualTx) =>
          // TODO: this is not my Sat
          val mySat = mutualTx.txOut.reduce(_.amount + _.amount)
          val mutualTxHumanStatus = humanStatus apply txStatus(mutualTx.txid)
          val mutualFee = coloredOut(data.commitments.commitInput.txOut.amount - mySat)
          val mutualView = commitStatus.format(mutualTx.txid.toString, mutualTxHumanStatus, mutualFee)

          lnOpsAction setVisibility View.GONE
          lnOpsDescription setText bilateralClosing.format(chan.state, started,
            me time new Date(data.closedAt), alias, coloredIn(mySat), mutualView).html

        case Right(info) =>
          val tier2HumanView = info.getState collect {
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
          } take 3

          val commitHumanStatus = humanStatus apply txStatus(info.commitTx.txid)
          val refundPart = if (tier2HumanView.isEmpty) new String else refundStatus + tier2HumanView.mkString("<br><br>")
          val commitFee = coloredOut(data.commitments.commitInput.txOut.amount - info.commitTx.txOut.map(_.amount).sum)
          val commitTxHumanView = commitStatus.format(info.commitTx.txid.toString, commitHumanStatus, commitFee)

          lnOpsAction setVisibility View.GONE
          lnOpsDescription setText unilateralClosing.format(chan.state, started,
            me time new Date(data.closedAt), alias, commitTxHumanView + refundPart).html
      }
    }

    val chanListener = new ChannelListener {
      // Updates UI accordingly to current chan state

      override def onBecome = {
        case (_, w: WaitFundingDoneData, _, _) => manageFunding(w).run
        case (_, c: ClosingData, _, _) if c.closings.nonEmpty => manageClosing(c).run
        case (_, _: NormalData, _, _) if !chan.isOperational => manageNegotiations.run
        case (_, _: NormalData, _, _) if chan.isOperational => manageOpen.run
        case (_, _: NegotiationsData, _, _) => manageNegotiations.run
        case _ => manageOther.run
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