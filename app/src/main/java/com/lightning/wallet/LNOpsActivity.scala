package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.R.string._
import com.lightning.wallet.lnutils.ImplicitConversions._
import android.support.v4.app.{Fragment, FragmentStatePagerAdapter}
import com.lightning.wallet.Utils.{denom, coloredOut, coloredIn}
import android.view.{LayoutInflater, View, ViewGroup}

import com.lightning.wallet.ln.LNParams.broadcaster.txStatus
import com.lightning.wallet.ln.LNParams.DepthAndDead
import me.relex.circleindicator.CircleIndicator
import com.lightning.wallet.ln.Tools.wrap
import com.lightning.wallet.Utils.app
import android.widget.Button
import android.os.Bundle
import java.util.Date


class LNOpsActivity extends TimerActivity with HumanTimeDisplay { me =>
  lazy val chanPager = findViewById(R.id.chanPager).asInstanceOf[android.support.v4.view.ViewPager]
  lazy val chanPagerIndicator = findViewById(R.id.chanPagerIndicator).asInstanceOf[CircleIndicator]

  lazy val slidingFragmentAdapter =
    new FragmentStatePagerAdapter(getSupportFragmentManager) {
      def getItem(pos: Int) = new ChanDetailsFrag(app.ChannelManager.all(pos), me)
      def getCount = app.ChannelManager.all.size
    }

  override def onBackPressed =
    if (0 == chanPager.getCurrentItem) super.onBackPressed
    else chanPager.setCurrentItem(chanPager.getCurrentItem - 1)

  def INIT(s: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_ops)
    chanPager setAdapter slidingFragmentAdapter
    chanPagerIndicator setViewPager chanPager
  } else me exitTo classOf[MainActivity]
}


class ChanDetailsFrag(chan: Channel, host: LNOpsActivity) extends Fragment { me =>
  lazy val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  lazy val txsConfs = getResources getStringArray R.array.txs_confs

  lazy val unilateralClosing = getString(ln_ops_chan_unilateral_closing)
  lazy val bilateralClosing = getString(ln_ops_chan_bilateral_closing)
  lazy val statusLeft = getString(ln_ops_chan_unilateral_status_left)
  lazy val refundStatus = getString(ln_ops_chan_refund_status)
  lazy val amountStatus = getString(ln_ops_chan_amount_status)
  lazy val commitStatus = getString(ln_ops_chan_commit_status)

  private val humanStatus: DepthAndDead => String = {
    case confs \ false => app.plurOrZero(txsConfs, confs)
    case _ \ true => txsConfs.last
    case _ => txsConfs.head
  }

  var whenStop: Runnable = _
  override def onStop = wrap(super.onStop)(whenStop.run)
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle) =
    inflater.inflate(R.layout.frag_chan_details, container, false)

  override def onViewCreated(view: View, state: Bundle) = {
    val lnOpsAction = view.findViewById(R.id.lnOpsAction).asInstanceOf[Button]
    val lnOpsDescription = Utils clickableTextField view.findViewById(R.id.lnOpsDescription)

    lazy val chanListener = new ChannelListener {
      // Updates UI accordingly to current chan state

      override def onBecome = {
        case (_, wait: WaitFundingDoneData, _, _) => host runOnUiThread manageFunding(wait)
        case (_, _: NormalData, _, _) if chan.isOperational => host runOnUiThread manageOpen
        case (_, _: NormalData, _, _) if !chan.isOperational => host runOnUiThread manageNegs
        case (_, _: NegotiationsData, _, _) => host runOnUiThread manageNegs
        case (_, close: ClosingData, _, _) if close.closings.nonEmpty =>
          // We may have an empty closing in some edge cases
          host runOnUiThread manageClosing(close)

        case _ => manageOther
      }

      override def onProcess = {
        case (_, _, _: CMDBestHeight) =>
          // Need to update UI on each block
          nullOnBecome(chan)
      }
    }

    def closeOnClick(title: Int) = lnOpsAction setOnClickListener host.onButtonTap {
      // First closing attempt will be a cooperative one, the second try will be uncooperative
      host.passWrap(host getString title) apply host.checkPass { pass => chan process CMDShutdown }
    }

    def manageOther = {
      // Just show some basic info here
      lnOpsDescription setText header(chan).html
      lnOpsAction setVisibility View.GONE
    }

    def manageFunding(wait: WaitFundingDoneData) = {
      val openStatus = humanStatus(LNParams.broadcaster txStatus wait.fundingTx.txid)
      val threshold = math.max(wait.commitments.remoteParams.minimumDepth, LNParams.minDepth)
      val balance = coloredIn(wait.commitments.commitInput.txOut.amount)

      // Set funding explanations
      lnOpsDescription setText getString(ln_ops_chan_opening).format(me header chan, balance,
        app.plurOrZero(txsConfs, threshold), wait.fundingTx.txid.toString, openStatus).html

      // Initialize button
      lnOpsAction setText ln_chan_close
      lnOpsAction setVisibility View.VISIBLE
      closeOnClick(ln_chan_close_details)
    }

    def manageOpen = {
      // Set open state explanations
      lnOpsDescription setText getString(ln_ops_chan_open).format(me header chan,
        chan.data.announce.nodeId.toString, chan(_.channelId).get.toString).html

      // Initialize button
      lnOpsAction setText ln_chan_close
      lnOpsAction setVisibility View.VISIBLE
      closeOnClick(ln_chan_close_details)
    }

    def manageNegs = {
      // Set negotiations explanations
      val text = host getString ln_ops_chan_negotiations
      lnOpsDescription setText text.format(me header chan).html

      // Initialize button
      lnOpsAction setText ln_force_close
      lnOpsAction setVisibility View.VISIBLE
      closeOnClick(ln_force_close)
    }

    // Show the best closing with most confirmations
    // Cooperative and uncooperative closings may be at once
    def manageClosing(data: ClosingData) = data.closings maxBy {
      case Left(mutualTx) => txStatus(mutualTx.txid) match { case cfs \ _ => cfs }
      case Right(info) => txStatus(info.commitTx.txid) match { case cfs \ _ => cfs }

    } match {
      case Left(mutualTx) =>
        val mutualTxHumanStatus = humanStatus apply txStatus(mutualTx.txid)
        val mutualFee = coloredOut(data.commitments.commitInput.txOut.amount - mutualTx.txOut.map(_.amount).sum)
        val mutualTxHumanView = commitStatus.format(mutualTx.txid.toString, mutualTxHumanStatus, mutualFee)
        lnOpsDescription setText bilateralClosing.format(me header chan, mutualTxHumanView).html
        lnOpsAction setVisibility View.GONE

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
        val commitFee = coloredOut(data.commitments.commitInput.txOut.amount - info.commitTx.txOut.map(_.amount).sum)
        val commitTxHumanView = commitStatus.format(info.commitTx.txid.toString, commitHumanStatus, commitFee)
        val combinedView = commitTxHumanView + refundStatus + tier2HumanView.mkString("<br><br>")
        lnOpsDescription setText unilateralClosing.format(me header chan, combinedView).html
        lnOpsAction setVisibility View.GONE
    }

    // Wire up local listener and reload it right away to update view
    wrap(chanListener nullOnBecome chan)(chan.listeners += chanListener)
    whenStop = anyToRunnable(chan.listeners -= chanListener)
  }

  private def header(chan: Channel) = {
    val humanStamp = host time new Date(chan(_.startedAt).get)
    val grayStamp = s"<font color=#999999>$humanStamp</font>"
    val alias = chan.data.announce.alias take 32
    s"${chan.state}<br>$grayStamp<br>$alias"
  }
}