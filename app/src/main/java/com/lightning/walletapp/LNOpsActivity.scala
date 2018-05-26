package com.lightning.walletapp

import me.relex.circleindicator._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.ln.LNParams.broadcaster.getStatus
import com.lightning.walletapp.ln.LNParams.DepthAndDead
import android.view.View.OnTouchListener
import org.bitcoinj.script.ScriptBuilder
import scala.util.Success
import android.os.Bundle
import java.util.Date

import android.support.v4.app.{Fragment, FragmentStatePagerAdapter}
import android.view.{LayoutInflater, MotionEvent, View, ViewGroup}
import android.widget.{ArrayAdapter, Button, ListView}
import com.lightning.walletapp.ln.Tools.{none, wrap}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}


class LNOpsActivity extends TimerActivity { me =>
  def resetIndicator = UITask(chanPagerIndicator setViewPager chanPager)
  def INIT(s: Bundle) = if (app.isAlive) fillViewPager else me exitTo classOf[MainActivity]
  lazy val chanPager = findViewById(R.id.chanPager).asInstanceOf[android.support.v4.view.ViewPager]
  lazy val chanPagerIndicator = findViewById(R.id.chanPagerIndicator).asInstanceOf[CircleIndicator]
  lazy val localChanCache = for (c <- app.ChannelManager.all if me canDisplay c.data) yield c

  lazy val inFlightPayments = getResources getStringArray R.array.ln_in_flight_payments
  lazy val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  lazy val txsConfs = getResources getStringArray R.array.txs_confs

  lazy val basic = getString(ln_ops_chan_basic)
  lazy val negotiations = getString(ln_ops_chan_negotiations)
  lazy val unilateralClosing = getString(ln_ops_chan_unilateral_closing)
  lazy val bilateralClosing = getString(ln_ops_chan_bilateral_closing)
  lazy val statusLeft = getString(ln_ops_chan_unilateral_status_left)
  lazy val refundStatus = getString(ln_ops_chan_refund_status)
  lazy val amountStatus = getString(ln_ops_chan_amount_status)
  lazy val commitStatus = getString(ln_ops_chan_commit_status)

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

  val touchListener = new OnTouchListener {
    def onTouch(view: View, event: MotionEvent) = {
      if (event.getAction == MotionEvent.ACTION_DOWN) {
        val coords = event.getX / chanPagerIndicator.getWidth
        val position = (localChanCache.size * coords).toInt
        chanPager.setCurrentItem(position, false)
      }

      false
    }
  }

  val humanStatus: DepthAndDead => String = {
    case cfs \ false => app.plurOrZero(txsConfs, cfs)
    case _ \ true => txsConfs.last
    case _ => txsConfs.head
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
    chanPagerIndicator setOnTouchListener touchListener
    chanPager setAdapter slidingFragmentAdapter
    resetIndicator.run
  }

  def canDisplay(chanData: ChannelData) = chanData match {
    case ref: RefundingData => ref.remoteLatestPoint.isDefined
    case otherwise => true
  }

  def startedBy(c: ClosingData) = {
    val byRemote = c.remoteCommit.nonEmpty || c.nextRemoteCommit.nonEmpty
    if (byRemote) ln_ops_unilateral_peer else ln_ops_unilateral_you
  }
}

class ChanDetailsFrag extends Fragment with HumanTimeDisplay { me =>
  override def onCreateView(i: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    i.inflate(R.layout.frag_view_pager_chan, vg, false)

  override def onDestroy = wrap(super.onDestroy)(whenDestroy.run)
  var whenDestroy: Runnable = new Runnable { def run = none }
  lazy val host = getActivity.asInstanceOf[LNOpsActivity]
  import host._

  override def onViewCreated(view: View, state: Bundle) = {
    val lnOpsAction = view.findViewById(R.id.lnOpsAction).asInstanceOf[Button]
    val lnOpsDescription = Utils clickableTextField view.findViewById(R.id.lnOpsDescription)

    val chan = localChanCache(getArguments getInt "position")
    val nodeId = humanNode(chan.data.announce.nodeId.toString, "<br>")
    // It is assumed that all channels here always have commitments
    val started = me time new Date(chan(_.startedAt).get)
    val capacity = chan(_.commitInput.txOut.amount).get
    val alias = chan.data.announce.alias take 64

    // Order matters here
    lnOpsAction setOnClickListener onButtonTap {
      val openButCurrentlyOffline = isOperational(chan)
      val openAndOnline = openButCurrentlyOffline && chan.state == OPEN
      val coopClosePossible = openAndOnline && inFlightHtlcs(chan).isEmpty

      if (coopClosePossible) showCoopOptions
      else if (openAndOnline) showWarning(host getString ln_chan_close_inflight_details)
      else if (openButCurrentlyOffline) showWarning(host getString ln_chan_force_offline_details)
      else showWarning(host getString ln_chan_force_details)
    }

    def showCoopOptions = {
      val lst = host.getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
      val alert = showForm(negBuilder(dialog_cancel, host getString ln_chan_close_details, lst).create)
      val options = Array(host getString ln_chan_close_option_local, host getString ln_chan_close_option_address)
      lst setAdapter new ArrayAdapter(host, R.layout.frag_top_tip, R.id.titleTip, for (text <- options) yield text.html)
      lst setOnItemClickListener onTap { case 0 => sendToLocalWallet case 1 => sendToAddress }
      lst setDividerHeight 0
      lst setDivider null

      def sendToLocalWallet = rm(alert) {
        // Simple case: send refunding tx to this wallet
        showWarning(host getString ln_chan_close_confirm_local)
      }

      def sendToAddress = rm(alert) {
        app.getBufferTry map app.toAddress match {
          // Users have an option to provide a custom address
          // to omit a useless intermediary in-wallet transaction

          case Success(address) =>
            val where = sumOut format humanSix(address.toString)
            val bld = baseBuilder(host getString action_ln_close, host.getString(ln_chan_close_confirm_address).format(where).html)
            val customFinalPubKeyScript: Option[BinaryData] = Some(ScriptBuilder.createOutputScript(address).getProgram)
            mkForm(chan process CMDShutdown(customFinalPubKeyScript), none, bld, dialog_ok, dialog_cancel)

          case _ =>
            // No address is present
            app toast err_no_data
        }
      }
    }

    def showWarning(details: String) = {
      val bld = baseTextBuilder(details.html).setCustomTitle(host getString action_ln_close)
      mkForm(chan process app.ChannelManager.CMDLocalShutdown, none, bld, dialog_ok, dialog_cancel)
    }

    def manageOther = UITask {
      // Just show basic channel info here since we don't know the specifics about this one
      val text = basic.format(chan.state, alias, started, coloredIn apply capacity).html
      lnOpsAction setVisibility View.GONE
      lnOpsDescription setText text
    }

    def manageFunding(wait: WaitFundingDoneData) = UITask {
      val fundingTxId = Commitments fundingTxid wait.commitments
      val threshold = math.max(wait.commitments.remoteParams.minimumDepth, LNParams.minDepth)
      lnOpsDescription setText host.getString(ln_ops_chan_opening).format(chan.state, alias, started,
        coloredIn(capacity), app.plurOrZero(txsConfs, threshold), fundingTxId.toString,
        humanStatus(LNParams.broadcaster getStatus fundingTxId), nodeId).html

      // Initialize button
      lnOpsAction setVisibility View.VISIBLE
      lnOpsAction setText action_ln_close
    }

    def manageOpen = UITask {
      val canSend = MilliSatoshi apply estimateCanSend(chan)
      val canReceive = MilliSatoshi apply estimateCanReceive(chan)
      val finalCanSend = if (canSend.amount < 0L) coloredOut(canSend) else coloredIn(canSend)
      val finalCanReceive = if (canReceive.amount < 0L) coloredOut(canReceive) else coloredIn(canReceive)
      lnOpsDescription setText host.getString(ln_ops_chan_open).format(chan.state, alias, coloredIn(capacity),
        finalCanSend, finalCanReceive, app.plurOrZero(inFlightPayments, inFlightHtlcs(chan).size), nodeId).html

      // Initialize button
      lnOpsAction setVisibility View.VISIBLE
      lnOpsAction setText action_ln_close
    }

    def manageNegotiations(cs: Commitments) = UITask {
      val refundable = MilliSatoshi(cs.localCommit.spec.toLocalMsat)
      val inFlight = app.plurOrZero(inFlightPayments, inFlightHtlcs(chan).size)
      lnOpsDescription setText negotiations.format(chan.state, alias, started,
        coloredIn(capacity), coloredIn(refundable), inFlight).html

      // Initialize button
      lnOpsAction setVisibility View.VISIBLE
      lnOpsAction setText action_ln_force
    }

    def manageClosing(close: ClosingData) = UITask {
      // Show the best current closing with most confirmations
      // since multiple different closings may be present at once
      // or no closing at all in case like restoking a closed commit
      val closedTimestamp = me time new Date(close.closedAt)
      lnOpsAction setVisibility View.GONE

      close.bestClosing match {
        case Left(mutualClosingTx) =>
          val fee = capacity - mutualClosingTx.allOutputsAmount
          val refundable = MilliSatoshi(close.commitments.localCommit.spec.toLocalMsat) - fee
          val view = commitStatus.format(mutualClosingTx.txid.toString, humanStatus apply getStatus(mutualClosingTx.txid), coloredOut apply fee)
          val text = bilateralClosing.format(chan.state, alias, started, closedTimestamp, coloredIn(capacity), coloredIn(refundable), view)
          lnOpsDescription setText text.html

        case Right(info) =>
          val tier12View = info.getState collect {
            case ShowDelayed(_ \ true \ _, _, fee, amt) =>
              val deadDetails = amountStatus.format(denom formatted amt + fee, coloredOut apply fee)
              host.getString(ln_ops_chan_unilateral_status_dead).format(deadDetails, coloredIn apply amt)

            case ShowReady(_, fee, amt) =>
              val doneDetails = amountStatus.format(denom formatted amt + fee, coloredOut apply fee)
              host.getString(ln_ops_chan_unilateral_status_done).format(doneDetails, coloredIn apply amt)

            case show @ ShowDelayed(_ \ false \ _, _, fee, amt) if show.isPublishable =>
              // This fails if input is spent by our peer, happens when we publish a revoked commit
              val doneDetails = amountStatus.format(denom formatted amt + fee, coloredOut apply fee)
              host.getString(ln_ops_chan_unilateral_status_done).format(doneDetails, coloredIn apply amt)

            case ShowDelayed(_ \ false \ left, _, fee, amt) =>
              val leftDetails = amountStatus.format(denom formatted amt + fee, coloredOut apply fee)
              statusLeft.format(app.plurOrZero(blocksLeft, left), leftDetails, coloredIn apply amt)
          }

          val startedByWhom = host getString startedBy(close)
          val humanTier12View = tier12View take 2 mkString "<br><br>"
          val status = humanStatus apply getStatus(info.commitTx.txid)
          val commitFee = coloredOut(capacity - info.commitTx.allOutputsAmount)
          val commitView = commitStatus.format(info.commitTx.txid.toString, status, commitFee)
          val refundsView = if (tier12View.isEmpty) new String else refundStatus + humanTier12View
          lnOpsDescription setText unilateralClosing.format(chan.state, startedByWhom, alias, started,
            closedTimestamp, coloredIn(capacity), commitView + refundsView).html
      }
    }

    val detailsListener = new ChannelListener {
      // Updates chan details to current chan state
      // must be removed once fragment is finished

      override def onProcessSuccess = {
        // Simply update current UI on each new block
        case (_, _, _: CMDBestHeight) => nullOnBecome(chan)
      }

      override def onBecome = {
        case (_, wait: WaitFundingDoneData, _, _) => manageFunding(wait).run
        case (_, _: NormalData, _, _) if isOperational(chan) => manageOpen.run
        case (_, norm: NormalData, _, _) => manageNegotiations(norm.commitments).run
        case (_, negs: NegotiationsData, _, _) => manageNegotiations(negs.commitments).run
        case (_, close: ClosingData, _, _) => manageClosing(close).run
        case otherwise => manageOther.run
      }
    }

    val transitionListener = new ChannelListener {
      // Updates circle indicator to current chan state
      // must also be removed once fragment is finished

      override def onBecome = {
        case (_, _, from, CLOSING) if from != CLOSING => resetIndicator.run
        case (_, _, OFFLINE | WAIT_FUNDING_DONE, OPEN) => resetIndicator.run
      }
    }

    val listeners = Vector(transitionListener, detailsListener)
    whenDestroy = UITask(chan.listeners --= listeners)
    detailsListener nullOnBecome chan
    chan.listeners ++= listeners
  }
}