package com.lightning.walletapp

import spray.json._
import me.relex.circleindicator._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.LNParams.broadcaster.getStatus
import com.lightning.walletapp.helper.RichCursor
import android.view.View.OnTouchListener
import org.bitcoinj.script.ScriptBuilder
import scala.util.Success
import android.os.Bundle
import java.util.Date

import com.lightning.walletapp.ln.Tools.{none, wrap, memoize, runAnd}
import android.support.v4.app.{Fragment, FragmentStatePagerAdapter}
import android.view.{LayoutInflater, MotionEvent, View, ViewGroup}
import android.widget.{ArrayAdapter, Button, ListView}
import com.lightning.walletapp.lnutils.PaymentTable
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}


class LNOpsActivity extends TimerActivity { me =>
  def resetIndicator = UITask(chanPagerIndicator setViewPager chanPager)
  def INIT(s: Bundle) = if (app.isAlive) fillViewPager else me exitTo classOf[MainActivity]
  lazy val chanPager = findViewById(R.id.chanPager).asInstanceOf[android.support.v4.view.ViewPager]
  lazy val chanPagerIndicator = findViewById(R.id.chanPagerIndicator).asInstanceOf[CircleIndicator]
  lazy val localChanCache = for (chan <- app.ChannelManager.all if me canDisplay chan.data) yield chan
  lazy val chanActions = for (txt <- getResources getStringArray R.array.ln_chan_actions_list) yield txt.html
  lazy val paymentsInFlight = getResources getStringArray R.array.ln_in_flight_payments
  lazy val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  lazy val totalPayments = getResources getStringArray R.array.ln_total_payments
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
      case chan if isOperational(chan) && chan.state == OPEN => R.drawable.green_radius
      case chan if isOperational(chan) || isOpening(chan) => R.drawable.yellow_radius
      case _ => R.drawable.white_radius
    }
  }

  val touchListener = new OnTouchListener {
    def onTouch(circle: View, event: MotionEvent): Boolean = {
      if (event.getAction != MotionEvent.ACTION_DOWN) return false
      val coords = event.getX / chanPagerIndicator.getWidth
      val position = (localChanCache.size * coords).toInt
      chanPager.setCurrentItem(position, false)
      false
    }
  }

  val humanStatus: DepthAndDead => String = {
    case cfs \ false => app.plurOrZero(txsConfs, cfs)
    case _ \ true => txsConfs.last
    case _ => txsConfs.head
  }

  val getTotalSent: BinaryData => Vector[Long] = memoize { chanId =>
    val cursor = db.select(PaymentTable.selectTotalSql, chanId, 0)
    RichCursor(cursor).vec(_ long PaymentTable.lastMsat)
  }

  def bundledFrag(pos: Int) = {
    val frag = new ChanDetailsFrag
    val fragmentArguments = new Bundle
    fragmentArguments.putInt("pos", pos)
    frag setArguments fragmentArguments
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
}

class ChanDetailsFrag extends Fragment with HumanTimeDisplay { me =>
  override def onCreateView(i: LayoutInflater, v: ViewGroup, b: Bundle) =
    i.inflate(R.layout.frag_view_pager_chan, v, false)

  override def onDestroy = wrap(super.onDestroy)(whenDestroy.run)
  var whenDestroy: Runnable = new Runnable { def run = none }
  lazy val host = getActivity.asInstanceOf[LNOpsActivity]
  import host._

  override def onViewCreated(view: View, s: Bundle) = {
    val chan = localChanCache(getArguments getInt "pos")
    val lnOpsAction = view.findViewById(R.id.lnOpsAction).asInstanceOf[Button]
    val lnOpsDescription = Utils clickableTextField view.findViewById(R.id.lnOpsDescription)
    def warnAndForceClose = warnAndMaybeClose(host getString ln_chan_force_details)

    def warnAndMaybeClose(warning: String) =
      mkForm(chan process app.ChannelManager.CMDLocalShutdown,
        none, baseTextBuilder(warning.html), dialog_ok, dialog_cancel)

    chan { cs =>
      val alias = chan.data.announce.alias take 16
      val started = me time new Date(cs.startedAt)
      val capacity = cs.commitInput.txOut.amount

      def showCoopOptions = {
        val lst = host.getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val alert = showForm(negBuilder(dialog_cancel, host getString ln_chan_actions, lst).create)
        lst setAdapter new ArrayAdapter(host, R.layout.frag_top_tip, R.id.titleTip, chanActions)
        lst setDividerHeight 0
        lst setDivider null

        def proceedCoopCloseOrWarn(startCoopClosing: => Unit) = rm(alert) {
          val fundingOrOpenButOffline = isOperational(chan) || isOpening(chan)
          val fundingOrOpenAndOnline = fundingOrOpenButOffline && chan.state != OFFLINE

          if (fundingOrOpenAndOnline && inFlightHtlcs(chan).isEmpty) startCoopClosing
          else if (fundingOrOpenAndOnline) warnAndMaybeClose(host getString ln_chan_close_inflight_details)
          else if (fundingOrOpenButOffline) warnAndMaybeClose(host getString ln_chan_force_offline_details)
          else warnAndForceClose
        }

        def shareInfo = host share {
          val cId = cs.channelId.toString
          val w = LNParams.nodePublicKey.toString
          val peer = chan.data.announce.nodeId.toString
          s"Wallet: $w\n\nPeer: $peer\n\nChannel: $cId"
        }

        def closeToWallet = {
          // Simple case: send refunding transaction to this wallet
          warnAndMaybeClose(host getString ln_chan_close_confirm_local)
        }

        def closeToAddress = {
          app.getBufferTry map app.toAddress match {
            // Users have an option to provide a custom address
            // to omit a useless intermediary in-wallet transaction

            case Success(address) =>
              val where = humanSix(address.toString)
              val text = host.getString(ln_chan_close_confirm_address).format(where).html
              val customFinalPubKeyScript: Option[BinaryData] = Some(ScriptBuilder.createOutputScript(address).getProgram)
              mkForm(chan process CMDShutdown(customFinalPubKeyScript), none, baseTextBuilder(text), dialog_ok, dialog_cancel)

            case _ =>
              // No address is present
              app toast err_no_data
          }
        }

        lst setOnItemClickListener onTap {
          case 3 => proceedCoopCloseOrWarn(startCoopClosing = closeToWallet)
          case 2 => proceedCoopCloseOrWarn(startCoopClosing = closeToAddress)
          case 1 => host share chan.data.asInstanceOf[HasCommitments].toJson.toString
          case 0 => shareInfo
        }
      }

      def manageOther = UITask {
        // Just show basic channel info here since we don't know the specifics about it
        val text = basic.format(chan.state, alias, started, coloredIn apply capacity).html
        lnOpsAction setVisibility View.GONE
        lnOpsDescription setText text
      }

      def manageFunding(wait: WaitFundingDoneData) = UITask {
        val fundingTxId = Commitments fundingTxid wait.commitments
        val fundingStatus = humanStatus(LNParams.broadcaster getStatus fundingTxId)
        val threshold = math.max(wait.commitments.remoteParams.minimumDepth, LNParams.minDepth)
        lnOpsDescription setText host.getString(ln_ops_chan_opening).format(chan.state, alias, started,
          coloredIn(capacity), app.plurOrZero(txsConfs, threshold), fundingTxId.toString, fundingStatus).html

        // Show channel actions with cooperative closing options
        lnOpsAction setOnClickListener onButtonTap(showCoopOptions)
        lnOpsAction setVisibility View.VISIBLE
        lnOpsAction setText ln_chan_actions
      }

      def manageOpen = UITask {
        val totalSent = getTotalSent(cs.channelId)
        val valueSent = coloredOut apply MilliSatoshi(totalSent.sum)
        val payNum = app.plurOrZero(totalPayments, totalSent.size)

        val canSend = MilliSatoshi(Channel estimateCanSend chan)
        val canReceive = MilliSatoshi(Channel estimateCanReceive chan)
        val canSend1 = if (canSend.amount < 0L) coloredOut(canSend) else coloredIn(canSend)
        val canReceive1 = if (canReceive.amount < 0L) coloredOut(canReceive) else coloredIn(canReceive)

        val commitFee = MilliSatoshi(cs.reducedRemoteState.feesSat * 1000L)
        val localReserve = MilliSatoshi(cs.localParams.channelReserveSat * 1000L)
        val remoteReserve = MilliSatoshi(cs.remoteParams.channelReserveSatoshis * 1000L)

        val inFlightHTLC = app.plurOrZero(paymentsInFlight, inFlightHtlcs(chan).size)
        val openTemplate = if (channelAndHop(chan).isEmpty) ln_ops_chan_open_no_receive else ln_ops_chan_open
        lnOpsDescription setText host.getString(openTemplate).format(chan.state, alias, coloredIn(capacity),
          canSend1, coloredOut(remoteReserve), coloredOut(commitFee), canReceive1, coloredOut(localReserve),
          inFlightHTLC, payNum, valueSent).html

        // Show channel actions with cooperative closing options
        lnOpsAction setOnClickListener onButtonTap(showCoopOptions)
        lnOpsAction setVisibility View.VISIBLE
        lnOpsAction setText ln_chan_actions
      }

      def manageNegotiations(cs: Commitments) = UITask {
        val refundable = MilliSatoshi(cs.localCommit.spec.toLocalMsat)
        val inFlightHTLC = app.plurOrZero(paymentsInFlight, inFlightHtlcs(chan).size)
        lnOpsDescription setText negotiations.format(chan.state, alias, started,
          coloredIn(capacity), coloredIn(refundable), inFlightHTLC).html

        // Show warning and proceed with an uncooperative closing
        lnOpsAction setOnClickListener onButtonTap(warnAndForceClose)
        lnOpsAction setVisibility View.VISIBLE
        lnOpsAction setText ln_chan_force
      }

      def manageClosing(close: ClosingData) = UITask {
        // Show the best current closing with most confirmations
        val closedTimestamp = me time new Date(close.closedAt)
        lnOpsAction setVisibility View.GONE

        close.bestClosing match {
          case Left(mutualClosingTx) =>
            val fee = capacity - mutualClosingTx.allOutputsAmount
            val status = humanStatus apply getStatus(mutualClosingTx.txid)
            val refundable = MilliSatoshi(close.commitments.localCommit.spec.toLocalMsat) - fee
            val view = commitStatus.format(mutualClosingTx.txid.toString, status, coloredOut apply fee)
            lnOpsDescription setText bilateralClosing.format(chan.state, alias, started, closedTimestamp,
              coloredIn(capacity), coloredIn(refundable), view).html

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

            val humanTier12View = tier12View take 2 mkString "<br><br>"
            val status = humanStatus apply getStatus(info.commitTx.txid)
            val commitFee = coloredOut(capacity - info.commitTx.allOutputsAmount)
            val balance = MilliSatoshi(close.commitments.localCommit.spec.toLocalMsat)
            val commitView = commitStatus.format(info.commitTx.txid.toString, status, commitFee)
            val refundsView = if (tier12View.isEmpty) new String else refundStatus + humanTier12View

            val isRemote = close.remoteCommit.nonEmpty || close.nextRemoteCommit.nonEmpty
            val startedByWhom = if (isRemote) ln_ops_unilateral_peer else ln_ops_unilateral_you
            lnOpsDescription setText unilateralClosing.format(chan.state, host getString startedByWhom, alias,
              started, closedTimestamp, coloredIn(capacity), coloredIn(balance), commitView + refundsView).html
        }
      }

      val detailsListener = new ChannelListener {
        override def onBecome: PartialFunction[Transition, Unit] = {
          case (_, wait: WaitFundingDoneData, _, _) => manageFunding(wait).run
          case (_, _: NormalData, _, _) if isOperational(chan) => manageOpen.run
          case (_, norm: NormalData, _, _) => manageNegotiations(norm.commitments).run
          case (_, negs: NegotiationsData, _, _) => manageNegotiations(negs.commitments).run
          case (_, close: ClosingData, _, _) => manageClosing(close).run
          case _ => manageOther.run
        }

        override def onProcessSuccess = {
          // Simply update current UI on each new block
          case (_, _, _: CMDBestHeight) => nullOnBecome(chan)
        }
      }

      val transitionListener = new ChannelListener {
        override def onBecome: PartialFunction[Transition, Unit] = {
          case (_, _, from, OFFLINE) if from != OFFLINE => resetIndicator.run
          case (_, _, from, CLOSING) if from != CLOSING => resetIndicator.run
          case (_, _, OFFLINE | WAIT_FUNDING_DONE, OPEN) => resetIndicator.run
        }
      }

      lnOpsDescription setOnLongClickListener new View.OnLongClickListener {
        def onLongClick(descriptionText: View) = runAnd(true)(showCoopOptions)
      }

      val listeners = Vector(transitionListener, detailsListener)
      whenDestroy = UITask(chan.listeners --= listeners)
      detailsListener nullOnBecome chan
      chan.listeners ++= listeners
    }
  }
}