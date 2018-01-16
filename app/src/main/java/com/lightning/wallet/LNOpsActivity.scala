package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.LNParams.broadcaster.txStatus
import com.lightning.wallet.ln.LNParams.DepthAndDead
import com.lightning.wallet.lnutils.RatesSaver
import com.lightning.wallet.ln.Tools.wrap
import fr.acinq.bitcoin.Transaction
import android.widget.Button
import android.view.View
import android.os.Bundle
import java.util.Date


class LNOpsActivity extends TimerActivity with HumanTimeDisplay { me =>
  lazy val lnOpsAction = findViewById(R.id.lnOpsAction).asInstanceOf[Button]
  lazy val lnOpsDescription = Utils clickableTextField findViewById(R.id.lnOpsDescription)
  lazy val blocksLeft = getResources getStringArray R.array.ln_status_left_blocks
  lazy val txsConfs = getResources getStringArray R.array.txs_confs

  lazy val unilateralClosing = getString(ln_ops_chan_unilateral_closing)
  lazy val bilateralClosing = getString(ln_ops_chan_bilateral_closing)
  lazy val statusLeft = getString(ln_ops_chan_unilateral_status_left)
  lazy val refundStatus = getString(ln_ops_chan_refund_status)
  lazy val amountStatus = getString(ln_ops_chan_amount_status)
  lazy val commitStatus = getString(ln_ops_chan_commit_status)
  def goBitcoin(view: View) = me exitTo classOf[BtcActivity]

  def goStartChannel = {
    val minRequired = RatesSaver.rates.feeLive multiply 2
    lazy val required = sumIn.format(denom withSign minRequired)
    lazy val balance = sumIn.format(denom withSign app.kit.conf1Balance)
    if (app.kit.conf1Balance isGreaterThan minRequired) me exitTo classOf[LNStartActivity]
    else mkForm(me negBld dialog_ok, getString(err_ln_not_enough_funds).format(balance, required).html, null)
  }

  // We may scan a payment request while opening a channel, remove it here
  override def onResume = wrap(super.onResume) { app.TransData.value = null }
  // May change destroy action throughout an activity lifecycle, hence a var
  private[this] var whenDestroy = anyToRunnable(super.onDestroy)
  override def onDestroy = whenDestroy.run

  def INIT(state: Bundle) = {
    setContentView(R.layout.activity_ln_ops)
    val chanOpt = app.ChannelManager.notRefunding.headOption
    chanOpt map manageFirst getOrElse manageNoActiveChannel
  }

  private def manageFirst(chan: Channel) = {
    def manageOpening(c: Commitments, open: Transaction) = {
      val threshold = math.max(c.remoteParams.minimumDepth, LNParams.minDepth)
      val openStatus = humanStatus(LNParams.broadcaster txStatus open.txid)
      val balance = coloredIn(c.commitInput.txOut.amount)

      closeOnClick(ln_chan_close)
      lnOpsAction setText ln_chan_close
      lnOpsDescription setText getString(ln_ops_chan_opening).format(balance,
        app.plurOrZero(txsConfs, threshold), open.txid.toString, openStatus).html
    }

    def manageNegs(c: Commitments) = {
      val details = getString(ln_ops_chan_negotiations)
      lnOpsDescription setText details.html
      lnOpsAction setText ln_force_close
      closeOnClick(ln_force_close)
    }

    def closeOnClick(title: Int) = lnOpsAction setOnClickListener onButtonTap {
      // First closing attempt will be a cooperative one, second will be uncooperative
      passWrap(me getString title) apply checkPass { pass => chan process CMDShutdown }
    }

    val chanOpsListener = new ChannelListener {
      // Updates UI accordingly to changes in channel
      // also repeatedly broadcasts a funding transaction

      override def onBecome = {
        case (_, close: ClosingData, _, _)
          // GUARD: either a cooperative or uncooperative close
          // actual tx broadcastings are handled in LocalBroadcaster
          if close.mutualClose.nonEmpty || close.tier12States.nonEmpty =>
          me runOnUiThread manageClosing(close)

        case (_, wait: WaitFundingDoneData, _, _) =>
          // Channel is saved so we repeatedly send our funding on each launch
          // we put it here instead of LocalBroadcaster so it does not interfere
          // when user cancels channel creation at final stages in LNStartActivity
          me runOnUiThread manageOpening(wait.commitments, wait.fundingTx)
          app.kit watchFunding wait.commitments
          app.kit blockingSend wait.fundingTx

        case (_, norm: NormalData, _, _) if norm.remoteShutdown.isDefined => me runOnUiThread manageNegs(norm.commitments)
        case (_, norm: NormalData, _, _) if norm.localShutdown.isDefined => me runOnUiThread manageNegs(norm.commitments)
        case (_, negs: NegotiationsData, _, _) => me runOnUiThread manageNegs(negs.commitments)
        case _ if chan.isOperational => me exitTo classOf[LNActivity]
        case _ => me runOnUiThread manageNoActiveChannel
      }

      override def onProcess = {
        case (_, _, _: CMDBestHeight) =>
          // Need to update UI on each block
          reloadOnBecome(chan)
      }
    }

    whenDestroy = anyToRunnable {
      chan.listeners -= chanOpsListener
      super.onDestroy
    }

    chan.listeners += chanOpsListener
    chanOpsListener reloadOnBecome chan
  }

  // UI which does not need a channel
  private val humanStatus: DepthAndDead => String = {
    case confs \ false => app.plurOrZero(txsConfs, confs)
    case _ \ true => txsConfs.last
    case _ => txsConfs.head
  }

  // We need to show the best closing with most confirmations
  // we may have a cooperative and uncooperative closings at once
  private def manageClosing(data: ClosingData) = data.closings maxBy {
    case Left(mutualTx) => txStatus(mutualTx.txid) match { case cfs \ _ => cfs }
    case Right(info) => txStatus(info.commitTx.txid) match { case cfs \ _ => cfs }
  } match {
    case Left(mutualTx) =>
      val mutualTxHumanStatus = humanStatus apply txStatus(mutualTx.txid)
      val mutualFee = coloredOut(data.commitments.commitInput.txOut.amount - mutualTx.txOut.map(_.amount).sum)
      val mutualTxHumanView = commitStatus.format(mutualTx.txid.toString, mutualTxHumanStatus, mutualFee)
      lnOpsDescription setText bilateralClosing.format(mutualTxHumanView).html
      lnOpsAction setOnClickListener onButtonTap(goStartChannel)
      lnOpsAction setText ln_ops_start

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

      val startedAtView = time apply new Date(data.startedAt)
      val commitHumanStatus = humanStatus apply txStatus(info.commitTx.txid)
      val commitFee = coloredOut(data.commitments.commitInput.txOut.amount - info.commitTx.txOut.map(_.amount).sum)
      val commitTxHumanView = commitStatus.format(info.commitTx.txid.toString, commitHumanStatus, commitFee)
      val combinedView = commitTxHumanView + refundStatus + tier2HumanView.mkString("<br><br>")
      lnOpsDescription setText unilateralClosing.format(startedAtView, combinedView).html
      lnOpsAction setOnClickListener onButtonTap(goStartChannel)
      lnOpsAction setText ln_ops_start
  }

  // Offer to create a new channel
  private def manageNoActiveChannel = {
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}