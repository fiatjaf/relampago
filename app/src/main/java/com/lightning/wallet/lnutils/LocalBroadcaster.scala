package com.lightning.wallet.lnutils

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lnutils.ImplicitConversions._
import org.bitcoinj.core.TransactionConfidence.ConfidenceType._

import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Sha256Hash
import fr.acinq.bitcoin.BinaryData


object LocalBroadcaster extends Broadcaster {
  def ratePerKwSat: Long = RatesSaver.rates.feeLive.value / 2
  def currentHeight = math.max(app.kit.wallet.getLastBlockSeenHeight,
    app.kit.peerGroup.getMostCommonChainHeight)

  def txStatus(txid: BinaryData) = {
    val wrapped = Sha256Hash wrap txid.toArray
    Option(app.kit.wallet getTransaction wrapped) map { tx =>
      val isTxDead = tx.getConfidence.getConfidenceType == DEAD
      tx.getConfidence.getDepthInBlocks -> isTxDead
    } getOrElse 0 -> false
  }

  override def onBecome = {
    case (chan, norm: NormalData, SYNC, OPEN) =>
      // Check for fee changes once when channel becomes online
      val currentFee = norm.commitments.localCommit.spec.feeratePerKw
      val shouldUpdate = LNParams.shouldUpdateFee(currentFee, ratePerKwSat)
      if (shouldUpdate) chan.sendFeeUpdate(norm, ratePerKwSat)

    case (_, wait: WaitFundingDoneData, _, _) =>
      // Watch funding script, broadcast funding tx
      app.kit watchFunding wait.commitments
      app.kit blockingSend wait.fundingTx
  }

  override def onProcess = {
    case (_, close: ClosingData, _: Command) =>
      val tier12Publishable = for (ts <- close.tier12States if ts.isPublishable) yield ts.txn
      val toSend = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ tier12Publishable
      for (tx <- toSend) try app.kit blockingSend tx catch none
  }
}