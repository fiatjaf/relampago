package com.lightning.walletapp.lnutils

import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import org.bitcoinj.core.TransactionConfidence.ConfidenceType._

import com.lightning.walletapp.ln.Tools.none
import com.lightning.walletapp.Utils.app
import org.bitcoinj.core.Sha256Hash
import fr.acinq.bitcoin.BinaryData


object LocalBroadcaster extends Broadcaster {
  def ratePerKwSat = RatesSaver.rates.feeLive.value / 4

  def currentHeight = {
    val processed = app.kit.wallet.getLastBlockSeenHeight
    val reported = app.kit.peerGroup.getMostCommonChainHeight
    math.max(processed, reported)
  }

  def getTx(txid: BinaryData) = {
    val wrapped = Sha256Hash wrap txid.toArray
    Option(app.kit.wallet getTransaction wrapped)
  }

  def getStatus(txid: BinaryData) = getTx(txid) map { tx =>
    val isTxDead = tx.getConfidence.getConfidenceType == DEAD
    tx.getConfidence.getDepthInBlocks -> isTxDead
  } getOrElse 0 -> false

  def getBlockHashString(txid: BinaryData) = for {
    // Given a txid return a hash of containing block
    // this will return a single block hash

    transaction <- getTx(txid)
    hashes <- Option(transaction.getAppearsInHashes)
    firstBlockHash = hashes.keySet.iterator.next
  } yield firstBlockHash.toString

  override def onProcessSuccess = {
    case (_, close: ClosingData, _: Command) =>
      val tier12Publishable = for (state <- close.tier12States if state.isPublishable) yield state.txn
      val toSend = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ tier12Publishable
      for (tx <- toSend) try app.kit blockingSend tx catch none
  }

  override def onBecome = {
    case (_, wait: WaitFundingDoneData, _, _) =>
      // Watch funding script, broadcast funding tx
      app.kit watchFunding wait.commitments
      app.kit blockingSend wait.fundingTx

    case (chan, norm: NormalData, OFFLINE, OPEN) =>
      // Check for fee changes once when channel becomes online
      val commitFee = norm.commitments.localCommit.spec.feeratePerKw
      val shouldUpdate = LNParams.shouldUpdateFee(ratePerKwSat, commitFee)
      if (shouldUpdate) chan.sendFeeUpdate(norm, ratePerKwSat)
  }
}