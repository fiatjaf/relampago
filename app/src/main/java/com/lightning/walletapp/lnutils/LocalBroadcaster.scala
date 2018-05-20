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
  def perKwSixSat = RatesSaver.rates.feeSix.value / 4
  def perKwThreeSat = RatesSaver.rates.feeThree.value / 4

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

    txj <- getTx(txid)
    hashes <- Option(txj.getAppearsInHashes)
    firstBlockHash = hashes.keySet.iterator.next
  } yield firstBlockHash.toString

  override def onProcessSuccess = {
    case (_, close: ClosingData, c: Command) =>
      val tier12Publishable = for (state <- close.tier12States if state.isPublishable) yield state.txn
      val toSend = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ tier12Publishable
      for (tx <- toSend) try app.kit blockingSend tx catch none
  }

  override def onBecome = {
    case (chan, wait: WaitFundingDoneData, OFFLINE, WAIT_FUNDING_DONE) => for {
      // CMDConfirmed may be sent to an offline channel and there will be no reaction
      // so always double check a funding state here as a failsafe measure

      txj <- getTx(wait.fundingTx.txid)
      depth \ isDead = getStatus(wait.fundingTx.txid)
      if depth >= LNParams.minDepth && !isDead
    } chan process CMDConfirmed(txj)

    case (_, wait: WaitFundingDoneData, _, _) =>
      // Watch funding script, broadcast funding tx
      app.kit watchFunding wait.commitments
      app.kit blockingSend wait.fundingTx

    case (chan, norm: NormalData, OFFLINE, OPEN) =>
      // Updated feerates may arrive sooner then channel gets open
      // so inform channel about updated fees once it gets open
      chan process CMDFeerate(perKwThreeSat)
  }
}