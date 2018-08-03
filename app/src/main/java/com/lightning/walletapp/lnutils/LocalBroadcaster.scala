package com.lightning.walletapp.lnutils

import collection.JavaConverters._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.wire.FundMsg._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import org.bitcoinj.core.TransactionConfidence.ConfidenceType._
import com.lightning.walletapp.ln.wire.{ChannelReestablish, Fail}
import com.lightning.walletapp.ln.Tools.none
import com.lightning.walletapp.Utils.app
import org.bitcoinj.core.Sha256Hash
import fr.acinq.bitcoin.BinaryData


object LocalBroadcaster extends Broadcaster {
  def perKwSixSat = RatesSaver.rates.feeSix.value / 4
  def perKwThreeSat = RatesSaver.rates.feeThree.value / 4
  def currentHeight = app.kit.wallet.getLastBlockSeenHeight

  def getTx(txid: BinaryData) = {
    val wrapped = Sha256Hash wrap txid.toArray
    Option(app.kit.wallet getTransaction wrapped)
  }

  def getStatus(txid: BinaryData) = getTx(txid) map { tx =>
    val isTxDead = tx.getConfidence.getConfidenceType == DEAD
    tx.getConfidence.getDepthInBlocks -> isTxDead
  } getOrElse 0 -> false

  def getBlockHashStrings(txid: BinaryData) = for {
    // Given a txid return a hash of containing blocks
    // note that this may return many block hashes

    txj <- getTx(txid).toVector
    hashes <- Option(txj.getAppearsInHashes).toVector
    hash <- hashes.keySet.asScala.toVector
  } yield hash.toString

  override def onProcessSuccess = {
    case (_, close: ClosingData, _: Command) =>
      // Repeatedly spend everything we can in this state in case it was unsuccessful before
      val tier12Publishable = for (state <- close.tier12States if state.isPublishable) yield state.txn
      val toSend = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ tier12Publishable
      for (tx <- toSend) try app.kit blockSend tx catch none

    case (chan, remote: WaitBroadcastRemoteData, _: ChannelReestablish) =>
      // Check if funding takes too much time or whether it's dead if present in a wallet
      val isOutdated = System.currentTimeMillis - remote.commitments.startedAt > 4 * 3600 * 1000L
      if (isOutdated && remote.fail.isEmpty) chan process Fail(FAIL_PUBLISH_ERROR, "Expired funding")

    case (chan, wait: WaitFundingDoneData, _: ChannelReestablish) if wait.our.isEmpty =>
      // CMDConfirmed may be sent to an offline channel and there will be no reaction
      // so always double check a funding state here as a failsafe measure

      for {
        txj <- getTx(wait.fundingTx.txid)
        depth \ isDead = getStatus(wait.fundingTx.txid)
        if depth >= LNParams.minDepth && !isDead
      } chan process CMDConfirmed(txj)
  }

  override def onBecome = {
    // Repeatedly resend a funding tx, update feerate on becoming open
    case (_, wait: WaitFundingDoneData, _, _) => app.kit blockSend wait.fundingTx
    case (chan, _: NormalData, OFFLINE, OPEN) => chan process CMDFeerate(perKwThreeSat)
  }
}