package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.Vibr._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.olympus.OlympusWrap.CMDStart
import com.lightning.wallet.lnutils.olympus.OlympusWrap
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.BinaryData
import fr.acinq.bitcoin.Crypto.PublicKey


object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  def updateStatus(status: Int, hash: BinaryData) = db.change(PaymentTable.updStatusSql, status, hash)
  def updateRouting(rpi: RuntimePaymentInfo) = db.change(PaymentTable.updRoutingSql, rpi.rd.toJson, rpi.pr.paymentHash)
  def updOkIncoming(u: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, u.amountMsat, System.currentTimeMillis, u.paymentHash)
  def updOkOutgoing(fulfill: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, fulfill.paymentPreimage, fulfill.paymentHash)
  def getPaymentInfo(paymentHash: BinaryData) = RichCursor apply db.select(PaymentTable.selectSql, paymentHash) headTry toPaymentInfo
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byQuery(query: String) = db.select(PaymentTable.searchSql, s"$query*")
  def byRecent = db select PaymentTable.selectRecentSql

  def toPaymentInfo(rc: RichCursor) = PaymentInfo(rawRd = rc string PaymentTable.rd, rawPr = rc string PaymentTable.pr,
    preimage = rc string PaymentTable.preimage, incoming = rc int PaymentTable.incoming, firstMsat = rc long PaymentTable.msat,
    status = rc int PaymentTable.status, stamp = rc long PaymentTable.stamp, description = rc string PaymentTable.description,
    hash = rc string PaymentTable.hash)

  def markFailedAndFrozen = db txWrap {
    db change PaymentTable.updFailAllWaitingSql
    for (hash <- app.ChannelManager.activeInFlightHashes) updateStatus(WAITING, hash)
    for (hash <- app.ChannelManager.frozenInFlightHashes) updateStatus(FROZEN, hash)
  }

  def halt(rpi: RuntimePaymentInfo) = {
    // This RPI has no routes but may have
    // new black nodes or channs so save it
    updateStatus(FAILURE, rpi.pr.paymentHash)
    updateRouting(rpi)
  }

  def resend(reason: UpdateFailHtlc, hash: BinaryData) = for {
    // This payment attempt has failed so get saved routing data,
    // cut affected routes, try to re-send if any routes still left

    info <- getPaymentInfo(hash)
    restoredRPI = RuntimePaymentInfo(info.rd, info.pr, info.firstMsat)
    updatedRPI = cutAffectedRoutes(fail = reason)(rpi = restoredRPI)
  } app.ChannelManager.sendEither(useRoutesLeft(updatedRPI), halt)

  override def onError = {
    case (_, exc: CMDException) =>
      // Retry failures, also prevents shutdown
      updateStatus(FAILURE, exc.rpi.pr.paymentHash)
      uiNotify

    case chan \ error =>
      // Close and log an error
      chan process CMDShutdown
      Tools errlog error
  }

  override def onProcess = {
    case (_, _: NormalData, rpi: RuntimePaymentInfo) =>
      // This may either be a new payment or an old payment retry attempt
      // so either insert or update should be executed successfully

      db txWrap {
        updateRouting(rpi)
        updateStatus(WAITING, rpi.pr.paymentHash)
        db.change(PaymentTable.newVirtualSql, rpi.searchText, rpi.paymentHashString)
        db.change(PaymentTable.newSql, rpi.paymentHashString, NOIMAGE, 0, rpi.firstMsat,
          WAITING, System.currentTimeMillis, rpi.pr.description, rpi.pr.toJson, rpi.rd.toJson)
      }

      // UI update
      uiNotify

    case (chan, norm: NormalData, _: CommitSig) =>
      // Update affected record states in a database
      // then retry failed payments where possible

      db txWrap {
        for (Htlc(true, add) \ fulfill <- norm.commitments.localCommit.spec.fulfilled) updOkIncoming(add)
        for (Htlc(false, _) \ fulfill <- norm.commitments.localCommit.spec.fulfilled) updOkOutgoing(fulfill)
        for (Htlc(false, add) <- norm.commitments.localCommit.spec.malformed) updateStatus(FAILURE, add.paymentHash)
        for (Htlc(false, add) \ reason <- norm.commitments.localCommit.spec.failed) resend(reason, add.paymentHash)
      }

      if (norm.commitments.localCommit.spec.fulfilled.nonEmpty) {
        // Let the cloud know since it may be waiting for a payment
        // also vibrate to let a user know that payment is fulfilled
        OlympusWrap tellClouds CMDStart
        vibrate(lnSettled)
      }

      // UI update
      uiNotify

    case (_, close: ClosingData, _: CMDBestHeight) if close.isOutdated =>
      // Mutual tx has enough confirmations or hard timeout has passed out
      db.change(ChannelTable.killSql, close.commitments.channelId)
  }

  override def onBecome = {
    case (_, _, OFFLINE | OPEN | NEGOTIATIONS, CLOSING) =>
      // WAITING will either be redeemed or refunded later
      markFailedAndFrozen
      uiNotify

    case (chan, _, OFFLINE | WAIT_FUNDING_DONE, OPEN) if isOperational(chan) =>
      // We may need to send an LN payment in -> OPEN unless it is a shutdown
      OlympusWrap tellClouds CMDStart
  }
}

object ChannelWrap {
  def doPut(chanId: String, data: String) = db txWrap {
    db.change(ChannelTable.newSql, params = chanId, data)
    db.change(ChannelTable.updSql, params = data, chanId)
  }

  def put(data: HasCommitments) = {
    val chanId = data.commitments.channelId
    val chanBody = "1" + data.toJson.toString
    doPut(chanId.toString, chanBody)
  }

  def get = {
    // Supports simple versioning: 1 is prepended
    val rc = RichCursor(db select ChannelTable.selectAllSql)
    rc.vec(_ string ChannelTable.data substring 1) map to[HasCommitments]
  }
}

object BadEntityWrap {
  def put(resId: String, resType: String, targetNodeId: String, span: Long) =
    db.change(BadEntityTable.newSql, resId, resType, targetNodeId,
      System.currentTimeMillis + span)

  def findRoutes(from: Set[PublicKey], recipientKey: String) = {
    def toResult(rc: RichCursor) = (rc string BadEntityTable.resType, rc string BadEntityTable.resId)
    val cursor = db.select(BadEntityTable.selectSql, System.currentTimeMillis, TARGET_ALL, recipientKey)
    val res = RichCursor(cursor) vec toResult

    val badNodes = res collect { case TYPE_NODE \ nodeId => nodeId }
    val badChans = res collect { case TYPE_CHAN \ shortId => shortId }
    OlympusWrap.findRoutes(badNodes, badChans, from, recipientKey)
  }
}

object GossipCatcher extends ChannelListener {
  // Catch ChannelUpdates to enable funds receiving

  override def onProcess = {
    case (chan, norm: NormalData, _: CMDBestHeight)
      // GUARD: don't have an extra hop, get the block
      if norm.commitments.extraHop.isEmpty =>

      // Extract funding txid and it's output index
      val txid = Commitments fundingTxid norm.commitments
      val outIdx = norm.commitments.commitInput.outPoint.index

      for {
        hash <- broadcaster getBlockHashString txid
        height \ txIds <- retry(OlympusWrap getBlock hash, pickInc, 4 to 5)
        shortChannelId <- Tools.toShortIdOpt(height, txIds indexOf txid.toString, outIdx)
      } chan process Hop(Tools.randomPrivKey.publicKey, shortChannelId, 0, 0L, 0L, 0L)

    case (chan, norm: NormalData, upd: ChannelUpdate)
      // GUARD: we already have an old or empty Hop, replace it with a new one
      if norm.commitments.extraHop.exists(_.shortChannelId == upd.shortChannelId) =>
      // Set a fresh update for this channel and process no further updates afterwards
      chan process upd.toHop(chan.data.announce.nodeId)
      chan.listeners -= GossipCatcher
  }
}