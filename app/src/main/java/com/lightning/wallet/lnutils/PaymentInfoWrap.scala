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
import com.lightning.wallet.lnutils.olympus.OlympusWrap
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.ln.Tools.runAnd
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.BinaryData
import scala.collection.mutable


object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  def updateStatus(status: Int, hash: BinaryData) = db.change(PaymentTable.updStatusSql, status, hash)
  def updOkIncoming(u: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, u.amountMsat, System.currentTimeMillis, u.paymentHash)
  def updOkOutgoing(fulfill: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, fulfill.paymentPreimage, fulfill.paymentHash)
  def getPaymentInfo(paymentHash: BinaryData) = RichCursor apply db.select(PaymentTable.selectSql, paymentHash) headTry toPaymentInfo
  def byQuery(query: String) = db.select(PaymentTable.searchSql, s"$query*")
  def byRecent = db select PaymentTable.selectRecentSql

  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  var pendingPayments = mutable.Map.empty[BinaryData, RoutingData]

  def toPaymentInfo(rc: RichCursor) =
    PaymentInfo(rawPr = rc string PaymentTable.pr, preimage = rc string PaymentTable.preimage, incoming = rc int PaymentTable.incoming,
      status = rc int PaymentTable.status, stamp = rc long PaymentTable.stamp, description = rc string PaymentTable.description,
      hash = rc string PaymentTable.hash, firstMsat = rc long PaymentTable.firstMsat, lastMsat = rc long PaymentTable.lastMsat,
      lastExpiry = rc long PaymentTable.lastExpiry)

  def markFailedAndFrozen = db txWrap {
    db change PaymentTable.updFailAllWaitingSql
    for (hash <- app.ChannelManager.activeInFlightHashes) updateStatus(WAITING, hash)
    for (hash <- app.ChannelManager.frozenInFlightHashes) updateStatus(FROZEN, hash)
  }

  def resend(reason: UpdateFailHtlc, hash: BinaryData) = {
    val rdOpt = pendingPayments get hash orElse getPaymentInfo(hash).map(emptyRDFromInfo).toOption
    val rd1BadEntitiesOpt = for (rd <- rdOpt) yield parseFailureCutRoutes(reason)(rd)

    rd1BadEntitiesOpt match {
      case Some(rd1 \ badEntities) =>
        badEntities foreach BadEntityWrap.put.tupled

      case None =>
        updateStatus(FAILURE, hash)
    }
  }

  override def onError = {
    case (_, exc: CMDException) =>
      // Retry failures, also prevents shutdown
      updateStatus(FAILURE, exc.rd.pr.paymentHash)
      uiNotify

    case chan \ error =>
      // Close and log an error
      chan process CMDShutdown
      Tools errlog error
  }

  override def onProcess = {
    case (_, _: NormalData, rd: RoutingData) =>
      // This may be a new payment or an old payment retry attempt
      // so either insert or update should be executed successfully

      db txWrap {
        pendingPayments(rd.pr.paymentHash) = rd.copy(callsLeft = rd.callsLeft - 1)
        db.change(PaymentTable.updLastSql, rd.lastMsat, rd.lastExpiry, rd.paymentHashString)
        updateStatus(WAITING, rd.pr.paymentHash)

        db.change(PaymentTable.newVirtualSql, rd.searchText, rd.paymentHashString, rd.paymentHashString)
        db.change(PaymentTable.newSql, rd.pr.toJson, NOIMAGE, 0, WAITING, System.currentTimeMillis,
          rd.pr.description, rd.paymentHashString, rd.firstMsat, rd.lastMsat, rd.lastExpiry)
      }

      // Display
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
        OlympusWrap tellClouds OlympusWrap.CMDStart
        vibrate(lnSettled)
      }

      // Display
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
      OlympusWrap tellClouds OlympusWrap.CMDStart
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
    val rc = RichCursor(db select ChannelTable.selectAllSql)
    val res = rc.vec(_ string ChannelTable.data substring 1)
    res map to[HasCommitments]
  }
}

object BadEntityWrap {
  type NodeIdOrShortChanId = Either[PublicKey, Long]
  val put = (resource: NodeIdOrShortChanId, target: String, span: Long) => resource match {
    case Left(nodeId) => db.change(BadEntityTable.newSql, nodeId.toString, TYPE_NODE, target, System.currentTimeMillis + span)
    case Right(shortId) => db.change(BadEntityTable.newSql, shortId.toString, TYPE_CHAN, target, System.currentTimeMillis + span)
  }

  def findRoutes(from: Set[PublicKey], recipientKey: String) = {
    def toResult(rc: RichCursor) = (rc string BadEntityTable.resId, rc string BadEntityTable.resType)
    val cursor = db.select(BadEntityTable.selectSql, System.currentTimeMillis, TARGET_ALL, recipientKey)
    val res = RichCursor(cursor) vec toResult

    val badNodes = res collect { case nodeId \ TYPE_NODE => nodeId }
    val badChans = res collect { case shortId \ TYPE_CHAN => shortId }
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