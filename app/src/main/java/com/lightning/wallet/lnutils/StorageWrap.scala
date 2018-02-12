package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.Vibr._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.Announcements._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.Connector.CMDStart
import com.lightning.wallet.lnutils.JsonHttpUtils.to
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.BinaryData
import scala.collection.mutable


object StorageWrap extends ChannelListener {
  def put(value: String, key: String) = db txWrap {
    db.change(StorageTable.newSql, params = key, value)
    db.change(StorageTable.updSql, params = value, key)
  }

  def get(key: String) = {
    val cursor = db.select(StorageTable.selectSql, key)
    RichCursor(cursor).headTry(_ string StorageTable.value)
  }

  private def ourUpdate(norm: NormalData, upd: ChannelUpdate) = {
    val (_, _, outputIndex) = Tools.fromShortId(upd.shortChannelId)
    val idxMatch = norm.commitments.commitInput.outPoint.index == outputIndex
    idxMatch && !isDisabled(upd.flags) && checkSig(upd, norm.announce.nodeId)
  }

  override def onProcess = {
    case (chan, norm: NormalData, upd: ChannelUpdate) if ourUpdate(norm, upd) =>
      // Store update in a database and process no further updates afterwards
      put(upd.toJson.toString, norm.commitments.channelId.toString)
      chan.listeners -= StorageWrap
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

object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  def updateStatus(status: Int, hash: BinaryData) = db.change(PaymentTable.updStatusSql, status, hash)
  def updateRouting(rpi: RuntimePaymentInfo) = db.change(PaymentTable.updRoutingSql, rpi.rd.toJson, rpi.pr.paymentHash)
  def updOkIncoming(u: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, u.amountMsat, System.currentTimeMillis, u.paymentHash)
  def updOkOutgoing(fulfill: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, fulfill.paymentPreimage, fulfill.paymentHash)
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(PaymentTable.selectSql, hash) headTry toPaymentInfo
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byQuery(query: String) = db.select(PaymentTable.searchSql, s"$query*")
  def byRecent = db select PaymentTable.selectRecentSql

  def toPaymentInfo(rc: RichCursor) =
    PaymentInfo(rawRd = rc string PaymentTable.rd, rawPr = rc string PaymentTable.pr, preimage = rc string PaymentTable.preimage,
      incoming = rc int PaymentTable.incoming, firstMsat = rc long PaymentTable.msat, status = rc int PaymentTable.status,
      stamp = rc long PaymentTable.stamp, text = rc string PaymentTable.text)

  def markNotInFlightFailed = db txWrap {
    db change PaymentTable.updFailAllWaitingSql
    // Mark all WAITING payments as FAILURE and immediately restore those in flight
    val inFlight = app.ChannelManager.notClosing.flatMap(inFlightOutgoingHtlcs)
    for (htlc <- inFlight) updateStatus(WAITING, htlc.add.paymentHash)
  }

  def stop(rpi: RuntimePaymentInfo) = {
    val extraRoutes = rpi.pr.routingInfo.flatMap(_.route).toSet
    extraRoutes.map(_.shortChannelId).subsetOf(rpi.rd.badChans) ||
      extraRoutes.map(_.nodeId).subsetOf(rpi.rd.badNodes) ||
      rpi.rd.badNodes.contains(rpi.pr.nodeId) ||
      srvCallAttempts(rpi.pr.paymentHash) > 4
  }

  // Records a number of retry attempts for a given outgoing payment hash
  val srvCallAttempts = mutable.Map.empty[BinaryData, Int] withDefaultValue 0

  override def onError = {
    case (_, exc: CMDException) =>
      // Needed for retry failures, prevents shutdown
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
      // so either an insert or update should be executed successfully

      db txWrap {
        updateRouting(rpi)
        updateStatus(WAITING, rpi.pr.paymentHash)
        val text = rpi.pr.description.right getOrElse new String
        val searchQueryText = s"$text ${rpi.pr.paymentHash.toString}"
        db.change(PaymentTable.newVirtualSql, searchQueryText, rpi.pr.paymentHash)
        db.change(PaymentTable.newSql, rpi.pr.paymentHash, NOIMAGE, 0, rpi.firstMsat,
          WAITING, System.currentTimeMillis, text, rpi.pr.toJson, rpi.rd.toJson)
      }

      // UI update
      uiNotify

    case (chan, norm: NormalData, _: CommitSig) =>
      // Update affected record states in a database
      // then retry failed payments where possible

      db txWrap {
        for (Htlc(true, add) \ fulfill <- norm.commitments.localCommit.spec.fulfilled) updOkIncoming(add)
        for (Htlc(false, _) \ fulfill <- norm.commitments.localCommit.spec.fulfilled) updOkOutgoing(fulfill)
        for (Htlc(false, add) \ updateFailure <- norm.commitments.localCommit.spec.failed) updateFailure match {
          // Malformed outgoing HTLC is a special case which can only happen if our direct peer replies with it
          // in all other cases an onion error should be extracted and payment should be retried

          case malformed: UpdateFailMalformedHtlc =>
            updateStatus(FAILURE, add.paymentHash)

          case updateFailHtlc: UpdateFailHtlc =>
            getPaymentInfo(add.paymentHash) foreach { info =>
              // Try to extract an onion error and prune the rest of affected routes
              val restoredRPI = RuntimePaymentInfo(info.rd, info.pr, info.firstMsat)
              val updatedRPI = cutAffectedRoutes(updateFailHtlc)(restoredRPI)
              app.ChannelManager.sendOpt(useRoutesLeft(updatedRPI), fail)

              def fail = {
                updateStatus(FAILURE, add.paymentHash)
                if (me stop updatedRPI) srvCallAttempts(add.paymentHash) = 0
                else app.ChannelManager.withRoutesAndOnionRPI(updatedRPI)
                  .doOnSubscribe(srvCallAttempts(add.paymentHash) += 1)
                  .foreach(app.ChannelManager.sendOpt(_, none), none)
              }
            }
        }
      }

      if (norm.commitments.localCommit.spec.fulfilled.nonEmpty) {
        // Let the cloud know since it may be waiting for a payment
        // also vibrate to let a user know that payment is fulfilled
        cloud doProcess CMDStart
        vibrate(lnSettled)
      }

      // UI update
      uiNotify

    case (_, close: ClosingData, _: CMDBestHeight) if close.isOutdated =>
      // Mutual tx has enough confirmations or hard timeout has passed out
      db.change(ChannelTable.killSql, close.commitments.channelId)
  }

  override def onBecome = {
    case (_, _, SYNC | OPEN | NEGOTIATIONS, CLOSING) =>
      // WAITING will either be redeemed or refunded later
      markNotInFlightFailed
      uiNotify

    case (chan, _, SYNC | WAIT_FUNDING_DONE, OPEN) if isOperational(chan) =>
      // We may need to send an LN payment in -> OPEN unless it is a shutdown
      // failed payments are really marked as FAILURE because of a branch above
      cloud doProcess CMDStart
  }
}