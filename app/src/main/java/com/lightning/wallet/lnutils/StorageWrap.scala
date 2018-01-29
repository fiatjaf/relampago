package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.Vibr._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.Announcements._
import com.lightning.wallet.ln.AddErrorCodes._
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
    doPut(chanId.toString, "1" + data.toJson.toString)
  }

  def get = {
    // Supports simple versioning (1 is prepended)
    val rc = RichCursor(db select ChannelTable.selectAllSql)
    rc.vec(_ string ChannelTable.data substring 1) map to[HasCommitments]
  }
}

object PaymentInfoWrap extends PaymentInfoBag with ChannelListener {
  def updateStatus(status: Int, hash: BinaryData) = db.change(PaymentTable.updStatusSql, status, hash)
  def updateRouting(rpi: RuntimePaymentInfo) = db.change(PaymentTable.updRoutingSql, rpi.rd.toJson, rpi.pr.paymentHash)
  def updOkIncoming(u: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, u.amountMsat, System.currentTimeMillis, u.paymentHash)
  def updOkOutgoing(fulfill: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, fulfill.paymentPreimage, fulfill.paymentHash)
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(PaymentTable.selectSql, hash) headTry toPaymentInfo
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byQuery(query: String) = db.select(PaymentTable.searchSql, s"$query*")
  def byRecent = db select PaymentTable.selectRecentSql

  def toPaymentInfo(rc: RichCursor) = PaymentInfo(rawRd = rc string PaymentTable.rd, rawPr = rc string PaymentTable.pr,
    preimage = rc string PaymentTable.preimage, incoming = rc int PaymentTable.incoming, firstMsat = rc long PaymentTable.msat,
    status = rc int PaymentTable.status, stamp = rc long PaymentTable.stamp, text = rc string PaymentTable.text)

  // Records a number of retry attempts for a given outgoing payment hash
  val serverCallAttempts = mutable.Map.empty[BinaryData, Int] withDefaultValue 0

  override def onError = {
    case _ \ CMDAddExcept(_, ERR_IN_FLIGHT) =>
      // Don't mark in-flight payments as failed
      uiNotify

    case (_, exc: CMDException) =>
      // Needed for retry failures, also prevents shutdown
      updateStatus(FAILURE, exc.cmd.rpi.pr.paymentHash)
      uiNotify

    case chan \ error =>
      // Close and log an error
      chan process CMDShutdown
      Tools errlog error
  }

  override def onProcess = {
    case (_, _: NormalData, cmd: CMDAddHtlc) => db txWrap {
      // This may either be a new payment or an old payment retry
      // so either an insert or update should be executed successfully

      updateRouting(cmd.rpi)
      updateStatus(WAITING, cmd.rpi.pr.paymentHash)
      db.change(PaymentTable.newVirtualSql, cmd.rpi.searchText, cmd.rpi.pr.paymentHash)
      db.change(PaymentTable.newSql, cmd.rpi.pr.paymentHash, NOIMAGE, 0, cmd.rpi.firstMsat,
        WAITING, System.currentTimeMillis, cmd.rpi.text, cmd.rpi.pr.toJson, cmd.rpi.rd.toJson)

      // End transaction
      // update interface
      uiNotify
    }

    case (chan, norm: NormalData, _: CommitSig) => db txWrap {
      if (norm.commitments.localCommit.spec.fulfilled.nonEmpty) {
        // First thing to do is to update related record states in a database
        for (Htlc(true, add) \ fulfill <- norm.commitments.localCommit.spec.fulfilled) updOkIncoming(add)
        for (Htlc(false, _) \ fulfill <- norm.commitments.localCommit.spec.fulfilled) updOkOutgoing(fulfill)
        // Then we need to let the cloud know as it may be waiting for a payment
        cloud doProcess CMDStart
        vibrate(lnSettled)
      }

      for (Htlc(false, add) \ some <- norm.commitments.localCommit.spec.failed) some match {
        // A special case since only our peer can reject payments like this, leave this failed
        case _: UpdateFailMalformedHtlc => updateStatus(FAILURE, add.paymentHash)

        case updateFailHtlc: UpdateFailHtlc =>
          getPaymentInfo(add.paymentHash) foreach { info =>
            val runtime = RuntimePaymentInfo(info.rd, info.pr, info.firstMsat)
            val cutPaymentInfo = cutAffectedRoutes(updateFailHtlc)(runtime)

            completeRPI(cutPaymentInfo) match {
              case Some(cutPaymentInfoWithRoutes) =>
                // There are still routes left and we have just used one
                // if accepted: routing info will be upserted in onProcess
                // if not accepted: status will change to FAILURE in onError
                chan process CMDPlainAddHtlc(cutPaymentInfoWithRoutes)

              case None =>
                // Fail right away
                updateRouting(cutPaymentInfo)
                updateStatus(FAILURE, cutPaymentInfo.pr.paymentHash)
                // Extra channel is bad OR critical node is bad OR none blacklisted OR too many attempts
                val stop = cutPaymentInfo.canNotProceed(chan) || serverCallAttempts(add.paymentHash) > 8

                // Reset in case of second QR scan attempt
                if (stop) serverCallAttempts(add.paymentHash) = 0
                else app.ChannelManager.getOutPaymentObs(cutPaymentInfo)
                  .doOnSubscribe(serverCallAttempts(add.paymentHash) += 1)
                  .foreach(_ map CMDPlainAddHtlc foreach chan.process, none)
            }
          }
      }

      // End transaction
      // update interface
      uiNotify
    }

    case (_, close: ClosingData, _: CMDBestHeight) if close.isOutdated =>
      // Mutual tx has enough confirmations or hard timeout has passed out
      db.change(ChannelTable.killSql, close.commitments.channelId)
  }

  override def onBecome = {
    case (_, norm: NormalData, null, SYNC) => db txWrap {
      // At init there may be some WAITING payments which won't be retired
      // because user has closed an app before next retry has started
      // so first thing to do is just fail all the waiting payments
      db change PaymentTable.updFailWaitingSql

      for (Htlc(false, add) <- norm.commitments.localCommit.spec.htlcs)
        // These are the outgoing payments which will be retransmitted
        // so we should immediately reinstate their WAITING state
        updateStatus(WAITING, add.paymentHash)
    }

    case (_, _, SYNC | OPEN | NEGOTIATIONS, CLOSING) =>
      // WAITING will either be redeemed or refunded later
      db change PaymentTable.updFailWaitingSql
      uiNotify

    case (chan, _, SYNC | WAIT_FUNDING_DONE, OPEN) if chan.isOperational =>
      // We may need to send an LN payment in -> OPEN unless it is a shutdown
      // failed payments are really marked as FAILURE because of a branch above
      cloud doProcess CMDStart
  }
}