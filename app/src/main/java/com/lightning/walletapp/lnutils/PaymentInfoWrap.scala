package com.lightning.walletapp.lnutils

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Vibr._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRouteVec
import com.lightning.walletapp.ln.crypto.Sphinx.PublicKeyVec
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import android.support.v4.app.NotificationCompat
import com.lightning.walletapp.helper.RichCursor
import com.lightning.walletapp.ln.Tools.none
import com.lightning.walletapp.MainActivity
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.walletapp.Utils.app
import com.lightning.walletapp.R
import scala.collection.mutable

import android.app.{AlarmManager, NotificationManager, PendingIntent}
import android.content.{BroadcastReceiver, Context, Intent}
import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}


object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  private[this] val pendingPayments = mutable.Map.empty[BinaryData, RoutingData]
  val goodRoutes = mutable.Map.empty[PublicKey, PaymentRouteVec]

  private def toRevoked(rc: RichCursor) = Tuple2(BinaryData(rc string RevokedTable.h160), rc long RevokedTable.expiry)
  def saveRevoked(h160: BinaryData, expiry: Long, number: Long) = db.change(RevokedTable.newSql, h160, expiry, number)
  def getAllRevoked(number: Long) = RichCursor apply db.select(RevokedTable.selectSql, number) vec toRevoked

  def extractPreimage(tx: Transaction) = {
    val fulfills = tx.txIn.map(txIn => txIn.witness.stack) collect {
      case Seq(_, pre, _) if pre.size == 32 => UpdateFulfillHtlc(null, 0L, pre)
      case Seq(_, _, _, pre, _) if pre.size == 32 => UpdateFulfillHtlc(null, 0L, pre)
    }

    fulfills foreach updOkOutgoing
    if (fulfills.nonEmpty) uiNotify
  }

  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(PaymentTable.selectSql, hash) headTry toPaymentInfo
  def updOkIncoming(u: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, u.amountMsat, System.currentTimeMillis, u.paymentHash)
  def updOkOutgoing(fulfill: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, fulfill.paymentPreimage, fulfill.paymentHash)
  def updStatus(status: Int, hash: BinaryData) = db.change(PaymentTable.updStatusSql, status, hash)

  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byQuery(query: String) = db.select(PaymentTable.searchSql, s"$query*")
  def byRecent = db select PaymentTable.selectRecentSql

  def toPaymentInfo(rc: RichCursor) = PaymentInfo(rc string PaymentTable.pr, rc string PaymentTable.preimage,
    rc int PaymentTable.incoming, rc int PaymentTable.status, rc long PaymentTable.stamp, rc string PaymentTable.description,
    rc string PaymentTable.hash, rc long PaymentTable.firstMsat, rc long PaymentTable.lastMsat, rc long PaymentTable.lastExpiry)

  def markFailedAndFrozen = db txWrap {
    db change PaymentTable.updFailWaitingSql
    for (hash <- app.ChannelManager.activeInFlightHashes) updStatus(WAITING, hash)
    for (hash <- app.ChannelManager.frozenInFlightHashes) updStatus(FROZEN, hash)
  }

  def failOnUI(rd: RoutingData) = {
    updStatus(FAILURE, rd.pr.paymentHash)
    uiNotify
  }

  def newRoutes(rd: RoutingData) = if (rd.callsLeft > 0) {
    val request = app.ChannelManager withRoutesAndOnionRD rd.copy(callsLeft = rd.callsLeft - 1)
    request.foreach(foeRD => app.ChannelManager.sendEither(foeRD, failOnUI), _ => me failOnUI rd)
  } else updStatus(FAILURE, rd.pr.paymentHash)

  override def onError = {
    case (_, exc: CMDException) => me failOnUI exc.rd
    case chan \ error => chan process CMDShutdown
  }

  override def outPaymentAccepted(rd: RoutingData) = {
    // This may be a new payment or an old payment retry attempt
    // Either insert or update should be executed successfully
    pendingPayments(rd.pr.paymentHash) = rd

    db txWrap {
      db.change(PaymentTable.updLastParamsSql, rd.lastMsat, rd.lastExpiry, rd.paymentHashString)
      db.change(PaymentTable.newSql, rd.pr.toJson, NOIMAGE, 0, WAITING, System.currentTimeMillis,
        rd.pr.description, rd.paymentHashString, rd.firstMsat, rd.lastMsat, rd.lastExpiry)
    }

    uiNotify
  }

  override def fulfillReceived(ok: UpdateFulfillHtlc) = {
    // Save preimage right away, don't wait for their commitSig
    // receiving a preimage means that payment is fulfilled
    updOkOutgoing(ok)

    pendingPayments.values.find(_.pr.paymentHash == ok.paymentHash) foreach { rd =>
      // Make payment searchable + runtime optimization: record last successful route
      db.change(PaymentTable.newVirtualSql, rd.qryText, rd.paymentHashString)
      goodRoutes(rd.pr.nodeId) = rd.usedRoute +: rd.routes
    }
  }

  override def sentSig(cs: Commitments) = db txWrap {
    for (waitRevocation <- cs.remoteNextCommitInfo.left) {
      val htlcs = waitRevocation.nextRemoteCommit.spec.htlcs
      for (Htlc(_, add) <- htlcs if add.amount >= cs.localParams.revokedSaveTolerance)
        saveRevoked(add.hash160, add.expiry, waitRevocation.nextRemoteCommit.index)
    }
  }

  override def settled(cs: Commitments) = {
    // Update affected record states in a database
    // then retry failed payments where possible

    db txWrap {
      for (Htlc(true, addHtlc) \ _ <- cs.localCommit.spec.fulfilled) updOkIncoming(addHtlc)
      for (Htlc(false, add) <- cs.localCommit.spec.malformed) updStatus(FAILURE, add.paymentHash)
      for (Htlc(false, add) \ failReason <- cs.localCommit.spec.failed) {

        val rd1Opt = pendingPayments get add.paymentHash
        rd1Opt map parseFailureCutRoutes(failReason) match {
          // Clear cached routes so they don't get in the way
          // then try use the routes left or fetch new ones

          case Some(prunedRD \ exclude) =>
            goodRoutes -= prunedRD.pr.nodeId
            for (entity <- exclude) BadEntityWrap.putEntity tupled entity
            app.ChannelManager.sendEither(useRoutesLeft(prunedRD), newRoutes)

          case _ =>
            // Either halted or not found at all
            updStatus(FAILURE, add.paymentHash)
        }
      }
    }

    if (cs.localCommit.spec.fulfilled.nonEmpty) {
      // Let the clouds know since they may be waiting
      // also vibrate to let a user know it's fulfilled
      OlympusWrap tellClouds OlympusWrap.CMDStart
      vibrate(lnSettled)
    }

    uiNotify
  }

  override def onProcessSuccess = {
    case (close: ClosingData, _: CMDBestHeight) if close.isOutdated =>
      // Mutual tx has enough confirmations or hard timeout passed out
      db.change(ChannelTable.killSql, close.commitments.channelId)
  }

  override def onBecome = {
    case (chan, _, from, CLOSING) if from != CLOSING =>
      val template = app getString R.string.chan_notice_body
      Notificator chanClosed template.format(chan.data.announce.alias)
      markFailedAndFrozen
      uiNotify

    case (chan, _, OFFLINE | WAIT_FUNDING_DONE, OPEN) if isOperational(chan) =>
      // We may need to send an LN payment in -> OPEN unless it is a shutdown
      OlympusWrap tellClouds OlympusWrap.CMDStart
  }
}

object ChannelWrap {
  def doPut(chanId: BinaryData, data: String) = db txWrap {
    // Insert and then update because of INSERT IGNORE effects
    db.change(ChannelTable.newSql, chanId, data)
    db.change(ChannelTable.updSql, data, chanId)
  }

  def put(data: HasCommitments) = {
    val raw = "1" + data.toJson.toString
    doPut(data.commitments.channelId, raw)
  }

  def get = {
    val rc = RichCursor(db select ChannelTable.selectAllSql)
    val res = rc.vec(_ string ChannelTable.data substring 1)
    res map to[HasCommitments]
  }
}

object BadEntityWrap {
  val putEntity = (entity: Any, targetNodeId: String, span: Long, msat: Long) => {
    // Insert and then update because of INSERT IGNORE effects on entity:targetNodeId key
    db.change(BadEntityTable.newSql, entity, targetNodeId, System.currentTimeMillis + span, msat)
    db.change(BadEntityTable.updSql, System.currentTimeMillis + span, msat, entity, targetNodeId)
  }

  def findRoutes(from: PublicKeyVec, targetId: PublicKey, rd: RoutingData) = {
    // Make sure the first cached route starts at a node which is operational currently
    // becuase of possible assisted routes a target node may not equal a payee node at this stage
    val good = PaymentInfoWrap.goodRoutes.get(rd.pr.nodeId).filter(from contains _.head.head.nodeId)
    good.map(Obs just _) getOrElse doFindRoutes(from, targetId, rd.firstMsat)
  }

  private def doFindRoutes(from: PublicKeyVec, targetId: PublicKey, msat: Long) = {
    // Short channel id length is 32 so anything of length beyond 60 is definitely a node id
    val cursor = db.select(BadEntityTable.selectSql, System.currentTimeMillis, msat, TARGET_ALL, targetId)
    val badNodes \ badChans = RichCursor(cursor).vec(_ string BadEntityTable.resId).partition(_.length > 60)
    OlympusWrap findRoutes OutRequest(badNodes, for (chanId <- badChans) yield chanId.toLong, from, targetId)
  }
}

// CHANNEL CLOSED NOTIFICATION

object Notificator {
  def chanClosed(extra: String) = try {
    val notificatorClass = classOf[Notificator]
    val parametersIntent = new Intent(app, notificatorClass).putExtra("extra", extra)
    val alarmManager = app.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]
    val pendingIntent = PendingIntent.getBroadcast(app, 0, parametersIntent, 0)
    alarmManager.set(AlarmManager.RTC_WAKEUP, 0, pendingIntent)
  } catch none
}

class Notificator extends BroadcastReceiver {
  def onReceive(ct: Context, intent: Intent) = try {
    // Immediately let user know a channel has been closed
    // used instead of toast so can be seen at later time

    val target = classOf[MainActivity]
    val targetIntent = PendingIntent.getActivity(ct, 0, new Intent(ct, target), PendingIntent.FLAG_UPDATE_CURRENT)
    val builder = new NotificationCompat.Builder(ct).setContentIntent(targetIntent).setSmallIcon(R.drawable.dead)
      .setAutoCancel(true).setContentTitle(ct getString R.string.chan_notice_title)
      .setContentText(intent.getExtras getString "extra")

    val service = ct.getSystemService(Context.NOTIFICATION_SERVICE)
    service.asInstanceOf[NotificationManager].notify(1, builder.build)
  } catch none
}