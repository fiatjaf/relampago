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
import com.lightning.wallet.ln.RoutingInfoTag.PaymentRouteVec
import com.lightning.wallet.lnutils.olympus.OlympusWrap
import android.support.v4.app.NotificationCompat
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.ln.Tools.none
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.MainActivity
import com.lightning.wallet.Utils.app
import scala.collection.mutable
import com.lightning.wallet.R

import android.app.{AlarmManager, NotificationManager, PendingIntent}
import android.content.{BroadcastReceiver, Context, Intent}
import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}


object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  private[this] val pendingPayments = mutable.Map.empty[BinaryData, RoutingData]
  val goodRoutes = mutable.Map.empty[PublicKey, PaymentRouteVec]

  def extractPreimg(tx: Transaction) = {
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
  def updateStatus(status: Int, hash: BinaryData) = db.change(PaymentTable.updStatusSql, status, hash)
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byQuery(query: String) = db.select(PaymentTable.searchSql, s"$query*")
  def byRecent = db select PaymentTable.selectRecentSql

  def toPaymentInfo(rc: RichCursor) = PaymentInfo(rc string PaymentTable.pr, rc string PaymentTable.preimage,
    rc int PaymentTable.incoming, rc int PaymentTable.status, rc long PaymentTable.stamp, rc string PaymentTable.description,
    rc string PaymentTable.hash, rc long PaymentTable.firstMsat, rc long PaymentTable.lastMsat, rc long PaymentTable.lastExpiry)

  def markFailedAndFrozen = db txWrap {
    db change PaymentTable.updFailAllWaitingSql
    for (hash <- app.ChannelManager.activeInFlightHashes) updateStatus(WAITING, hash)
    for (hash <- app.ChannelManager.frozenInFlightHashes) updateStatus(FROZEN, hash)
  }

  def failOnUI(rd: RoutingData) = {
    updateStatus(FAILURE, rd.pr.paymentHash)
    uiNotify
  }

  def newRoutes(rd: RoutingData) = if (rd.callsLeft > 0) {
    val request = app.ChannelManager withRoutesAndOnionRD rd.copy(callsLeft = rd.callsLeft - 1)
    request.foreach(foeRD => app.ChannelManager.sendEither(foeRD, failOnUI), _ => me failOnUI rd)
  } else updateStatus(FAILURE, rd.pr.paymentHash)

  override def onError = {
    case (_, exc: CMDException) => me failOnUI exc.rd
    case chan \ error => chan process CMDShutdown
  }

  override def onProcess = {
    case (_, _: NormalData, rd: RoutingData) =>
      // This may be a new payment or an old payment retry attempt
      // Either insert or update should be executed successfully
      pendingPayments(rd.pr.paymentHash) = rd

      db txWrap {
        db.change(PaymentTable.updLastSql, rd.lastMsat, rd.lastExpiry, rd.paymentHashString)
        db.change(PaymentTable.newSql, rd.pr.toJson, NOIMAGE, 0, WAITING, System.currentTimeMillis,
          rd.pr.description, rd.paymentHashString, rd.firstMsat, rd.lastMsat, rd.lastExpiry)
      }

      // Display
      uiNotify

    case (_, _, fulfill: UpdateFulfillHtlc) =>
      // Save preimage right away, don't wait for commitSig
      // receiving a preimage means a payment is fulfilled
      updOkOutgoing(fulfill)

      pendingPayments.values.find(_.pr.paymentHash == fulfill.paymentHash) foreach { rd =>
        // Make payment searchable + runtime optimization: record last successful route
        db.change(PaymentTable.newVirtualSql, rd.qryText, rd.paymentHashString)
        goodRoutes(rd.pr.nodeId) = rd.usedRoute +: rd.routes
      }

    case (chan, norm: NormalData, _: CommitSig) =>
      // Update affected record states in a database
      // then retry failed payments where possible

      db txWrap {
        for (Htlc(true, add) \ fulfill <- norm.commitments.localCommit.spec.fulfilled) updOkIncoming(add)
        for (Htlc(false, add) <- norm.commitments.localCommit.spec.malformed) updateStatus(FAILURE, add.paymentHash)
        for (Htlc(false, add) \ why <- norm.commitments.localCommit.spec.failed) {

          // Runtime RD will not be present after restart
          val rdOpt = pendingPayments.get(add.paymentHash) orElse
            getPaymentInfo(add.paymentHash).map(emptyRDFromInfo).toOption

          rdOpt map parseFailureCutRoutes(why) match {
            case Some(rd1 \ badNodesAndChans) if rd1.ok =>
              // Not halted: try use the routes left or fetch new
              for (ban <- badNodesAndChans) BadEntityWrap.put tupled ban
              app.ChannelManager.sendEither(useRoutesLeft(rd1), newRoutes)

            // Payment is either halted or not found at all
            case _ => updateStatus(FAILURE, add.paymentHash)
          }

        }
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
    case (chan, _, from, CLOSING) if from != CLOSING =>
      // Frozen non-dust payments may be fulfilled on-chain
      Notificator chanClosed chan.data.announce.alias
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
  val put = (res: Any, targetNodeId: String, span: Long) => {
    db.change(BadEntityTable.newSql, res, targetNodeId, System.currentTimeMillis + span)
    db.change(BadEntityTable.updSql, System.currentTimeMillis + span, res, targetNodeId)
  }

  def findRoutes(from: Set[PublicKey], targetNodeId: PublicKey) =
    PaymentInfoWrap.goodRoutes get targetNodeId match {
      case None => doFindRoutes(from, targetNodeId)
      case Some(routes) => Obs just routes
    }

  private def doFindRoutes(from: Set[PublicKey], targetNodeId: PublicKey) = {
    // Hacky but acceptable: short cannel id length is 32 so anything larger than 60 is node id
    val cursor = db.select(BadEntityTable.selectSql, System.currentTimeMillis, TARGET_ALL, targetNodeId)
    val badNodes \ badChans = RichCursor(cursor).set(_ string BadEntityTable.resId).partition(_.length > 60)
    OlympusWrap.findRoutes(badNodes, badChans.map(_.toLong), from, targetNodeId.toString)
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

// CHANNEL CLOSED NOTIFICATION

object Notificator {
  def chanClosed(alias: String) = try {
    val notificatorClass = classOf[Notificator]
    val parametersIntent = new Intent(app, notificatorClass).putExtra("extra", alias)
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
    val message = ct getString R.string.chan_notice_message format intent.getExtras.getString("extra")
    val targetIntent = PendingIntent.getActivity(ct, 0, new Intent(ct, target), PendingIntent.FLAG_UPDATE_CURRENT)
    val builder = new NotificationCompat.Builder(ct).setContentIntent(targetIntent).setSmallIcon(R.drawable.dead)
      .setAutoCancel(true).setContentTitle(ct getString R.string.chan_notice_title).setContentText(message)

    val service = ct.getSystemService(Context.NOTIFICATION_SERVICE)
    service.asInstanceOf[NotificationManager].notify(1, builder.build)
  } catch none
}