package com.lightning.walletapp.lnutils

import spray.json._
import com.lightning.walletapp.ln._
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
    val rdMinusOneCall = rd.copy(callsLeft = rd.callsLeft - 1)
    val request = app.ChannelManager.withRoutesAndOnionRD(rdMinusOneCall, useCache = false)
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

  override def fulfillReceived(ok: UpdateFulfillHtlc) = db txWrap {
    // Save preimage right away, don't wait for their next commitSig
    // receiving a preimage means that payment is fulfilled
    updOkOutgoing(ok)

    pendingPayments.values.find(_.pr.paymentHash == ok.paymentHash) foreach { rd =>
      // Make payment searchable + routing optimization: record subroutes in database
      db.change(PaymentTable.newVirtualSql, rd.qryText, rd.paymentHashString)
      if (rd.usedRoute.nonEmpty) RouteWrap putSubRoutes rd
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
          // Try use the routes left or fetch new ones if empty

          case Some(prunedRD \ excludes) =>
            for (entity <- excludes) BadEntityWrap.putEntity tupled entity
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
      com.lightning.walletapp.Vibr.vibrate
    }

    uiNotify
  }

  override def onProcessSuccess = {
    case (_, close: ClosingData, _: CMDBestHeight) if close.isOutdated =>
      // Mutual tx has enough confirmations or hard timeout has passed out
      db.change(ChannelTable.killSql, close.commitments.channelId)
  }

  override def onBecome = {
    case (chan, _, from, CLOSING) if from != CLOSING =>
      // Mark dropped and frozen payments, visually notify
      markFailedAndFrozen
      uiNotify

    case (chan, _, OFFLINE | WAIT_FUNDING_DONE, OPEN) if isOperational(chan) =>
      // We may need to send an LN payment in -> OPEN unless it is a shutdown
      Notificator.scheduleResyncNotificationOnceAgain
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

object RouteWrap {
  def putSubRoutes(rd: RoutingData) = {
    // This will only work if we have at least one hop, should check if route vector is empty
    // then merge each of generated subroutes with a respected routing node or recipient node key
    val subs = (rd.usedRoute drop 1).scanLeft(rd.usedRoute take 1) { case rs \ hop => rs :+ hop }

    for (_ \ node \ path <- rd.onion.sharedSecrets drop 1 zip subs) {
      val expiration = System.currentTimeMillis + 1000 * 3600 * 24 * 7
      val subPathJson = Vector(path).toJson.toString
      val subNodeString = node.toString

      db.change(RouteTable.newSql, subPathJson, subNodeString, expiration)
      db.change(RouteTable.updSql, subPathJson, expiration, subNodeString)
    }
  }

  def findRoutes(from: PublicKeyVec, targetId: PublicKey, rd: RoutingData) = {
    val cursor = db.select(RouteTable.selectSql, targetId, System.currentTimeMillis)
    val routeTry = RichCursor(cursor).headTry(_ string RouteTable.path) map to[PaymentRouteVec]
    val validRouteTry = routeTry.filter(from contains _.head.head.nodeId).map(rs => Obs just rs)
    validRouteTry getOrElse BadEntityWrap.findRoutes(from, targetId, rd)
  }
}

object BadEntityWrap {
  // entity is either nodeId or shortChannelId
  val putEntity = (entity: String, span: Long, msat: Long) => {
    db.change(BadEntityTable.newSql, entity, System.currentTimeMillis + span, msat)
    db.change(BadEntityTable.updSql, System.currentTimeMillis + span, msat, entity)
  }

  def findRoutes(from: PublicKeyVec, targetId: PublicKey, rd: RoutingData) = {
    // shortChannelId length is 32 so anything of length beyond 60 is definitely a nodeId
    val cursor = db.select(BadEntityTable.selectSql, System.currentTimeMillis, rd.firstMsat)
    val badNodes \ badChanIds = RichCursor(cursor).vec(_ string BadEntityTable.resId).partition(_.length > 60)
    OlympusWrap findRoutes OutRequest(badNodes, for (id <- badChanIds) yield id.toLong, from, targetId)
  }
}

object GossipCatcher extends ChannelListener {
  // Catch ChannelUpdate to enable funds receiving

  override def onProcessSuccess = {
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

// RESYNC NOTIFICATION

object Notificator {
  private[this] val notificatorClass = classOf[Notificator]
  def getIntent = PendingIntent.getBroadcast(app, 0, new Intent(app, notificatorClass), 0)
  def getAlarmManager = app.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]
  def removeResyncNotification = getAlarmManager cancel getIntent

  def scheduleResyncNotificationOnceAgain =
    try getAlarmManager.setAndAllowWhileIdle(AlarmManager.RTC_WAKEUP,
      System.currentTimeMillis + 1000 * 3600 * 24 * 21, getIntent) catch none
}

class Notificator extends BroadcastReceiver {
  def onReceive(ct: Context, intent: Intent) = classOf[MainActivity] match { case target =>
    val targetIntent = PendingIntent.getActivity(ct, 0, new Intent(ct, target), PendingIntent.FLAG_UPDATE_CURRENT)
    try ct.getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager].notify(1, new NotificationCompat.Builder(ct)
      .setContentIntent(targetIntent).setSmallIcon(R.drawable.dead).setContentTitle(ct getString R.string.notice_sync_title)
      .setContentText(ct getString R.string.notice_sync_body).setAutoCancel(true).build) catch none
  }
}