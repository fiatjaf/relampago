package com.lightning.walletapp.lnutils

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import com.lightning.walletapp.ln.crypto.Sphinx.PublicKeyVec
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import android.support.v4.app.NotificationCompat
import com.lightning.walletapp.helper.RichCursor
import com.lightning.walletapp.MainActivity
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.walletapp.Utils.app
import com.lightning.walletapp.R
import java.util.Collections

import android.app.{AlarmManager, NotificationManager, PendingIntent}
import android.content.{BroadcastReceiver, Context, Intent}
import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}


object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  var inFlightPayments = Map.empty[BinaryData, RoutingData]
  var unsent = Map.empty[BinaryData, RoutingData]

  def addPendingPayment(rd: RoutingData) = {
    // Add payment to unsents and try to resolve it
    unsent = unsent.updated(rd.pr.paymentHash, rd)
    me insertOrUpdateOutgoingPayment rd
    resolvePending
    uiNotify
  }

  def resolvePending = if (app.ChannelManager.currentBlocksLeft < 1) {
    // Send all pending payments only if we have an updated chain height
    unsent.values foreach fetchAndSend
    unsent = Map.empty
  }

  def fetchAndSend(rd: RoutingData) = app.ChannelManager.fetchRoutes(rd)
    .foreach(app.ChannelManager.sendEither(_, failOnUI), exc => me failOnUI rd)

  private def toRevoked(rc: RichCursor) = Tuple2(BinaryData(rc string RevokedTable.h160), rc long RevokedTable.expiry)
  def saveRevoked(h160: BinaryData, expiry: Long, number: Long) = db.change(RevokedTable.newSql, h160, expiry, number)
  def getAllRevoked(number: Long) = RichCursor apply db.select(RevokedTable.selectSql, number) vec toRevoked

  def extractPreimage(candidateTx: Transaction) = {
    val fulfills = candidateTx.txIn.map(_.witness.stack) collect {
      case Seq(_, pre, _) if pre.size == 32 => UpdateFulfillHtlc(NOCHANID, 0L, pre)
      case Seq(_, _, _, pre, _) if pre.size == 32 => UpdateFulfillHtlc(NOCHANID, 0L, pre)
    }

    fulfills foreach updOkOutgoing
    if (fulfills.nonEmpty) uiNotify
  }

  def updStatus(status: Int, hash: BinaryData) = db.change(PaymentTable.updStatusSql, status, hash)
  def updOkIncoming(m: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, m.amountMsat, System.currentTimeMillis, m.channelId, m.paymentHash)
  def updOkOutgoing(m: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, m.paymentPreimage, m.channelId, m.paymentHash)
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(PaymentTable.selectSql, hash) headTry toPaymentInfo
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byQuery(query: String) = db.select(PaymentTable.searchSql, s"$query*")
  def byRecent = db select PaymentTable.selectRecentSql

  def toPaymentInfo(rc: RichCursor) = PaymentInfo(rc string PaymentTable.pr, rc string PaymentTable.preimage,
    rc int PaymentTable.incoming, rc int PaymentTable.status, rc long PaymentTable.stamp, rc string PaymentTable.description,
    rc string PaymentTable.hash, rc long PaymentTable.firstMsat, rc long PaymentTable.lastMsat, rc long PaymentTable.lastExpiry)

  def insertOrUpdateOutgoingPayment(rd: RoutingData) = db txWrap {
    db.change(PaymentTable.updLastParamsSql, rd.lastMsat, rd.lastExpiry, rd.paymentHashString)
    db.change(PaymentTable.newSql, rd.pr.toJson, NOIMAGE, 0, WAITING, System.currentTimeMillis,
      rd.pr.description, rd.paymentHashString, rd.firstMsat, rd.lastMsat, rd.lastExpiry, NOCHANID)
  }

  def markFailedAndFrozen = db txWrap {
    db change PaymentTable.updFailWaitingAndFrozenSql
    for (hash <- app.ChannelManager.activeInFlightHashes) updStatus(WAITING, hash)
    for (hash <- app.ChannelManager.frozenInFlightHashes) updStatus(FROZEN, hash)
  }

  def failOnUI(rd: RoutingData) = {
    // Fail a payment and let user know
    updStatus(FAILURE, rd.pr.paymentHash)
    uiNotify
  }

  def newRoutes(rd: RoutingData) =
    app.ChannelManager checkIfSendable rd match {
      case Right(stillCanSendRD) if stillCanSendRD.callsLeft > 0 =>
        // Local conditions have not changed and we are still able to resend
        me fetchAndSend rd.copy(callsLeft = rd.callsLeft - 1, useCache = false)

      case _ =>
        // UI will be updated a bit later
        updStatus(FAILURE, rd.pr.paymentHash)
    }

  override def onException = {
    case _ \ HTLCExpiryException(norm, _) => // do nothing, prevent shutdown
    case _ \ CMDAddImpossible(rd, _) => me failOnUI rd // prevent shutdown
    case chan \ _ => chan process app.ChannelManager.CMDLocalShutdown
  }

  override def outPaymentAccepted(rd: RoutingData) = {
    // Payment has been accepted by channel so start local tracking
    inFlightPayments = inFlightPayments.updated(rd.pr.paymentHash, rd)
    me insertOrUpdateOutgoingPayment rd
  }

  override def fulfillReceived(ok: UpdateFulfillHtlc) = db txWrap {
    // Save preimage right away, don't wait for their next commitSig
    me updOkOutgoing ok

    inFlightPayments.values.find(_.pr.paymentHash == ok.paymentHash) foreach { rd =>
      // Make payment searchable + routing optimization: record subroutes in database
      db.change(PaymentTable.newVirtualSql, rd.queryText, rd.paymentHashString)
      if (rd.usedRoute.nonEmpty) RouteWrap cacheSubRoutes rd
    }
  }

  override def sentSig(cs: Commitments) = db txWrap {
    for (waitRevocation <- cs.remoteNextCommitInfo.left) {
      val activeHtlcs = waitRevocation.nextRemoteCommit.spec.htlcs
      for (Htlc(_, add) <- activeHtlcs if add.amount > cs.localParams.dustLimit)
        saveRevoked(add.hash160, add.expiry, waitRevocation.nextRemoteCommit.index)
    }
  }

  override def settled(cs: Commitments) = {
    // Update affected record states in a database
    // then retry failed payments where possible

    db txWrap {
      for (Htlc(true, addPayment) \ _ <- cs.localCommit.spec.fulfilled) me updOkIncoming addPayment
      for (Htlc(false, add) <- cs.localCommit.spec.malformed) updStatus(FAILURE, add.paymentHash)
      for (Htlc(false, add) \ failReason <- cs.localCommit.spec.failed) {

        val rdOpt = inFlightPayments get add.paymentHash
        rdOpt map parseFailureCutRoutes(failReason) match {
          // Try to use the routes left or fetch new ones if empty
          // but account for possibility of rd not being in place

          case Some(Some(prunedRD) \ excludes) =>
            for (entity <- excludes) BadEntityWrap.putEntity tupled entity
            app.ChannelManager.sendEither(useRoutesLeft(prunedRD), newRoutes)

          case _ =>
            // May happen after app restart
            // also when recipient sends an error
            updStatus(FAILURE, add.paymentHash)
        }
      }
    }

    if (cs.localCommit.spec.fulfilled.nonEmpty) {
      // Let the clouds know since they may be waiting
      // also vibrate to let a user know it's fulfilled
      OlympusWrap tellClouds OlympusWrap.CMDStart
      com.lightning.walletapp.Vibrator.vibrate
    }

    uiNotify
  }

  override def onProcessSuccess = {
    case (_, close: ClosingData, _: CMDBestHeight) if close.isOutdated =>
      val fundingScript = close.commitments.commitInput.txOut.publicKeyScript
      app.kit.wallet.removeWatchedScripts(Collections singletonList fundingScript)
      app.kit.wallet.removeWatchedScripts(app.kit closingPubKeyScripts close)
      db.change(ChannelTable.killSql, close.commitments.channelId)

    case (_, _: NormalData, _: UpdateAddHtlc) =>
      // We have just accepted an incoming payment
      // must periodically watch this chan from now on
      Notificator.scheduleResyncNotificationOnceAgain
  }

  override def onBecome = {
    case (chan, _, from, CLOSING) if from != CLOSING =>
      // Mark dropped and frozen payments and let user know
      markFailedAndFrozen
      uiNotify

    case (chan, _, WAIT_FUNDING_DONE, OPEN) =>
      // It should now be possible to buy tokens
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
  def cacheSubRoutes(rd: RoutingData) = {
    // This will only work if we have at least one hop, should check if route vector is empty
    // then merge each of generated subroutes with a respected routing node or recipient node key
    val subs = (rd.usedRoute drop 1).scanLeft(rd.usedRoute take 1) { case rs \ hop => rs :+ hop }

    for (_ \ node \ path <- rd.onion.sharedSecrets drop 1 zip subs) {
      val expiration = System.currentTimeMillis + 1000L * 3600 * 24 * 14
      val subPathJson = path.toJson.toString
      val subNodeString = node.toString

      db.change(RouteTable.newSql, subPathJson, subNodeString, expiration)
      db.change(RouteTable.updSql, subPathJson, expiration, subNodeString)
    }
  }

  def findRoutes(from: PublicKeyVec, targetId: PublicKey, rd: RoutingData) = {
    val cursor = db.select(RouteTable.selectSql, targetId, System.currentTimeMillis)
    val routeTry = RichCursor(cursor).headTry(_ string RouteTable.path) map to[PaymentRoute]
    // Channels could be closed so make sure we still have a matching channel for this cached route
    val validRouteTry = for (rt <- routeTry if from contains rt.head.nodeId) yield Obs just Vector(rt)

    db.change(RouteTable.killSql, targetId)
    // Remove cached route in case if it starts hanging our payments
    // this route will be put back again if payment was a successful one
    validRouteTry getOrElse BadEntityWrap.findRoutes(from, targetId, rd)
  }
}

object BadEntityWrap {
  val putEntity = (entity: String, span: Long, msat: Long) => {
    db.change(BadEntityTable.newSql, entity, System.currentTimeMillis + span, msat)
    db.change(BadEntityTable.updSql, System.currentTimeMillis + span, msat, entity)
  }

  def findRoutes(from: PublicKeyVec, targetId: PublicKey, rd: RoutingData) = {
    // shortChannelId length is 32 so anything of length beyond 60 is definitely a nodeId
    val cursor = db.select(BadEntityTable.selectSql, params = System.currentTimeMillis, rd.firstMsat)
    val badNodes \ badChans = RichCursor(cursor).set(_ string BadEntityTable.resId).partition(_.length > 60)
    val fromAsString = for (peerPubKey: PublicKey <- from.toSet) yield peerPubKey.toString

    // One of blacklisted nodes may become our peer or final payee so we remove them from bad nodes
    OlympusWrap findRoutes OutRequest(rd.firstMsat / 1000L, badNodes - targetId.toString -- fromAsString,
      for (shortChanId: String <- badChans) yield shortChanId.toLong, fromAsString, targetId.toString)
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
        hash <- broadcaster getBlockHashStrings txid
        blockHeight \ txIds <- OlympusWrap getBlock hash
        shortChannelId <- Tools.toShortIdOpt(blockHeight, txIds indexOf txid.toString, outIdx)
      } chan process Hop(Tools.randomPrivKey.publicKey, shortChannelId, 0, 0L, 0L, 0L)

    case (chan, norm: NormalData, upd: ChannelUpdate)
      // GUARD: we already have an old or empty Hop, replace it with a new one
      if norm.commitments.extraHop.exists(_.shortChannelId == upd.shortChannelId) =>
      // Set a fresh update for this channel and process no further updates afterwards
      chan.process(upd toHop chan.data.announce.nodeId)
      chan.listeners -= GossipCatcher
  }
}

object Notificator {
  private[this] val notificatorClass = classOf[Notificator]
  def removeResyncNotification = getAlarmManager cancel getIntent
  def getIntent = PendingIntent.getBroadcast(app, 0, new Intent(app, notificatorClass), 0)
  def getAlarmManager = app.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]

  def scheduleResyncNotificationOnceAgain =
    try getAlarmManager.setAndAllowWhileIdle(AlarmManager.RTC_WAKEUP,
      System.currentTimeMillis + 1000L * 3600 * 24 * 10, getIntent) catch none
}

class Notificator extends BroadcastReceiver {
  private[this] val mainClass = classOf[MainActivity]

  def onReceive(ct: Context, intent: Intent) = {
    val manager = ct.getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]
    val targetIntent = PendingIntent.getActivity(ct, 0, new Intent(ct, mainClass), PendingIntent.FLAG_UPDATE_CURRENT)
    try manager.notify(1, new NotificationCompat.Builder(ct).setContentIntent(targetIntent).setSmallIcon(R.drawable.dead)
      .setContentTitle(ct getString R.string.notice_sync_title).setContentText(ct getString R.string.notice_sync_body)
      .setAutoCancel(true).build) catch none
  }
}