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


object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  var pendingPayments = mutable.Map.empty[BinaryData, RoutingData]

  def extractPreimg(tx: Transaction) = {
    val fulfills = tx.txIn.map(txIn => txIn.witness.stack) collect {
      case Seq(_, pre, _) if pre.size == 32 => UpdateFulfillHtlc(null, 0L, pre)
      case Seq(_, _, _, pre, _) if pre.size == 32 => UpdateFulfillHtlc(null, 0L, pre)
    }

    fulfills foreach updOkOutgoing
    if (fulfills.nonEmpty) uiNotify
  }

  def updateStatus(status: Int, hash: BinaryData) = db.change(PaymentTable.updStatusSql, status, hash)
  def updOkIncoming(u: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, u.amountMsat, System.currentTimeMillis, u.paymentHash)
  def updOkOutgoing(fulfill: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, fulfill.paymentPreimage, fulfill.paymentHash)
  def getPaymentInfo(paymentHash: BinaryData) = RichCursor apply db.select(PaymentTable.selectSql, paymentHash) headTry toPaymentInfo
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

  def failRD(rd: RoutingData) = {
    // Utility to let user know it's failed
    updateStatus(FAILURE, rd.pr.paymentHash)
    uiNotify
  }

  def reCall(rd: RoutingData) = if (rd.callsLeft > 0) {
    val request = app.ChannelManager withRoutesAndOnionRD rd.copy(callsLeft = rd.callsLeft - 1)
    request.foreach(foeRD => app.ChannelManager.sendEither(foeRD, failRD), _ => me failRD rd)
  } else me failRD rd

  def resendMaybe(reason: UpdateFailHtlc, hash: BinaryData) = {
    lazy val reCreated = me getPaymentInfo hash map emptyRDFromInfo
    val rdOpt = pendingPayments get hash orElse reCreated.toOption
    val rd1BadData = rdOpt map parseFailureCutRoutes(reason)

    for (rd1 \ badsNodesChans <- rd1BadData) if (rd1.ok) {
      for (ban <- badsNodesChans) BadEntityWrap.put tupled ban
      app.ChannelManager.sendEither(useRoutesLeft(rd1), reCall)
    } else me failRD rd1
  }

  override def onError = {
    case (_, exc: CMDException) => me failRD exc.rd
    case chan \ error => chan process CMDShutdown
  }

  override def onProcess = {
    case (_, _: NormalData, rd: RoutingData) =>
      // This may be a new payment or an old payment retry attempt
      // so either insert or update should be executed successfully

      db txWrap {
        pendingPayments(rd.pr.paymentHash) = rd
        db.change(PaymentTable.updLastSql, rd.lastMsat, rd.lastExpiry, rd.paymentHashString)
        db.change(sql = PaymentTable.newVirtualSql, params = rd.qryText, rd.paymentHashString)
        db.change(sql = PaymentTable.newSql, params = rd.pr.toJson, NOIMAGE, 0, WAITING,
          System.currentTimeMillis, rd.pr.description, rd.paymentHashString,
          rd.firstMsat, rd.lastMsat, rd.lastExpiry)
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
        for (Htlc(false, add) \ reason <- norm.commitments.localCommit.spec.failed) resendMaybe(reason, add.paymentHash)
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
  type NodeOrShortChanId = Either[PublicKey, Long]
  val put = (res: NodeOrShortChanId, targetNodeId: String, span: Long) => res match {
    case Right(shortChannelId) => rmAndPut(shortChannelId, TYPE_CHAN, targetNodeId, span)
    case Left(nodeId) => rmAndPut(nodeId, TYPE_NODE, targetNodeId, span)
  }

  private def rmAndPut(resId: Any, resType: String, targetNodeId: String, span: Long) = {
    db.change(BadEntityTable.killSql, resId, targetNodeId, System.currentTimeMillis - 86400 * 3 * 1000)
    db.change(BadEntityTable.newSql, resId, resType, targetNodeId, System.currentTimeMillis + span)
  }

  def findRoutes(from: Set[PublicKey], recipientKey: String) = {
    def toResult(rc: RichCursor) = Tuple2(rc string BadEntityTable.resId, rc string BadEntityTable.resType)
    val cursor = db.select(BadEntityTable.selectSql, System.currentTimeMillis, TARGET_ALL, recipientKey)
    val res = RichCursor(cursor) vec toResult

    val badNodes = res collect { case nodeId \ TYPE_NODE => nodeId }
    val badChans = res collect { case sid \ TYPE_CHAN => sid.toLong }
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

// CLOSED CHANNEL NOTIFICATION

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
    val title = ct getString R.string.chan_notice_title format intent.getExtras.getString("extra")
    val targetIntent = PendingIntent.getActivity(ct, 0, new Intent(ct, target), PendingIntent.FLAG_UPDATE_CURRENT)
    val builder = new NotificationCompat.Builder(ct).setContentIntent(targetIntent).setSmallIcon(R.drawable.dead)
      .setAutoCancel(true).setContentTitle(title).setContentText(ct getString R.string.chan_notice_body)

    val service = ct.getSystemService(Context.NOTIFICATION_SERVICE)
    service.asInstanceOf[NotificationManager].notify(1, builder.build)
  } catch none
}