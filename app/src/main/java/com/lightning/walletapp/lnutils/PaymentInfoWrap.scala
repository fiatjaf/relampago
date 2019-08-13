package com.lightning.walletapp.lnutils

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.ln.NormalChannel._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import rx.lang.scala.{Observable => Obs}
import com.lightning.walletapp.helper.{AES, RichCursor}
import fr.acinq.bitcoin.{Crypto, MilliSatoshi, Transaction}
import com.lightning.walletapp.lnutils.olympus.{CerberusAct, OlympusWrap}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.cerberusPayloadCodec
import com.lightning.walletapp.ln.wire.LightningMessageCodecs
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import com.lightning.walletapp.ln.crypto.Sphinx.PublicKeyVec
import com.lightning.walletapp.ChannelManager
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.walletapp.Utils.app
import scodec.bits.ByteVector


object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  private[this] var unsentPayments = Map.empty[ByteVector, RoutingData]
  var acceptedPayments = Map.empty[ByteVector, RoutingData]

  def addPendingPayment(rd: RoutingData) = {
    // Add payment to unsentPayments and try to resolve it later
    unsentPayments = unsentPayments.updated(rd.pr.paymentHash, rd)
    me insertOrUpdateOutgoingPayment rd
    resolvePending
    uiNotify
  }

  def resolvePending =
    if (ChannelManager.currentBlocksLeft.isDefined) {
      // When uncapable chan becomes online: persists, waits for capable channel
      // When no routes found or any other error happens: gets removed in failOnUI
      // When accepted by channel: gets removed in outPaymentAccepted
      unsentPayments.values foreach fetchAndSend
    }

  def extractPreimage(candidateTx: Transaction) = {
    val fulfills = candidateTx.txIn.map(_.witness.stack) collect {
      case Seq(_, pre, _) if pre.size == 32 => UpdateFulfillHtlc(NOCHANID, 0L, pre)
      case Seq(_, _, _, pre, _) if pre.size == 32 => UpdateFulfillHtlc(NOCHANID, 0L, pre)
    }

    fulfills foreach updOkOutgoing
    if (fulfills.nonEmpty) uiNotify
  }

  def fetchAndSend(rd: RoutingData) = ChannelManager.fetchRoutes(rd).foreach(ChannelManager.sendEither(_, failOnUI), anyError => me failOnUI rd)
  def updOkIncoming(m: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, m.amountMsat, System.currentTimeMillis, m.channelId, m.paymentHash)
  def updOkOutgoing(m: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, m.paymentPreimage, m.channelId, m.paymentHash)
  def getPaymentInfo(hash: ByteVector) = RichCursor apply db.select(PaymentTable.selectSql, hash) headTry toPaymentInfo
  def updStatus(status: Int, hash: ByteVector) = db.change(PaymentTable.updStatusSql, status, hash)
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byRecent = db select PaymentTable.selectRecentSql

  def byQuery(rawQuery: String) = {
    val refinedQuery = rawQuery.replaceAll("[^ a-zA-Z0-9]", "")
    db.select(PaymentTable.searchSql, s"$refinedQuery*")
  }

  def toPaymentInfo(rc: RichCursor) =
    PaymentInfo(rawPr = rc string PaymentTable.pr, preimage = ByteVector.fromValidHex(rc string PaymentTable.preimage),
      rc int PaymentTable.incoming, rc int PaymentTable.status, rc long PaymentTable.stamp, rc string PaymentTable.description,
      rc long PaymentTable.firstMsat, rc long PaymentTable.lastMsat, rc long PaymentTable.lastExpiry)

  def insertOrUpdateOutgoingPayment(rd: RoutingData) = db txWrap {
    db.change(PaymentTable.updLastParamsSql, rd.firstMsat, rd.lastMsat, rd.lastExpiry, rd.pr.paymentHash)
    db.change(PaymentTable.newSql, rd.pr.toJson, NOIMAGE, 0 /* this is outgoing payment */, WAITING,
      System.currentTimeMillis, rd.pr.description, rd.pr.paymentHash, rd.firstMsat, rd.lastMsat,
      rd.lastExpiry, NOCHANID)
  }

  def recordRoutingDataWithPr(extraRoutes: Vector[PaymentRoute], sum: MilliSatoshi, preimage: ByteVector, description: String): RoutingData = {
    val pr = PaymentRequest(chainHash, amount = Some(sum), Crypto sha256 preimage, nodePrivateKey, description, fallbackAddress = None, extraRoutes)
    val rd = emptyRD(pr, sum.amount, useCache = true, airLeft = 0)

    db.change(PaymentTable.newVirtualSql, rd.queryText, pr.paymentHash)
    db.change(PaymentTable.newSql, pr.toJson, preimage, 1 /* this is incoming payment */, WAITING,
      System.currentTimeMillis, pr.description, pr.paymentHash, sum.amount, 0L /* lastMsat */,
      0L /* lastExpiry, will be updated on incoming for reflexive payments */, NOCHANID)

    uiNotify
    rd
  }

  def markFailedPayments = db txWrap {
    db.change(PaymentTable.updFailWaitingSql, System.currentTimeMillis - PaymentRequest.expiryTag.seconds * 1000L)
    for (activeInFlightHash <- ChannelManager.activeInFlightHashes) updStatus(WAITING, activeInFlightHash)
  }

  def failOnUI(rd: RoutingData) = {
    unsentPayments = unsentPayments - rd.pr.paymentHash
    updStatus(FAILURE, rd.pr.paymentHash)
    uiNotify
  }

  override def outPaymentAccepted(rd: RoutingData) = {
    acceptedPayments = acceptedPayments.updated(rd.pr.paymentHash, rd)
    unsentPayments = unsentPayments - rd.pr.paymentHash
    me insertOrUpdateOutgoingPayment rd
  }

  override def fulfillReceived(ok: UpdateFulfillHtlc) = db txWrap {
    // Save preimage right away, don't wait for their next commitSig
    me updOkOutgoing ok

    acceptedPayments get ok.paymentHash foreach { rd =>
      val isFeeLow = !isFeeBreach(rd.usedRoute, rd.firstMsat, divider = 1000L)
      db.change(PaymentTable.newVirtualSql, rd.queryText, rd.pr.paymentHash)
      if (rd.usedRoute.nonEmpty && isFeeLow) RouteWrap cacheSubRoutes rd
    }
  }

  override def onSettled(cs: Commitments) = {
    def newRoutesOrGiveUp(rd: RoutingData): Unit =
      if (rd.callsLeft > 0 && ChannelManager.checkIfSendable(rd).isRight) {
        // We don't care about AIR or multipart here, supposedly it's one of them
        me fetchAndSend rd.copy(callsLeft = rd.callsLeft - 1, useCache = false)
      } else updStatus(FAILURE, rd.pr.paymentHash)

    db txWrap {
      cs.localSpec.fulfilledIncoming foreach updOkIncoming
      // Malformed payments are returned by our direct peer and should never be retried again
      for (Htlc(false, add) <- cs.localSpec.malformed) updStatus(FAILURE, add.paymentHash)
      for (Htlc(false, add) \ failReason <- cs.localSpec.failed) {

        val rdOpt = acceptedPayments get add.paymentHash
        rdOpt map parseFailureCutRoutes(failReason) match {
          // Try to use reamining routes or fetch new ones if empty
          // but account for possibility of rd not being in place

          case Some(Some(rd1) \ excludes) =>
            for (badEntity <- excludes) BadEntityWrap.putEntity.tupled(badEntity)
            ChannelManager.sendEither(useFirstRoute(rd1.routes, rd1), newRoutesOrGiveUp)

          case _ =>
            // May happen after app has been restarted
            // also when someone sends an unparsable error
            updStatus(FAILURE, add.paymentHash)
        }
      }
    }

    uiNotify
    if (cs.localSpec.fulfilled.nonEmpty) com.lightning.walletapp.Vibrator.vibrate
    if (cs.localSpec.fulfilledOutgoing.nonEmpty) app.olympus.tellClouds(OlympusWrap.CMDStart)
    if (cs.localSpec.fulfilledIncoming.nonEmpty) getVulnerableRevActs.foreach(app.olympus.tellClouds)
  }

  def getVulnerableRevActs = {
    val operational = ChannelManager.all.filter(isOperational)
    getCerberusActs(operational.flatMap(getVulnerableRevVec).toMap)
  }

  def getVulnerableRevVec(chan: Channel) =
    chan.getCommits collect { case nc: NormalCommits =>
      // Find previous channel states which peer might now be tempted to spend
      val threshold = nc.remoteCommit.spec.toRemoteMsat - dust.amount * 20 * 1000L
      def toTxidAndInfo(rc: RichCursor) = Tuple2(rc string RevokedInfoTable.txId, rc string RevokedInfoTable.info)
      RichCursor apply db.select(RevokedInfoTable.selectLocalSql, nc.channelId, threshold) vec toTxidAndInfo
    } getOrElse Vector.empty

  def getCerberusActs(infos: Map[String, String] = Map.empty) = {
    // Remove currently pending infos and limit max number of uploads
    val notPendingInfos = infos -- app.olympus.pendingWatchTxIds take 100

    val encrypted = for {
      txid \ revInfo <- notPendingInfos
      txidBytes = ByteVector.fromValidHex(txid).toArray
      revInfoBytes = ByteVector.fromValidHex(revInfo).toArray
      enc = AES.encBytes(revInfoBytes, txidBytes)
    } yield txid -> enc

    for {
      pack <- encrypted grouped 20
      txids \ zygotePayloads = pack.unzip
      halfTxIds = for (txid <- txids) yield txid take 16
      cp = CerberusPayload(zygotePayloads.toVector, halfTxIds.toVector)
      bin = LightningMessageCodecs.serialize(cerberusPayloadCodec encode cp)
    } yield CerberusAct(bin, Nil, "cerberus/watch", txids.toVector)
  }

  override def onProcessSuccess = {
    case (_: NormalChannel, wbr: WaitBroadcastRemoteData, CMDChainTipKnown) if wbr.isLost =>
      // We don't allow manual deletion here as funding may just not be locally visible yet
      app.kit.wallet.removeWatchedScripts(app.kit fundingPubScript wbr)
      db.change(ChannelTable.killSql, wbr.commitments.channelId)

    case (_: NormalChannel, close: ClosingData, CMDChainTipKnown) if close.canBeRemoved =>
      // Either a lot of time has passed or ALL closing transactions have enough confirmations
      app.kit.wallet.removeWatchedScripts(app.kit closingPubKeyScripts close)
      app.kit.wallet.removeWatchedScripts(app.kit fundingPubScript close)
      db.change(RevokedInfoTable.killSql, close.commitments.channelId)
      db.change(ChannelTable.killSql, close.commitments.channelId)
  }

  override def onBecome = {
    case (_, _, SLEEPING, OPEN) =>
      // We may have some payments waiting
      resolvePending

    case (_, _, WAIT_FUNDING_DONE, OPEN) =>
      // We may have a channel upload act waiting
      app.olympus tellClouds OlympusWrap.CMDStart
  }
}

object ChannelWrap {
  def doPut(chanId: ByteVector, data: String) = db txWrap {
    // Insert and then update because of INSERT IGNORE effects
    db.change(ChannelTable.newSql, chanId, data)
    db.change(ChannelTable.updSql, data, chanId)
  }

  def put(data: ChannelData) = data match {
    case hasNorm: HasNormalCommits => doPut(hasNorm.commitments.channelId, '1' + hasNorm.toJson.toString)
    case hostedCommits: HostedCommits => doPut(hostedCommits.channelId, '2' + hostedCommits.toJson.toString)
    case otherwise => throw new RuntimeException(s"Can not presist this channel data type: $otherwise")
  }

  def doGet(database: LNOpenHelper) =
    RichCursor(database select ChannelTable.selectAllSql).vec(_ string ChannelTable.data) map {
      case rawChanData if '1' == rawChanData.head => to[HasNormalCommits](rawChanData substring 1)
      case rawChanData if '2' == rawChanData.head => to[HostedCommits](rawChanData substring 1)
      case otherwise => throw new RuntimeException(s"Can not deserialize: $otherwise")
    }
}

object RouteWrap {
  def cacheSubRoutes(rd: RoutingData) = {
    // This will only work if we have at least one hop, should check if route vector is empty
    // then merge each of generated subroutes with a respected routing node or recipient node key
    val subs = (rd.usedRoute drop 1).scanLeft(rd.usedRoute take 1) { case rs \ hop => rs :+ hop }

    for (_ \ node \ path <- rd.onion.sharedSecrets drop 1 zip subs) {
      val pathJson \ nodeString = path.toJson.toString -> node.toString
      db.change(RouteTable.newSql, pathJson, nodeString)
      db.change(RouteTable.updSql, pathJson, nodeString)
    }
  }

  def findRoutes(from: PublicKeyVec, targetId: PublicKey, rd: RoutingData) = {
    // Cached routes never expire, but local channels might be closed or excluded
    // so make sure we still have a matching channel for retrieved cached route

    val cursor = db.select(RouteTable.selectSql, targetId)
    val routeTry = RichCursor(cursor).headTry(_ string RouteTable.path) map to[PaymentRoute]
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

    val targetStr = targetId.toString
    // Remove source and sink nodes from badNodes because they may have been blacklisted earlier
    val fromAsStr = for (oneOfDirectPeerNodeKeys: PublicKey <- from.toSet) yield oneOfDirectPeerNodeKeys.toString
    val badChanLongs = for (publicChannelDeniedByRoutingNode: String <- badChans) yield publicChannelDeniedByRoutingNode.toLong
    app.olympus findRoutes OutRequest(rd.firstMsat / 1000L, badNodes - targetStr -- fromAsStr, badChanLongs, fromAsStr, targetStr)
  }
}