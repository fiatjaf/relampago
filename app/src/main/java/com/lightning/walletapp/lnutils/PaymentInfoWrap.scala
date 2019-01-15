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

import rx.lang.scala.{Observable => Obs}
import fr.acinq.bitcoin.{BinaryData, Transaction}
import com.lightning.walletapp.helper.{AES, RichCursor}
import com.lightning.walletapp.lnutils.olympus.{CerberusAct, OlympusWrap}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.cerberusPayloadCodec
import com.lightning.walletapp.ln.wire.LightningMessageCodecs
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import com.lightning.walletapp.ln.crypto.Sphinx.PublicKeyVec
import com.lightning.walletapp.ChannelManager
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.walletapp.Utils.app


object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  var inFlightPayments = Map.empty[BinaryData, RoutingData]
  var unsentPayments = Map.empty[BinaryData, RoutingData]

  def addPendingPayment(rd: RoutingData) = {
    // Add payment to unsentPayments and try to resolve it later
    unsentPayments = unsentPayments.updated(rd.pr.paymentHash, rd)
    me insertOrUpdateOutgoingPayment rd
    resolvePending
    uiNotify
  }

  def resolvePending =
    if (app.kit.peerGroup.numConnectedPeers > 0)
      if (ChannelManager.currentBlocksLeft < Int.MaxValue)
        unsentPayments.values foreach fetchAndSend

  def extractPreimage(candidateTx: Transaction) = {
    val fulfills = candidateTx.txIn.map(_.witness.stack) collect {
      case Seq(_, pre, _) if pre.size == 32 => UpdateFulfillHtlc(NOCHANID, 0L, pre)
      case Seq(_, _, _, pre, _) if pre.size == 32 => UpdateFulfillHtlc(NOCHANID, 0L, pre)
    }

    fulfills foreach updOkOutgoing
    if (fulfills.nonEmpty) uiNotify
  }

  def fetchAndSend(rd: RoutingData) = ChannelManager.fetchRoutes(rd).foreach(ChannelManager.sendEither(_, failOnUI), olympusErr => me failOnUI rd)
  def updOkIncoming(m: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, m.amountMsat, System.currentTimeMillis, m.channelId, m.paymentHash)
  def updOkOutgoing(m: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, m.paymentPreimage, m.channelId, m.paymentHash)
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(PaymentTable.selectSql, hash) headTry toPaymentInfo
  def updStatus(status: Int, hash: BinaryData) = db.change(PaymentTable.updStatusSql, status, hash)
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byQuery(query: String) = db.select(PaymentTable.searchSql, s"$query*")
  def byRecent = db select PaymentTable.selectRecentSql

  def toPaymentInfo(rc: RichCursor) =
    PaymentInfo(rc string PaymentTable.pr, rc string PaymentTable.preimage, rc int PaymentTable.incoming, rc int PaymentTable.status,
      rc long PaymentTable.stamp, rc string PaymentTable.description, rc string PaymentTable.hash, rc long PaymentTable.firstMsat,
      rc long PaymentTable.lastMsat, rc long PaymentTable.lastExpiry)

  def insertOrUpdateOutgoingPayment(rd: RoutingData) = db txWrap {
    db.change(PaymentTable.updLastParamsSql, rd.firstMsat, rd.lastMsat, rd.lastExpiry, rd.pr.paymentHash)
    db.change(PaymentTable.newSql, rd.pr.toJson, NOIMAGE, 0, WAITING, System.currentTimeMillis, rd.pr.description,
      rd.pr.paymentHash, rd.firstMsat, rd.lastMsat, rd.lastExpiry, NOCHANID)
  }

  def markFailedAndFrozen = db txWrap {
    db change PaymentTable.updFailWaitingAndFrozenSql
    for (hash <- ChannelManager.activeInFlightHashes) updStatus(WAITING, hash)
    for (hash <- ChannelManager.frozenInFlightHashes) updStatus(FROZEN, hash)
  }

  def failOnUI(rd: RoutingData) = {
    // Fail this payment on UI and remove from unsent
    // it may still be in unsent if no routes were found
    unsentPayments = unsentPayments - rd.pr.paymentHash
    updStatus(FAILURE, rd.pr.paymentHash)
    uiNotify
  }

  override def outPaymentAccepted(rd: RoutingData) = {
    me insertOrUpdateOutgoingPayment rd
    inFlightPayments = inFlightPayments.updated(rd.pr.paymentHash, rd)
    unsentPayments = unsentPayments - rd.pr.paymentHash
  }

  override def fulfillReceived(ok: UpdateFulfillHtlc) = db txWrap {
    // Save preimage right away, don't wait for their next commitSig
    me updOkOutgoing ok

    inFlightPayments get ok.paymentHash foreach { rd =>
      // Make payment searchable + optimization: record sub-routes in database
      db.change(PaymentTable.newVirtualSql, rd.queryText, rd.pr.paymentHash)
      if (rd.usedRoute.nonEmpty) RouteWrap cacheSubRoutes rd
    }
  }

  override def settled(cs: Commitments) = {
    // Update affected record states in a database, then retry failed payments where possible
    val fulfilledIncoming = for (Htlc(true, add) \ _ <- cs.localCommit.spec.fulfilled) yield add

    def newRoutes(rd: RoutingData) = {
      // UI will be updated upstream if we can't re-send any more
      val stillCanReSend = rd.callsLeft > 0 && ChannelManager.checkIfSendable(rd).isRight
      if (stillCanReSend) me fetchAndSend rd.copy(callsLeft = rd.callsLeft - 1, useCache = false)
      else updStatus(FAILURE, rd.pr.paymentHash)
    }

    db txWrap {
      fulfilledIncoming foreach updOkIncoming
      // Malformed payments are returned by our direct peer and should never be retried again
      for (Htlc(false, add) <- cs.localCommit.spec.malformed) updStatus(FAILURE, add.paymentHash)
      for (Htlc(false, add) \ failReason <- cs.localCommit.spec.failed) {

        val rdOpt = inFlightPayments get add.paymentHash
        rdOpt map parseFailureCutRoutes(failReason) match {
          // Try to use the routes left or fetch new ones if empty
          // but account for possibility of rd not being in place

          case Some(Some(rd1) \ excludes) =>
            for (entity <- excludes) BadEntityWrap.putEntity tupled entity
            ChannelManager.sendEither(useFirstRoute(rd1.routes, rd1), newRoutes)

          case _ =>
            // May happen after app restart
            // also when recipient sends an error
            updStatus(FAILURE, add.paymentHash)
        }
      }
    }

    uiNotify
    if (cs.localCommit.spec.fulfilled.nonEmpty) {
      // Let the clouds know since they may be waiting
      // also vibrate to let a user know it's fulfilled
      app.olympus tellClouds OlympusWrap.CMDStart
      com.lightning.walletapp.Vibrator.vibrate
    }

    if (fulfilledIncoming.nonEmpty) {
      // Collect vulnerable infos from ALL currently active channels on incoming payment
      val infos = ChannelManager.notClosingOrRefunding.flatMap(getVulnerableRevInfos)
      getCerberusActs(infos.toMap) foreach app.olympus.tellClouds
    }
  }

  def getVulnerableRevInfos(chan: Channel) = chan.hasCsOr(some => {
    // Find previous channel states which peer might be now tempted to spend
    val threshold = some.commitments.localCommit.spec.toLocalMsat - dust.amount * 4 * 1000L
    def toTxidAndInfo(rc: RichCursor) = Tuple2(rc string RevokedInfoTable.txId, rc string RevokedInfoTable.info)
    RichCursor apply db.select(RevokedInfoTable.selectLocalSql, some.commitments.channelId, threshold) vec toTxidAndInfo
  }, Vector.empty)

  type TxIdAndRevInfoMap = Map[String, String]
  def getCerberusActs(infos: TxIdAndRevInfoMap) = {
    // Remove currently pending infos and limit max number of uploads
    val notPendingInfos = infos -- app.olympus.pendingWatchTxIds take 100

    val encrypted = for {
      txid \ revInfo <- notPendingInfos
      txidBytes = BinaryData(txid).toArray
      revInfoBytes = BinaryData(revInfo).toArray
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
    case (_, wbr: WaitBroadcastRemoteData, _: CMDBestHeight) if wbr.isFailed =>
      app.kit.wallet.removeWatchedScripts(app.kit fundingPubScript wbr)
      db.change(ChannelTable.killSql, wbr.commitments.channelId)

    case (_, close: ClosingData, _: CMDBestHeight) if close.isOutdated =>
      app.kit.wallet.removeWatchedScripts(app.kit closingPubKeyScripts close)
      app.kit.wallet.removeWatchedScripts(app.kit fundingPubScript close)
      db.change(RevokedInfoTable.killSql, close.commitments.channelId)
      db.change(ChannelTable.killSql, close.commitments.channelId)
  }

  override def onBecome = {
    case (_, _, SLEEPING, OPEN) => resolvePending
    case (_, _, WAIT_FUNDING_DONE, OPEN) => app.olympus tellClouds OlympusWrap.CMDStart
    case (_, _, from, CLOSING) if from != CLOSING => runAnd(markFailedAndFrozen)(uiNotify)
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

  def doGet(db1: LNOpenHelper) = {
    val rc = RichCursor(db1 select ChannelTable.selectAllSql)
    rc.vec(_ string ChannelTable.data substring 1) map to[HasCommitments]
  }
}

object RouteWrap {
  def cacheSubRoutes(rd: RoutingData) = {
    // This will only work if we have at least one hop, should check if route vector is empty
    // then merge each of generated subroutes with a respected routing node or recipient node key
    val subs = (rd.usedRoute drop 1).scanLeft(rd.usedRoute take 1) { case rs \ hop => rs :+ hop }

    for (_ \ node \ path <- rd.onion.sharedSecrets drop 1 zip subs) {
      val expiration = System.currentTimeMillis + 1000L * 3600 * 24 * 14
      val subPathJson \ subNodeString = path.toJson.toString -> node.toString
      db.change(RouteTable.newSql, subPathJson, subNodeString, expiration)
      db.change(RouteTable.updSql, subPathJson, expiration, subNodeString)
    }
  }

  def findRoutes(from: PublicKeyVec, targetId: PublicKey, rd: RoutingData) = {
    val cursor = db.select(RouteTable.selectSql, targetId, System.currentTimeMillis)
    val routeTry = RichCursor(cursor).headTry(_ string RouteTable.path) map to[PaymentRoute]
    // Channels could be closed or excluded so make sure we still have a matching channel for cached route
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

    val fromAsString = from.map(_.toString).toSet
    // One of blacklisted nodes may become our peer or final payee
    val filteredBadNodes = badNodes - targetId.toString -- fromAsString
    app.olympus findRoutes OutRequest(rd.firstMsat / 1000L, filteredBadNodes,
      badChans.map(_.toLong), fromAsString, targetId.toString)
  }
}

object GossipCatcher extends ChannelListener {
  // Catch ChannelUpdate to enable funds receiving

  override def onProcessSuccess = {
    case (chan, norm: NormalData, _: CMDBestHeight) if norm.commitments.extraHop.isEmpty =>
      // Stage 1: we don't have any hop at all so at this point we need to obtain a shortChannelId

      for {
        blockHeight \ txIndex <- app.olympus.getShortId(Commitments fundingTxid norm.commitments)
        shortChannelId <- Tools.toShortIdOpt(blockHeight, txIndex, outputIndex = norm.commitments.commitInput.outPoint.index)
        hop = Hop(chan.data.announce.nodeId, shortChannelId, cltvExpiryDelta = 0, 0L, 0L, feeProportionalMillionths = 0L)
      } chan process hop

    case (chan, norm: NormalData, _: CMDBestHeight)
      // Stage 2: we have a dummy hop with real shortChannelId, obtain parameters
      if norm.commitments.extraHop.nonEmpty && channelAndHop(chan).isEmpty =>

      for {
        dummyExtraHop <- norm.commitments.extraHop
        ChannelUpdate(_, _, _, _, _, _, cltv, min, base, prop, _) <- app.olympus.findUpdate(chan.data.announce.nodeId)
        hop = dummyExtraHop.copy(cltvExpiryDelta = cltv, htlcMinimumMsat = min, feeBaseMsat = base, feeProportionalMillionths = prop)
      } chan process hop

    case (chan, norm: NormalData, upd: ChannelUpdate)
      // Stage 3: we have an old or empty Hop, replace it with (peer -> localPhone) hop
      if norm.commitments.extraHop.exists(_.shortChannelId == upd.shortChannelId) =>

      val newHop = upd toHop chan.data.announce.nodeId
      val isCopy = norm.commitments.extraHop contains newHop
      if (!isCopy) chan process newHop
      chan.listeners -= GossipCatcher
  }
}