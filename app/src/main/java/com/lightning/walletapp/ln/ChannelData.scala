package com.lightning.walletapp.ln

import fr.acinq.bitcoin.Crypto._
import com.softwaremill.quicklens._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Scripts._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.AddErrorCodes._
import com.lightning.walletapp.ln.LNParams.broadcaster._
import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}
import com.lightning.walletapp.ln.CommitmentSpec.{HtlcAndFail, HtlcAndFulfill}
import com.lightning.walletapp.ln.Helpers.Closing.{SuccessAndClaim, TimeoutAndClaim}
import com.lightning.walletapp.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.LNMessageVector
import org.bitcoinj.wallet.SendRequest
import fr.acinq.eclair.UInt64
import language.postfixOps


sealed trait Command
case class CMDConfirmed(tx: Transaction) extends Command
case class CMDBestHeight(height: Long) extends Command
case class CMDFunding(tx: Transaction) extends Command
case class CMDSpent(tx: Transaction) extends Command
case class CMDFeerate(sat: Long) extends Command
case object CMDHTLCProcess extends Command
case object CMDForceClose extends Command
case object CMDShutdown extends Command
case object CMDOffline extends Command
case object CMDProceed extends Command
case object CMDOnline extends Command

case class CMDOpenChannel(localParams: LocalParams, tempChanId: BinaryData, initialFeeratePerKw: Long,
                          pushMsat: Long, remoteInit: Init, dummyRequest: SendRequest, outIndex: Int,
                          realFundingAmountSat: Long) extends Command

case class CMDFailMalformedHtlc(id: Long, onionHash: BinaryData, code: Int) extends Command
case class CMDFulfillHtlc(id: Long, preimage: BinaryData) extends Command
case class CMDFailHtlc(id: Long, reason: BinaryData) extends Command

// CHANNEL DATA

sealed trait ChannelData { val announce: NodeAnnouncement }
sealed trait HasCommitments extends ChannelData { val commitments: Commitments }

case class InitData(announce: NodeAnnouncement) extends ChannelData
case class WaitAcceptData(announce: NodeAnnouncement, cmd: CMDOpenChannel) extends ChannelData
case class WaitFundingData(announce: NodeAnnouncement, cmd: CMDOpenChannel, accept: AcceptChannel) extends ChannelData

case class WaitFundingSignedData(announce: NodeAnnouncement, localParams: LocalParams, channelId: BinaryData,
                                 remoteParams: AcceptChannel, fundingTx: Transaction, localSpec: CommitmentSpec,
                                 localCommitTx: CommitTx, remoteCommit: RemoteCommit) extends ChannelData

// All the data below will be stored
case class WaitFundingDoneData(announce: NodeAnnouncement, our: Option[FundingLocked],
                               their: Option[FundingLocked], fundingTx: Transaction,
                               commitments: Commitments) extends HasCommitments

case class NormalData(announce: NodeAnnouncement,
                      commitments: Commitments, localShutdown: Option[Shutdown] = None,
                      remoteShutdown: Option[Shutdown] = None) extends HasCommitments

case class ClosingTxProposed(unsignedTx: ClosingTx, localClosingSigned: ClosingSigned)
case class NegotiationsData(announce: NodeAnnouncement, commitments: Commitments, localShutdown: Shutdown, remoteShutdown: Shutdown,
                            localProposals: Seq[ClosingTxProposed], lastSignedTx: Option[ClosingTx] = None) extends HasCommitments

case class RefundingData(announce: NodeAnnouncement, remoteLatestPoint: Option[Point],
                         commitments: Commitments) extends HasCommitments

case class ClosingData(announce: NodeAnnouncement,
                       commitments: Commitments, localProposals: Seq[ClosingTxProposed] = Nil,
                       mutualClose: Seq[Transaction] = Nil, localCommit: Seq[LocalCommitPublished] = Nil,
                       remoteCommit: Seq[RemoteCommitPublished] = Nil, nextRemoteCommit: Seq[RemoteCommitPublished] = Nil,
                       refundRemoteCommit: Seq[RemoteCommitPublished] = Nil, revokedCommit: Seq[RevokedCommitPublished] = Nil,
                       closedAt: Long = System.currentTimeMillis) extends HasCommitments {

  def tier12States =
    revokedCommit.flatMap(_.getState) ++ localCommit.flatMap(_.getState) ++
      remoteCommit.flatMap(_.getState) ++ nextRemoteCommit.flatMap(_.getState) ++
      refundRemoteCommit.flatMap(_.getState)

  def bestClosing = {
    val allClosings = mutualClose.map(Left.apply) ++
      revokedCommit.map(Right.apply) ++ localCommit.map(Right.apply) ++
        remoteCommit.map(Right.apply) ++ nextRemoteCommit.map(Right.apply) ++
        refundRemoteCommit.map(Right.apply)

    allClosings maxBy {
      case Left(mutualTx) => getStatus(mutualTx.txid) match { case cfs \ _ => cfs }
      case Right(info) => getStatus(info.commitTx.txid) match { case cfs \ _ => cfs }
    }
  }

  def isOutdated = {
    val allConfirmedOrDead = bestClosing match {
      case Left(mutualClosingTx) => getStatus(mutualClosingTx.txid) match { case cfs \ isDead => cfs > minDepth || isDead }
      case Right(info) => info.getState.map(_.txn.txid) map getStatus forall { case cfs \ isDead => cfs > minDepth || isDead }
    }

    // Either everything including tier12 tx is confirmed or hard timeout has passed
    val hardTimeout = closedAt + 1000 * 3600 * 24 * 90 < System.currentTimeMillis
    allConfirmedOrDead || hardTimeout
  }
}

sealed trait CommitPublished {
  def getState: Seq[PublishStatus]
  val commitTx: Transaction
}

case class LocalCommitPublished(claimMainDelayed: Seq[ClaimDelayedOutputTx], claimHtlcSuccess: Seq[SuccessAndClaim],
                                claimHtlcTimeout: Seq[TimeoutAndClaim], commitTx: Transaction) extends CommitPublished {

  def getState = {
    val success = for (t1 \ t2 <- claimHtlcSuccess) yield HideReady(t1.tx) :: csvShowDelayed(t1, t2) :: Nil
    val timeout = for (t1 \ t2 <- claimHtlcTimeout) yield HideDelayed(cltv(commitTx, t1.tx), t1.tx) :: csvShowDelayed(t1, t2) :: Nil
    val main = for (t1 <- claimMainDelayed) yield ShowDelayed(csv(commitTx, t1.tx), t1.tx, t1 -- t1, t1.tx.allOutputsAmount) :: Nil
    main.flatten ++ success.flatten ++ timeout.flatten
  }
}

case class RemoteCommitPublished(claimMain: Seq[ClaimP2WPKHOutputTx], claimHtlcSuccess: Seq[ClaimHtlcSuccessTx],
                                 claimHtlcTimeout: Seq[ClaimHtlcTimeoutTx], commitTx: Transaction) extends CommitPublished {

  def getState = {
    val timeout = for (t1 <- claimHtlcTimeout) yield ShowDelayed(cltv(commitTx, t1.tx), t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    val success = for (t1 <- claimHtlcSuccess) yield ShowReady(t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    val main = for (t1 <- claimMain) yield ShowReady(t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    main ++ success ++ timeout
  }
}

case class RevokedCommitPublished(claimMain: Seq[ClaimP2WPKHOutputTx], claimTheirMainPenalty: Seq[MainPenaltyTx],
                                  htlcPenalty: Seq[HtlcPenaltyTx], commitTx: Transaction) extends CommitPublished {

  def getState = {
    val main = for (t1 <- claimMain) yield ShowReady(t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    val their = for (t1 <- claimTheirMainPenalty) yield ShowReady(t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    val penalty = for (t1 <- htlcPenalty) yield ShowReady(t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    main ++ their ++ penalty
  }
}

// COMMITMENTS

case class Htlc(incoming: Boolean, add: UpdateAddHtlc)
case class CommitmentSpec(feeratePerKw: Long, toLocalMsat: Long, toRemoteMsat: Long,
                          htlcs: Set[Htlc] = Set.empty, fulfilled: Set[HtlcAndFulfill] = Set.empty,
                          failed: Set[HtlcAndFail] = Set.empty, malformed: Set[Htlc] = Set.empty)

object CommitmentSpec {
  def findHtlcById(cs: CommitmentSpec, id: Long, isIncoming: Boolean): Option[Htlc] =
    cs.htlcs.find(htlc => htlc.add.id == id && htlc.incoming == isIncoming)

  type HtlcAndFulfill = (Htlc, UpdateFulfillHtlc)
  def fulfill(cs: CommitmentSpec, isIncoming: Boolean, m: UpdateFulfillHtlc) = findHtlcById(cs, m.id, isIncoming) match {
    case Some(h) if h.incoming => cs.copy(toLocalMsat = cs.toLocalMsat + h.add.amountMsat, fulfilled = cs.fulfilled + Tuple2(h, m), htlcs = cs.htlcs - h)
    case Some(h) => cs.copy(toRemoteMsat = cs.toRemoteMsat + h.add.amountMsat, fulfilled = cs.fulfilled + Tuple2(h, m), htlcs = cs.htlcs - h)
    case None => cs
  }

  type HtlcAndFail = (Htlc, UpdateFailHtlc)
  def fail(cs: CommitmentSpec, isIncoming: Boolean, m: UpdateFailHtlc) = findHtlcById(cs, m.id, isIncoming) match {
    case Some(h) if h.incoming => cs.copy(toRemoteMsat = cs.toRemoteMsat + h.add.amountMsat, failed = cs.failed + Tuple2(h, m), htlcs = cs.htlcs - h)
    case Some(h) => cs.copy(toLocalMsat = cs.toLocalMsat + h.add.amountMsat, failed = cs.failed + Tuple2(h, m), htlcs = cs.htlcs - h)
    case None => cs
  }

  def failMalformed(cs: CommitmentSpec, isIncoming: Boolean, m: UpdateFailMalformedHtlc) = findHtlcById(cs, m.id, isIncoming) match {
    case Some(h) if h.incoming => cs.copy(toRemoteMsat = cs.toRemoteMsat + h.add.amountMsat, malformed = cs.malformed + h, htlcs = cs.htlcs - h)
    case Some(h) => cs.copy(toLocalMsat = cs.toLocalMsat + h.add.amountMsat, malformed = cs.malformed + h, htlcs = cs.htlcs - h)
    case None => cs
  }

  def plusOutgoing(data: UpdateAddHtlc, cs: CommitmentSpec) =
    cs.copy(htlcs = cs.htlcs + Htlc(incoming = false, add = data),
      toLocalMsat = cs.toLocalMsat - data.amountMsat)

  def plusIncoming(data: UpdateAddHtlc, cs: CommitmentSpec) =
    cs.copy(htlcs = cs.htlcs + Htlc(incoming = true, add = data),
      toRemoteMsat = cs.toRemoteMsat - data.amountMsat)

  def reduce(cs: CommitmentSpec, local: LNMessageVector, remote: LNMessageVector) = {
    val spec1 = cs.copy(fulfilled = Set.empty, failed = Set.empty, malformed = Set.empty)
    val spec2 = (spec1 /: local) { case (s, add: UpdateAddHtlc) => plusOutgoing(add, s) case s \ _ => s }
    val spec3 = (spec2 /: remote) { case (s, add: UpdateAddHtlc) => plusIncoming(add, s) case s \ _ => s }

    val spec4 = (spec3 /: local) {
      case (s, msg: UpdateFee) => s.copy(feeratePerKw = msg.feeratePerKw)
      case (s, msg: UpdateFulfillHtlc) => fulfill(s, isIncoming = true, msg)
      case (s, msg: UpdateFailMalformedHtlc) => failMalformed(s, isIncoming = true, msg)
      case (s, msg: UpdateFailHtlc) => fail(s, isIncoming = true, msg)
      case s \ _ => s
    }

    (spec4 /: remote) {
      case (s, msg: UpdateFee) => s.copy(feeratePerKw = msg.feeratePerKw)
      case (s, msg: UpdateFulfillHtlc) => fulfill(s, isIncoming = false, msg)
      case (s, msg: UpdateFailMalformedHtlc) => failMalformed(s, isIncoming = false, msg)
      case (s, msg: UpdateFailHtlc) => fail(s, isIncoming = false, msg)
      case s \ _ => s
    }
  }
}

case class LocalParams(maxHtlcValueInFlightMsat: UInt64, channelReserveSat: Long, toSelfDelay: Int,
                       maxAcceptedHtlcs: Int, fundingPrivKey: PrivateKey, revocationSecret: Scalar,
                       paymentKey: Scalar, delayedPaymentKey: Scalar, htlcKey: Scalar,
                       defaultFinalScriptPubKey: BinaryData, dustLimit: Satoshi,
                       shaSeed: BinaryData, isFunder: Boolean) {

  lazy val delayedPaymentBasepoint = delayedPaymentKey.toPoint
  lazy val revocationBasepoint = revocationSecret.toPoint
  lazy val paymentBasepoint = paymentKey.toPoint
  lazy val htlcBasepoint = htlcKey.toPoint
}

case class WaitingForRevocation(nextRemoteCommit: RemoteCommit, sent: CommitSig, localCommitIndexSnapshot: Long)
case class LocalCommit(index: Long, spec: CommitmentSpec, htlcTxsAndSigs: Seq[HtlcTxAndSigs], commitTx: CommitTx)
case class RemoteCommit(index: Long, spec: CommitmentSpec, txid: BinaryData, remotePerCommitmentPoint: Point)
case class HtlcTxAndSigs(txinfo: TransactionWithInputInfo, localSig: BinaryData, remoteSig: BinaryData)
case class Changes(proposed: LNMessageVector, signed: LNMessageVector, acked: LNMessageVector)

case class Commitments(localParams: LocalParams, remoteParams: AcceptChannel, localCommit: LocalCommit,
                       remoteCommit: RemoteCommit, localChanges: Changes, remoteChanges: Changes, localNextHtlcId: Long,
                       remoteNextHtlcId: Long, remoteNextCommitInfo: Either[WaitingForRevocation, Point], commitInput: InputInfo,
                       remotePerCommitmentSecrets: ShaHashesWithIndex, channelId: BinaryData, extraHop: Option[Hop] = None,
                       startedAt: Long = System.currentTimeMillis)

object Commitments {
  def fundingTxid(c: Commitments) = BinaryData(c.commitInput.outPoint.hash.reverse)
  def localHasUnsignedOutgoing(c: Commitments) = c.localChanges.proposed.collectFirst { case u: UpdateAddHtlc => u }.isDefined
  def remoteHasUnsignedOutgoing(c: Commitments) = c.remoteChanges.proposed.collectFirst { case u: UpdateAddHtlc => u }.isDefined
  def latestRemoteCommit(c: Commitments) = c.remoteNextCommitInfo.left.toOption.map(_.nextRemoteCommit) getOrElse c.remoteCommit
  def addRemoteProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.remoteChanges.proposed).using(_ :+ proposal)
  def addLocalProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.localChanges.proposed).using(_ :+ proposal)

  def isHtlcExpired(htlc: Htlc, limitMsat: Long, height: Long) =
    (htlc.add.amountMsat < limitMsat && height - 432 >= htlc.add.expiry) ||
      (htlc.add.amountMsat >= limitMsat && height >= htlc.add.expiry)

  def hasExpiredHtlcs(c: Commitments, height: Long) = {
    val limitMsat = (c.localCommit.spec.feeratePerKw + c.localParams.dustLimit.amount) * 1000L
    c.localCommit.spec.htlcs.exists(htlc => isHtlcExpired(htlc, limitMsat, height) && !htlc.incoming) ||
      c.remoteCommit.spec.htlcs.exists(htlc => isHtlcExpired(htlc, limitMsat, height) && htlc.incoming) ||
      latestRemoteCommit(c).spec.htlcs.exists(htlc => isHtlcExpired(htlc, limitMsat, height) && htlc.incoming)
  }

  def getHtlcCrossSigned(commitments: Commitments, incomingRelativeToLocal: Boolean, htlcId: Long) = {
    val remoteSigned = CommitmentSpec.findHtlcById(commitments.localCommit.spec, htlcId, incomingRelativeToLocal)
    val localSigned = CommitmentSpec.findHtlcById(latestRemoteCommit(commitments).spec, htlcId, !incomingRelativeToLocal)

    for {
      htlcOut <- remoteSigned
      htlcIn <- localSigned
    } yield htlcIn.add
  }

  def sendAdd(c: Commitments, rd: RoutingData) =
    if (rd.firstMsat < c.remoteParams.htlcMinimumMsat) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_LOW)
    else if (rd.firstMsat > maxHtlcValue.amount) throw CMDAddImpossible(rd, ERR_AMOUNT_OVERFLOW)
    else if (rd.pr.paymentHash.size != 32) throw CMDAddImpossible(rd, ERR_FAILED)
    else {

      // Let's compute the current commitment
      // *as seen by them* with this change taken into account
      val add = UpdateAddHtlc(c.channelId, c.localNextHtlcId, rd.lastMsat,
        rd.pr.paymentHash, rd.lastExpiry, rd.onion.packet.serialize)

      val c1 = addLocalProposal(c, add).modify(_.localNextHtlcId).using(_ + 1)
      val reduced = CommitmentSpec.reduce(latestRemoteCommit(c1).spec, c1.remoteChanges.acked, c1.localChanges.proposed)
      val feesSat = if (c1.localParams.isFunder) Scripts.commitTxFee(c.remoteParams.dustLimitSat, reduced).amount else 0L
      val maxAllowedHtlcs = math.min(c.localParams.maxAcceptedHtlcs, c.remoteParams.maxAcceptedHtlcs)
      val totalInFlightMsat = UInt64(reduced.htlcs.map(_.add.amountMsat).sum)
      val incoming \ outgoing = reduced.htlcs.partition(_.incoming)

      // WE can't send more than OUR reserve + commit tx Bitcoin fees
      val reserveWithTxFeeSat = feesSat + c.remoteParams.channelReserveSatoshis
      val missingSat = reduced.toRemoteMsat / 1000L - reserveWithTxFeeSat

      // We should both check if WE can send another HTLC and if PEER can accept another HTLC
      if (totalInFlightMsat > c.remoteParams.maxHtlcValueInFlightMsat) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_HIGH)
      if (outgoing.size > maxAllowedHtlcs || incoming.size > maxAllowedHtlcs) throw CMDAddImpossible(rd, ERR_TOO_MANY_HTLC)
      if (missingSat < 0L) throw CMDReserveOverflow(rd, missingSat, reserveWithTxFeeSat)
      c1 -> add
    }

  def receiveAdd(c: Commitments, add: UpdateAddHtlc) =
    if (add.amountMsat < minHtlcValue.amount) throw new LightningException
    else if (add.id != c.remoteNextHtlcId) throw new LightningException
    else if (add.paymentHash.size != 32) throw new LightningException
    else {

      val c1 = addRemoteProposal(c, add).modify(_.remoteNextHtlcId).using(_ + 1)
      // Let's compute the current commitment *as seen by us* with this change taken into account
      val reduced = CommitmentSpec.reduce(c1.localCommit.spec, c1.localChanges.acked, c1.remoteChanges.proposed)
      val feesSat = if (c.localParams.isFunder) 0L else Scripts.commitTxFee(c.localParams.dustLimit, reduced).amount
      val totalInFlightMsat = UInt64(reduced.htlcs.map(_.add.amountMsat).sum)

      // We should both check if WE can accept another HTLC and if PEER can send another HTLC
      if (totalInFlightMsat > c.localParams.maxHtlcValueInFlightMsat) throw new LightningException
      if (reduced.htlcs.count(_.incoming) > c.localParams.maxAcceptedHtlcs) throw new LightningException
      if (reduced.toRemoteMsat / 1000L - feesSat - c.localParams.channelReserveSat < 0L) throw new LightningException
      c1
    }

  def sendFulfill(c: Commitments, cmd: CMDFulfillHtlc) = {
    val ok = UpdateFulfillHtlc(c.channelId, cmd.id, cmd.preimage)
    getHtlcCrossSigned(commitments = c, incomingRelativeToLocal = true, cmd.id) match {
      case Some(add) if ok.paymentHash == add.paymentHash => addLocalProposal(c, ok) -> ok
      case None => throw new LightningException
    }
  }

  def receiveFulfill(c: Commitments, fulfill: UpdateFulfillHtlc) =
    getHtlcCrossSigned(commitments = c, incomingRelativeToLocal = false, fulfill.id) match {
      case Some(add) if fulfill.paymentHash == add.paymentHash => addRemoteProposal(c, fulfill)
      case None => throw new LightningException
    }

  def sendFail(c: Commitments, cmd: CMDFailHtlc) = {
    val fail = UpdateFailHtlc(c.channelId, cmd.id, cmd.reason)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, cmd.id)
    if (found.isEmpty) throw new LightningException else addLocalProposal(c, fail) -> fail
  }

  def sendFailMalformed(c: Commitments, cmd: CMDFailMalformedHtlc) = {
    val fail = UpdateFailMalformedHtlc(c.channelId, cmd.id, cmd.onionHash, cmd.code)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, htlcId = cmd.id)
    if (found.isEmpty) throw new LightningException else addLocalProposal(c, fail) -> fail
  }

  def receiveFail(c: Commitments, fail: UpdateFailHtlc) = {
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = false, fail.id)
    if (found.isEmpty) throw new LightningException else addRemoteProposal(c, fail)
  }

  def receiveFailMalformed(c: Commitments, fail: UpdateFailMalformedHtlc) = {
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = false, fail.id)
    val notBadOnion = (fail.failureCode & FailureMessageCodecs.BADONION) == 0
    // A receiving node MUST fail a channel if BADONION bit is not set

    if (notBadOnion) throw new LightningException
    if (found.isEmpty) throw new LightningException
    addRemoteProposal(c, fail)
  }

  def sendFee(c: Commitments, ratePerKw: Long) = {
    val updateFee = UpdateFee(c.channelId, ratePerKw)
    val c1 = addLocalProposal(c, updateFee)

    val reduced = CommitmentSpec.reduce(latestRemoteCommit(c1).spec, c1.remoteChanges.acked, c1.localChanges.proposed)
    val remoteWithFeeSat = Scripts.commitTxFee(c1.remoteParams.dustLimitSat, reduced).amount + c1.remoteParams.channelReserveSatoshis
    if (reduced.toRemoteMsat / 1000L - remoteWithFeeSat < 0L) None else Some(c1, updateFee)
  }

  def sendCommit(c: Commitments, remoteNextPerCommitmentPoint: Point) = {
    val spec = CommitmentSpec.reduce(c.remoteCommit.spec, c.remoteChanges.acked, c.localChanges.proposed)
    val htlcKey = Generators.derivePrivKey(c.localParams.htlcKey, remoteNextPerCommitmentPoint)

    val (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs, _, _) =
      Helpers.makeRemoteTxs(c.remoteCommit.index + 1, c.localParams,
        c.remoteParams, c.commitInput, remoteNextPerCommitmentPoint, spec)

    // Generate signatures
    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, htlcKey)

    // Update commitment data
    val remoteChanges1 = c.remoteChanges.copy(acked = Vector.empty, signed = c.remoteChanges.acked)
    val localChanges1 = c.localChanges.copy(proposed = Vector.empty, signed = c.localChanges.proposed)
    val commitSig = CommitSig(c.channelId, Scripts.sign(remoteCommitTx, c.localParams.fundingPrivKey), htlcSigs.toList)
    val remoteCommit1 = RemoteCommit(c.remoteCommit.index + 1, spec, remoteCommitTx.tx.txid, remoteNextPerCommitmentPoint)
    val c1 = c.copy(remoteNextCommitInfo = Left apply WaitingForRevocation(remoteCommit1, commitSig, c.localCommit.index),
      localChanges = localChanges1, remoteChanges = remoteChanges1)

    c1 -> commitSig
  }

  def receiveCommit(c: Commitments, commit: CommitSig) = {
    val spec = CommitmentSpec.reduce(c.localCommit.spec, c.localChanges.acked, c.remoteChanges.proposed)
    val localPerCommitmentSecret = Generators.perCommitSecret(c.localParams.shaSeed, c.localCommit.index)
    val localPerCommitmentPoint = Generators.perCommitPoint(c.localParams.shaSeed, c.localCommit.index + 1)
    val localNextPerCommitmentPoint = Generators.perCommitPoint(c.localParams.shaSeed, c.localCommit.index + 2)
    val remoteHtlcPubkey = Generators.derivePubKey(c.remoteParams.htlcBasepoint, localPerCommitmentPoint)
    val localHtlcKey = Generators.derivePrivKey(c.localParams.htlcKey, localPerCommitmentPoint)

    val (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
      Helpers.makeLocalTxs(c.localCommit.index + 1, c.localParams,
        c.remoteParams, c.commitInput, localPerCommitmentPoint, spec)

    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val signedLocalCommitTx = Scripts.addSigs(localCommitTx, c.localParams.fundingPrivKey.publicKey,
      c.remoteParams.fundingPubkey, Scripts.sign(localCommitTx, c.localParams.fundingPrivKey), commit.signature)

    if (commit.htlcSignatures.size != sortedHtlcTxs.size) throw new LightningException
    if (Scripts.checkSpendable(signedLocalCommitTx).isFailure) throw new LightningException
    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, localHtlcKey)
    val combined = (sortedHtlcTxs, htlcSigs, commit.htlcSignatures).zipped.toList

    val htlcTxsAndSigs = combined collect {
      case (htlcTx: HtlcTimeoutTx, localSig, remoteSig) =>
        val check = Scripts checkSpendable Scripts.addSigs(htlcTx, localSig, remoteSig)
        if (check.isSuccess) HtlcTxAndSigs(htlcTx, localSig, remoteSig)
        else throw new LightningException

      case (htlcTx: HtlcSuccessTx, localSig, remoteSig) =>
        val sigValid = Scripts.checkSig(htlcTx, remoteSig, remoteHtlcPubkey)
        if (sigValid) HtlcTxAndSigs(htlcTx, localSig, remoteSig)
        else throw new LightningException
    }

    val localCommit1 = LocalCommit(c.localCommit.index + 1, spec, htlcTxsAndSigs, signedLocalCommitTx)
    val remoteChanges1 = c.remoteChanges.copy(proposed = Vector.empty, acked = c.remoteChanges.acked ++ c.remoteChanges.proposed)
    val c1 = c.copy(localChanges = c.localChanges.copy(acked = Vector.empty), remoteChanges = remoteChanges1, localCommit = localCommit1)
    val revokeAndAck = RevokeAndAck(c.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
    c1 -> revokeAndAck
  }

  def receiveRevocation(c: Commitments, rev: RevokeAndAck) = c.remoteNextCommitInfo match {
    case Left(_) if c.remoteCommit.remotePerCommitmentPoint != rev.perCommitmentSecret.toPoint =>
      throw new LightningException

    case Left(wait: WaitingForRevocation) =>
      val nextIndex = ShaChain.largestTxIndex - c.remoteCommit.index
      val secrets1 = ShaChain.addHash(c.remotePerCommitmentSecrets, rev.perCommitmentSecret.toBin, nextIndex)
      val localChanges1 = c.localChanges.copy(signed = Vector.empty, acked = c.localChanges.acked ++ c.localChanges.signed)
      val remoteChanges1 = c.remoteChanges.copy(signed = Vector.empty)

      c.copy(localChanges = localChanges1, remoteChanges = remoteChanges1, remoteCommit = wait.nextRemoteCommit,
        remoteNextCommitInfo = Right apply rev.nextPerCommitmentPoint, remotePerCommitmentSecrets = secrets1)

    // Unexpected revocation when we have Point
    case _ => throw new LightningException
  }
}