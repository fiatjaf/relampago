package com.lightning.wallet.ln

import fr.acinq.bitcoin.Crypto._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Scripts._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.AddErrorCodes._
import com.lightning.wallet.ln.LNParams.broadcaster._

import fr.acinq.bitcoin.{BinaryData, Transaction}
import com.lightning.wallet.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex}
import com.lightning.wallet.ln.Helpers.Closing.{SuccessAndClaim, TimeoutAndClaim}
import com.lightning.wallet.ln.CommitmentSpec.{HtlcAndFail, HtlcAndFulfill}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.LNMessageVector
import fr.acinq.eclair.UInt64


sealed trait Command
case class CMDConfirmed(tx: Transaction) extends Command
case class CMDBestHeight(height: Long) extends Command
case class CMDSpent(tx: Transaction) extends Command
case object CMDHTLCProcess extends Command
case object CMDShutdown extends Command
case object CMDOffline extends Command
case object CMDProceed extends Command
case object CMDOnline extends Command

case class CMDOpenChannel(localParams: LocalParams, temporaryChannelId: BinaryData, initialFeeratePerKw: Long,
                          pushMsat: Long, remoteInit: Init, fundingAmountSat: Long) extends Command

case class CMDFailMalformedHtlc(id: Long, onionHash: BinaryData, code: Int) extends Command
case class CMDFulfillHtlc(id: Long, preimage: BinaryData) extends Command
case class CMDFailHtlc(id: Long, reason: BinaryData) extends Command

sealed trait CMDAddHtlc extends Command { val rpi: RuntimePaymentInfo }
case class CMDSilentAddHtlc(rpi: RuntimePaymentInfo) extends CMDAddHtlc
case class CMDPlainAddHtlc(rpi: RuntimePaymentInfo) extends CMDAddHtlc

// CHANNEL DATA

sealed trait ChannelData { val announce: NodeAnnouncement }
sealed trait HasCommitments extends ChannelData { val commitments: Commitments }

case class InitData(announce: NodeAnnouncement) extends ChannelData
case class WaitAcceptData(announce: NodeAnnouncement, cmd: CMDOpenChannel) extends ChannelData
case class WaitFundingData(announce: NodeAnnouncement, cmd: CMDOpenChannel, accept: AcceptChannel) extends ChannelData
case class RefundingData(announce: NodeAnnouncement, commitments: Commitments, fundingTx: Transaction) extends HasCommitments

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

case class NegotiationsData(announce: NodeAnnouncement, commitments: Commitments, localClosingSigned: ClosingSigned,
                            localShutdown: Shutdown, remoteShutdown: Shutdown) extends HasCommitments

case class ClosingData(announce: NodeAnnouncement,
                       commitments: Commitments, mutualClose: Seq[Transaction] = Nil, localCommit: Seq[LocalCommitPublished] = Nil,
                       remoteCommit: Seq[RemoteCommitPublished] = Nil, nextRemoteCommit: Seq[RemoteCommitPublished] = Nil,
                       revokedCommit: Seq[RevokedCommitPublished] = Nil) extends HasCommitments {

  // Every tier12 state txInfo is checked for spendability so if tier12States is empty
  // then we have no tier12 txs to spend which means this is either a mutual close or nothing
  def isOutdated = commitments.startedAt + 1000 * 3600 * 24 * 14 < System.currentTimeMillis
  def tier12States = localCommit.flatMap(_.getState) ++ remoteCommit.flatMap(_.getState) ++
    nextRemoteCommit.flatMap(_.getState) ++ revokedCommit.flatMap(_.getState)

  lazy val commitTxs = localCommit.map(_.commitTx) ++ remoteCommit.map(_.commitTx) ++ nextRemoteCommit.map(_.commitTx)
  lazy val closings = mutualClose.map(Left.apply) ++ localCommit.map(Right.apply) ++ remoteCommit.map(Right.apply) ++
    nextRemoteCommit.map(Right.apply) ++ revokedCommit.map(Right.apply)
}

sealed trait CommitPublished {
  def getState: Seq[PublishStatus]
  val commitTx: Transaction
}

case class LocalCommitPublished(claimMainDelayed: Seq[ClaimDelayedOutputTx], claimHtlcSuccess: Seq[SuccessAndClaim],
                                claimHtlcTimeout: Seq[TimeoutAndClaim], commitTx: Transaction) extends CommitPublished {

  def getState = {
    val main = for (t1 <- claimMainDelayed) yield ShowDelayed(csv(commitTx, t1.tx), t1.tx, t1 -- t1, t1.amount) :: Nil
    val timeout = for (t1 \ t2 <- claimHtlcTimeout) yield HideDelayed(cltv(commitTx, t1.tx), t1.tx) :: csvShowDelayed(t1, t2) :: Nil
    val success = for (t1 \ t2 <- claimHtlcSuccess) yield HideReady(t1.tx) :: csvShowDelayed(t1, t2) :: Nil
    main.flatten ++ success.flatten ++ timeout.flatten
  }
}

case class RemoteCommitPublished(claimMain: Seq[ClaimP2WPKHOutputTx], claimHtlcSuccess: Seq[ClaimHtlcSuccessTx],
                                 claimHtlcTimeout: Seq[ClaimHtlcTimeoutTx], commitTx: Transaction) extends CommitPublished {

  def getState = {
    val main = for (t1 <- claimMain) yield ShowReady(t1.tx, t1 -- t1, t1.amount)
    val timeout = for (t1 <- claimHtlcTimeout) yield cltvShowDelayed(commitTx, t1)
    val success = for (t1 <- claimHtlcSuccess) yield ShowReady(t1.tx, t1 -- t1, t1.amount)
    main ++ success ++ timeout
  }
}

case class RevokedCommitPublished(claimMain: Seq[ClaimP2WPKHOutputTx], claimPenalty: Seq[MainPenaltyTx],
                                  commitTx: Transaction) extends CommitPublished {

  def getState = {
    val main = for (t1 <- claimMain) yield ShowReady(t1.tx, t1 -- t1, t1.amount)
    val penalty = for (t1 <- claimPenalty) yield ShowReady(t1.tx, t1 -- t1, t1.amount)
    main ++ penalty
  }
}

// COMMITMENTS

case class Htlc(incoming: Boolean, add: UpdateAddHtlc)
case class CommitmentSpec(htlcs: Set[Htlc], fulfilled: Set[HtlcAndFulfill], failed: Set[HtlcAndFail],
                          feeratePerKw: Long, toLocalMsat: Long, toRemoteMsat: Long)

object CommitmentSpec {
  def findHtlcById(cs: CommitmentSpec, id: Long, isIncoming: Boolean): Option[Htlc] =
    cs.htlcs.find(htlc => htlc.add.id == id && htlc.incoming == isIncoming)

  type HtlcAndFulfill = (Htlc, UpdateFulfillHtlc)
  private def fulfill(cs: CommitmentSpec, in: Boolean, u: UpdateFulfillHtlc) =

    findHtlcById(cs, u.id, in) match {
      case Some(htlc) if htlc.incoming =>
        cs.copy(toLocalMsat = cs.toLocalMsat + htlc.add.amountMsat,
          fulfilled = cs.fulfilled + Tuple2(htlc, u), htlcs = cs.htlcs - htlc)

      case Some(htlc) =>
        cs.copy(toRemoteMsat = cs.toRemoteMsat + htlc.add.amountMsat,
          fulfilled = cs.fulfilled + Tuple2(htlc, u), htlcs = cs.htlcs - htlc)

      case None => cs
    }

  type HtlcAndFail = (Htlc, LightningMessage)
  private def fail(cs: CommitmentSpec, in: Boolean, u: HasHtlcId) =

    findHtlcById(cs, u.id, in) match {
      case Some(htlc) if htlc.incoming =>
        cs.copy(toRemoteMsat = cs.toRemoteMsat + htlc.add.amountMsat,
          failed = cs.failed + Tuple2(htlc, u), htlcs = cs.htlcs - htlc)

      case Some(htlc) =>
        cs.copy(toLocalMsat = cs.toLocalMsat + htlc.add.amountMsat,
          failed = cs.failed + Tuple2(htlc, u), htlcs = cs.htlcs - htlc)

      case None => cs
    }

  private def plusOutgoing(data: UpdateAddHtlc, cs: CommitmentSpec) =
    cs.copy(htlcs = cs.htlcs + Htlc(incoming = false, add = data),
      toLocalMsat = cs.toLocalMsat - data.amountMsat)

  private def plusIncoming(data: UpdateAddHtlc, cs: CommitmentSpec) =
    cs.copy(htlcs = cs.htlcs + Htlc(incoming = true, add = data),
      toRemoteMsat = cs.toRemoteMsat - data.amountMsat)

  def reduce(cs: CommitmentSpec, localChanges: LNMessageVector, remoteChanges: LNMessageVector) = {
    // Before reducing fresh changes we need to get rid of previous fulfilled and failed messages

    val spec1 = cs.copy(fulfilled = Set.empty, failed = Set.empty)
    val spec2 = (spec1 /: localChanges) { case (s, add: UpdateAddHtlc) => plusOutgoing(add, s) case s \ _ => s }
    val spec3 = (spec2 /: remoteChanges) { case (s, add: UpdateAddHtlc) => plusIncoming(add, s) case s \ _ => s }

    val spec4 = (spec3 /: localChanges) {
      case (s, msg: UpdateFailHtlc) => fail(s, in = true, msg)
      case (s, msg: UpdateFailMalformedHtlc) => fail(s, in = true, msg)
      case (s, msg: UpdateFee) => s.copy(feeratePerKw = msg.feeratePerKw)
      case (s, msg: UpdateFulfillHtlc) => fulfill(s, in = true, msg)
      case s \ _ => s
    }

    (spec4 /: remoteChanges) {
      case (s, msg: UpdateFailHtlc) => fail(s, in = false, msg)
      case (s, msg: UpdateFailMalformedHtlc) => fail(s, in = false, msg)
      case (s, msg: UpdateFee) => s.copy(feeratePerKw = msg.feeratePerKw)
      case (s, msg: UpdateFulfillHtlc) => fulfill(s, in = false, msg)
      case s \ _ => s
    }
  }
}

case class LocalParams(maxHtlcValueInFlightMsat: UInt64, channelReserveSat: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int,
                       fundingPrivKey: PrivateKey, revocationSecret: Scalar, paymentKey: Scalar, delayedPaymentKey: Scalar,
                       htlcKey: Scalar, defaultFinalScriptPubKey: BinaryData, shaSeed: BinaryData, isFunder: Boolean) {

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
                       remotePerCommitmentSecrets: ShaHashesWithIndex, channelId: BinaryData, startedAt: Long = System.currentTimeMillis)

object Commitments {
  def hasNoPendingHtlc(c: Commitments) = c.localCommit.spec.htlcs.isEmpty && actualRemoteCommit(c).spec.htlcs.isEmpty
  def localHasUnsignedOutgoing(c: Commitments) = c.localChanges.proposed.collectFirst { case u: UpdateAddHtlc => u }.isDefined
  def remoteHasUnsignedOutgoing(c: Commitments) = c.remoteChanges.proposed.collectFirst { case u: UpdateAddHtlc => u }.isDefined
  def actualRemoteCommit(c: Commitments) = c.remoteNextCommitInfo.left.toOption.map(_.nextRemoteCommit) getOrElse c.remoteCommit
  def addRemoteProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.remoteChanges.proposed).using(_ :+ proposal)
  def addLocalProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.localChanges.proposed).using(_ :+ proposal)

  def hasOutdatedOutgoing(c: Commitments, height: Long) =
    c.localCommit.spec.htlcs.exists(htlc => !htlc.incoming && height >= htlc.add.expiry) ||
      c.remoteCommit.spec.htlcs.exists(htlc => htlc.incoming && height >= htlc.add.expiry)

  def getHtlcCrossSigned(commitments: Commitments, incomingRelativeToLocal: Boolean, htlcId: Long) = {
    val remoteSigned = CommitmentSpec.findHtlcById(commitments.localCommit.spec, htlcId, incomingRelativeToLocal)
    val localSigned = CommitmentSpec.findHtlcById(actualRemoteCommit(commitments).spec, htlcId, !incomingRelativeToLocal)

    for {
      htlcOut <- remoteSigned
      htlcIn <- localSigned
    } yield htlcIn.add
  }

  def sendAdd(c: Commitments, cmd: CMDAddHtlc) =
    if (cmd.rpi.firstMsat < c.remoteParams.htlcMinimumMsat) throw CMDAddExcept(cmd, ERR_REMOTE_AMOUNT_LOW)
    else if (cmd.rpi.firstMsat > maxHtlcValue.amount) throw CMDAddExcept(cmd, ERR_AMOUNT_OVERFLOW)
    else if (cmd.rpi.pr.paymentHash.size != 32) throw CMDAddExcept(cmd, ERR_FAILED)
    else {

      // Let's compute the current commitment
      // *as seen by them* with this change taken into account
      val add = UpdateAddHtlc(c.channelId, c.localNextHtlcId, cmd.rpi.rd.lastMsat,
        cmd.rpi.pr.paymentHash, cmd.rpi.rd.lastExpiry, cmd.rpi.rd.onion.packet.serialize)

      val c1 = addLocalProposal(c, add).modify(_.localNextHtlcId).using(_ + 1)
      val reduced = CommitmentSpec.reduce(actualRemoteCommit(c1).spec, c1.remoteChanges.acked, c1.localChanges.proposed)
      val feesSat = if (c1.localParams.isFunder) Scripts.commitTxFee(c.remoteParams.dustLimitSat, reduced).amount else 0L
      val maxAllowedHtlcs = math.min(c.localParams.maxAcceptedHtlcs, c.remoteParams.maxAcceptedHtlcs)
      val totalInFlightMsat = UInt64(reduced.htlcs.map(_.add.amountMsat).sum)
      val incoming \ outgoing = reduced.htlcs.partition(_.incoming)

      // WE can't send more than OUR reserve + commit tx Bitcoin fees
      val reserveWithTxFeeSat = feesSat + c.remoteParams.channelReserveSatoshis
      val missingSat = reduced.toRemoteMsat / 1000L - reserveWithTxFeeSat

      // We should both check if WE can send another HTLC and if PEER can accept another HTLC
      if (totalInFlightMsat > c.remoteParams.maxHtlcValueInFlightMsat) throw CMDAddExcept(cmd, ERR_TOO_MANY_HTLC)
      if (outgoing.size > maxAllowedHtlcs | incoming.size > maxAllowedHtlcs) throw CMDAddExcept(cmd, ERR_TOO_MANY_HTLC)
      if (missingSat < 0L) throw CMDReserveExcept(cmd, missingSat, reserveWithTxFeeSat)
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
      val feesSat = if (c.localParams.isFunder) 0L else Scripts.commitTxFee(dustLimit, reduced).amount
      val totalInFlightMsat = UInt64(reduced.htlcs.map(_.add.amountMsat).sum)

      // We should both check if WE can accept another HTLC and if PEER can send another HTLC
      if (totalInFlightMsat > c.localParams.maxHtlcValueInFlightMsat) throw new LightningException
      if (reduced.htlcs.count(_.incoming) > c.localParams.maxAcceptedHtlcs) throw new LightningException
      if (reduced.toRemoteMsat / 1000L - feesSat - c.localParams.channelReserveSat < 0L) throw new LightningException
      c1
    }

  def sendFulfill(c: Commitments, cmd: CMDFulfillHtlc) = {
    val fulfill = UpdateFulfillHtlc(c.channelId, cmd.id, cmd.preimage)
    getHtlcCrossSigned(commitments = c, incomingRelativeToLocal = true, cmd.id) match {
      case Some(add) if fulfill.paymentHash == add.paymentHash => addLocalProposal(c, fulfill) -> fulfill
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

    val reduced = CommitmentSpec.reduce(actualRemoteCommit(c1).spec, c1.remoteChanges.acked, c1.localChanges.proposed)
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
    val remoteCommit1 = RemoteCommit(c.remoteCommit.index + 1, spec, remoteCommitTx.tx.txid, remoteNextPerCommitmentPoint)
    val commitSig = CommitSig(c.channelId, Scripts.sign(remoteCommitTx, c.localParams.fundingPrivKey), htlcSigs.toList)
    val waiting = WaitingForRevocation(remoteCommit1, commitSig, c.localCommit.index)

    val c1 = c.copy(remoteNextCommitInfo = Left apply waiting,
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
    if (Scripts.checkSpendable(signedLocalCommitTx).isEmpty) throw new LightningException
    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, localHtlcKey)
    val combined = (sortedHtlcTxs, htlcSigs, commit.htlcSignatures).zipped.toList

    val htlcTxsAndSigs = combined collect {
      case (htlcTx: HtlcTimeoutTx, localSig, remoteSig) =>
        val check = Scripts checkSpendable Scripts.addSigs(htlcTx, localSig, remoteSig)
        if (check.isDefined) HtlcTxAndSigs(htlcTx, localSig, remoteSig)
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