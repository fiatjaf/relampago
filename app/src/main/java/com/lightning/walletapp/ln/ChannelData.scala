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
import org.bitcoinj.core.Batch
import fr.acinq.eclair.UInt64


sealed trait Command
case class CMDShutdown(scriptPubKey: Option[BinaryData] = None) extends Command
case class CMDBestHeight(heightNow: Long, heightInit: Long) extends Command
case class CMDConfirmed(tx: Transaction) extends Command
case class CMDFunding(tx: Transaction) extends Command
case class CMDSpent(tx: Transaction) extends Command
case class CMDFeerate(sat: Long) extends Command
case object CMDHTLCProcess extends Command
case object CMDProceed extends Command
case object CMDOffline extends Command
case object CMDOnline extends Command

case class CMDOpenChannel(localParams: LocalParams, tempChanId: BinaryData,
                          initialFeeratePerKw: Long, batch: Batch, fundingSat: Long,
                          pushMsat: Long) extends Command

case class CMDFailMalformedHtlc(id: Long, onionHash: BinaryData, code: Int) extends Command
case class CMDFulfillHtlc(id: Long, preimage: BinaryData) extends Command
case class CMDFailHtlc(id: Long, reason: BinaryData) extends Command

// CHANNEL DATA

sealed trait ChannelData { val announce: NodeAnnouncement }
sealed trait HasCommitments extends ChannelData { val commitments: Commitments }
case class InitData(announce: NodeAnnouncement) extends ChannelData

// INCOMING CHANNEL

case class WaitFundingCreatedRemote(announce: NodeAnnouncement, localParams: LocalParams,
                                    remoteParams: AcceptChannel, open: OpenChannel) extends ChannelData

// OUTGOING CHANNEL

case class WaitAcceptData(announce: NodeAnnouncement, cmd: CMDOpenChannel) extends ChannelData
case class WaitFundingData(announce: NodeAnnouncement, cmd: CMDOpenChannel, accept: AcceptChannel) extends ChannelData

// Funding tx may arrive locally or from external funder
case class WaitFundingSignedCore(localParams: LocalParams, channelId: BinaryData,
                                 remoteParams: AcceptChannel, localSpec: CommitmentSpec,
                                 localCommitTx: CommitTx, remoteCommit: RemoteCommit) {

  def makeCommitments(signedLocalCommitTx: CommitTx) =
    Commitments(localParams, remoteParams, LocalCommit(index = 0L, localSpec, Nil, signedLocalCommitTx), remoteCommit,
      localChanges = Changes(Vector.empty, Vector.empty, Vector.empty), remoteChanges = Changes(Vector.empty, Vector.empty, Vector.empty),
      localNextHtlcId = 0L, remoteNextHtlcId = 0L, remoteNextCommitInfo = Right(Tools.randomPrivKey.toPoint), localCommitTx.input,
      remotePerCommitmentSecrets = ShaHashesWithIndex(Map.empty, None), channelId)
}

case class WaitFundingSignedRemoteData(announce: NodeAnnouncement, core: WaitFundingSignedCore, txHash: BinaryData) extends ChannelData {
  def makeWaitBroadcastRemoteData(signedCommit: CommitTx) = WaitBroadcastRemoteData(announce, core, txHash, core makeCommitments signedCommit)
}

case class WaitFundingSignedData(announce: NodeAnnouncement, core: WaitFundingSignedCore, fundingTx: Transaction) extends ChannelData {
  def makeWaitFundingDoneData(signedCommit: CommitTx) = WaitFundingDoneData(announce, None, None, fundingTx, core makeCommitments signedCommit)
}

// ALL THE DATA BELOW WILL BE STORED

case class WaitBroadcastRemoteData(announce: NodeAnnouncement, core: WaitFundingSignedCore, txHash: BinaryData,
                                   commitments: Commitments, fail: Option[Fail] = None) extends HasCommitments {

  // Failed channels will be automatically removed in one day after being started
  def isFailed = fail.isDefined && commitments.startedAt < System.currentTimeMillis - 3600 * 24 * 1000L
  def isOutdated = commitments.startedAt < System.currentTimeMillis - 3600 * 4 * 1000L
}

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

  private lazy val realTier12Closings =
    revokedCommit ++ localCommit ++ remoteCommit ++
      nextRemoteCommit ++ refundRemoteCommit

  lazy val commitTxs = realTier12Closings.map(_.commitTx)
  lazy val frozenPublishedHashes = realTier12Closings.flatMap(_.frozenHashes)
  def tier12States: Seq[PublishStatus] = realTier12Closings.flatMap(_.getState)

  def bestClosing: CommitPublished = {
    // At least one closing is guaranteed to be here
    val mutualWrappers = mutualClose map MutualCommitPublished
    mutualWrappers ++ realTier12Closings maxBy { commitPublished =>
      val confirmations \ isDead = getStatus(commitPublished.commitTx.txid)
      if (isDead) -confirmations else confirmations
    }
  }

  def isOutdated: Boolean = {
    val allConfirmedOrDead = bestClosing match {
      case MutualCommitPublished(mutualTx) => getStatus(mutualTx.txid) match { case cfs \ isDead => cfs > minDepth || isDead }
      case info => info.getState.map(state => state.txn.txid) map getStatus forall { case cfs \ isDead => cfs > minDepth || isDead }
    }

    val hardDelay = closedAt + 1000L * 3600 * 24 * 28
    allConfirmedOrDead || hardDelay < System.currentTimeMillis
  }
}

sealed trait CommitPublished {
  def frozenHashes: Seq[BinaryData] = Nil
  def getState: Seq[PublishStatus] = Nil
  val commitTx: Transaction
}

case class LocalCommitPublished(claimMainDelayed: Seq[ClaimDelayedOutputTx], claimHtlcSuccess: Seq[SuccessAndClaim],
                                claimHtlcTimeout: Seq[TimeoutAndClaim], commitTx: Transaction) extends CommitPublished {

  override def frozenHashes = for {
    claimTimeout \ _ <- claimHtlcTimeout
  } yield claimTimeout.add.paymentHash

  override def getState = {
    val success = for (tier1 \ tier2 <- claimHtlcSuccess) yield HideReady(tier1.tx) :: csvShowDelayed(tier1, tier2, commitTx) :: Nil
    val timeout = for (t1 \ t2 <- claimHtlcTimeout) yield HideDelayed(cltv(commitTx, t1.tx), t1.tx) :: csvShowDelayed(t1, t2, commitTx) :: Nil
    val main = for (t1 <- claimMainDelayed) yield ShowDelayed(csv(commitTx, t1.tx), t1.tx, commitTx, t1 -- t1, t1.tx.allOutputsAmount) :: Nil
    main.flatten ++ success.flatten ++ timeout.flatten
  }
}

case class RemoteCommitPublished(claimMain: Seq[ClaimP2WPKHOutputTx], claimHtlcSuccess: Seq[ClaimHtlcSuccessTx],
                                 claimHtlcTimeout: Seq[ClaimHtlcTimeoutTx], commitTx: Transaction) extends CommitPublished {

  override def frozenHashes = for {
    claimTimeout <- claimHtlcTimeout
    add <- claimTimeout.addOpt
  } yield add.paymentHash

  override def getState = {
    val timeout = for (t1 <- claimHtlcTimeout) yield ShowDelayed(cltv(commitTx, t1.tx), t1.tx, commitTx, t1 -- t1, t1.tx.allOutputsAmount)
    val success = for (tier1 <- claimHtlcSuccess) yield ShowReady(tier1.tx, tier1 -- tier1, tier1.tx.allOutputsAmount)
    val main = for (t1 <- claimMain) yield ShowReady(t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    main ++ success ++ timeout
  }
}

case class MutualCommitPublished(commitTx: Transaction) extends CommitPublished
case class RevokedCommitPublished(claimMain: Seq[ClaimP2WPKHOutputTx], claimTheirMainPenalty: Seq[MainPenaltyTx],
                                  htlcPenalty: Seq[HtlcPenaltyTx], commitTx: Transaction) extends CommitPublished {

  override def getState = {
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

case class ReducedState(htlcs: Set[Htlc], canSendMsat: Long, canReceiveMsat: Long, myFeeSat: Long, theirFeeSat: Long)
case class Commitments(localParams: LocalParams, remoteParams: AcceptChannel, localCommit: LocalCommit, remoteCommit: RemoteCommit, localChanges: Changes,
                       remoteChanges: Changes, localNextHtlcId: Long, remoteNextHtlcId: Long, remoteNextCommitInfo: Either[WaitingForRevocation, Point],
                       commitInput: InputInfo, remotePerCommitmentSecrets: ShaHashesWithIndex, channelId: BinaryData, extraHop: Option[Hop] = None,
                       startedAt: Long = System.currentTimeMillis) { me =>

  lazy val reducedRemoteState = {
    val reduced = CommitmentSpec.reduce(Commitments.latestRemoteCommit(me).spec, remoteChanges.acked, localChanges.proposed)
    val commitFeeSat = Scripts.commitTxFee(dustLimit = remoteParams.dustLimitSat, spec = reduced).amount
    val myFeeSat \ theirFeeSat = if (localParams.isFunder) commitFeeSat -> 0L else 0L -> commitFeeSat

    val canSendMsat = reduced.toRemoteMsat - (myFeeSat + remoteParams.channelReserveSatoshis) * 1000L
    val canReceiveMsat = localCommit.spec.toRemoteMsat - (theirFeeSat + localParams.channelReserveSat) * 1000L
    ReducedState(reduced.htlcs, canSendMsat, canReceiveMsat, myFeeSat, theirFeeSat)
  }
}

object Commitments {
  def fundingTxid(c: Commitments) = BinaryData(c.commitInput.outPoint.hash.reverse)
  def localHasUnsignedOutgoing(c: Commitments) = c.localChanges.proposed.collectFirst { case u: UpdateAddHtlc => u }.isDefined
  def remoteHasUnsignedOutgoing(c: Commitments) = c.remoteChanges.proposed.collectFirst { case u: UpdateAddHtlc => u }.isDefined
  def latestRemoteCommit(c: Commitments) = c.remoteNextCommitInfo.left.toOption.map(_.nextRemoteCommit) getOrElse c.remoteCommit
  def addRemoteProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.remoteChanges.proposed).using(_ :+ proposal)
  def addLocalProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.localChanges.proposed).using(_ :+ proposal)

  def findExpiredHtlc(c: Commitments, cmd: CMDBestHeight) =
    c.localCommit.spec.htlcs.find(htlc => !htlc.incoming && cmd.heightNow >= htlc.add.expiry && cmd.heightInit <= htlc.add.expiry) orElse
      c.remoteCommit.spec.htlcs.find(htlc => htlc.incoming && cmd.heightNow >= htlc.add.expiry && cmd.heightInit <= htlc.add.expiry) orElse
      latestRemoteCommit(c).spec.htlcs.find(htlc => htlc.incoming && cmd.heightNow >= htlc.add.expiry && cmd.heightInit <= htlc.add.expiry)

  def getHtlcCrossSigned(commitments: Commitments, incomingRelativeToLocal: Boolean, htlcId: Long) = for {
    _ <- CommitmentSpec.findHtlcById(latestRemoteCommit(commitments).spec, htlcId, !incomingRelativeToLocal)
    htlcOut <- CommitmentSpec.findHtlcById(commitments.localCommit.spec, htlcId, incomingRelativeToLocal)
  } yield htlcOut.add

  def ifSenderCanAffordFees(cs: Commitments) = {
    val reduced = CommitmentSpec.reduce(cs.localCommit.spec, cs.localChanges.acked, cs.remoteChanges.proposed)
    val feesSat = if (cs.localParams.isFunder) 0L else Scripts.commitTxFee(cs.localParams.dustLimit, reduced).amount
    if (reduced.toRemoteMsat / 1000L - feesSat - cs.localParams.channelReserveSat < 0L) throw new LightningException
    cs -> reduced
  }

  def sendFee(commitments: Commitments, ratePerKw: Long) = {
    if (!commitments.localParams.isFunder) throw new LightningException
    val updateFeeMessage = UpdateFee(commitments.channelId, ratePerKw)
    val c1 = addLocalProposal(commitments, updateFeeMessage)
    if (c1.reducedRemoteState.canSendMsat < 0L) None
    else Some(c1 -> updateFeeMessage)
  }

  def receiveFee(commitments: Commitments, fee: UpdateFee) = {
    if (commitments.localParams.isFunder) throw new LightningException
    if (fee.feeratePerKw < minFeeratePerKw) throw new LightningException
    val c1 \ _ = Commitments ifSenderCanAffordFees addRemoteProposal(commitments, fee)
    c1
  }

  def sendAdd(c: Commitments, rd: RoutingData) =
    if (rd.firstMsat < c.remoteParams.htlcMinimumMsat) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_LOW)
    else if (rd.firstMsat > maxHtlcValueMsat) throw CMDAddImpossible(rd, ERR_AMOUNT_OVERFLOW)
    else if (rd.pr.paymentHash.size != 32) throw CMDAddImpossible(rd, ERR_FAILED)
    else {

      // Let's compute the current commitment
      // *as seen by them* with this change taken into account
      val add = UpdateAddHtlc(c.channelId, c.localNextHtlcId, rd.lastMsat,
        rd.pr.paymentHash, rd.lastExpiry, rd.onion.packet.serialize)

      val c1 = addLocalProposal(c, add).modify(_.localNextHtlcId).using(_ + 1)
      // We have an updated Commitments in c1 and thus c1.reducedRemoteState gets updated too
      val maxAllowedHtlcs = math.min(c.localParams.maxAcceptedHtlcs, c.remoteParams.maxAcceptedHtlcs)
      val totalInFlightMsat = UInt64(c1.reducedRemoteState.htlcs.map(_.add.amountMsat).sum)
      val incoming \ outgoing = c1.reducedRemoteState.htlcs.partition(_.incoming)

      // We should both check if we can send another HTLC and if PEER can accept another HTLC
      if (totalInFlightMsat > c.remoteParams.maxHtlcValueInFlightMsat) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_HIGH)
      if (outgoing.size > maxAllowedHtlcs || incoming.size > maxAllowedHtlcs) throw CMDAddImpossible(rd, ERR_TOO_MANY_HTLC)
      if (c1.reducedRemoteState.canSendMsat < 0L) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_HIGH)
      c1 -> add
    }

  def receiveAdd(c: Commitments, add: UpdateAddHtlc) =
    if (add.amountMsat < minHtlcValue.amount) throw new LightningException
    else if (add.id != c.remoteNextHtlcId) throw new LightningException
    else if (add.paymentHash.size != 32) throw new LightningException
    else {

      // We should both check if WE can accept another HTLC and if PEER can send another HTLC
      // let's compute the current commitment *as seen by us* with this change taken into account
      val c1 \ reduced = Commitments ifSenderCanAffordFees addRemoteProposal(c, add).modify(_.remoteNextHtlcId).using(_ + 1)
      if (UInt64(reduced.htlcs.map(_.add.amountMsat).sum) > c.localParams.maxHtlcValueInFlightMsat) throw new LightningException
      if (reduced.htlcs.count(_.incoming) > c.localParams.maxAcceptedHtlcs) throw new LightningException
      c1
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
    if (fail.failureCode.&(FailureMessageCodecs.BADONION) == 0) throw new LightningException("BadOnion not set")
    if (getHtlcCrossSigned(c, incomingRelativeToLocal = false, fail.id).isEmpty) throw new LightningException
    addRemoteProposal(c, fail)
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
    if (Scripts.checkValid(signedLocalCommitTx).isFailure) throw new LightningException
    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, localHtlcKey)
    val combined = (sortedHtlcTxs, htlcSigs, commit.htlcSignatures).zipped.toList

    val htlcTxsAndSigs = combined collect {
      case (htlcTx: HtlcTimeoutTx, localSig, remoteSig) =>
        val check = Scripts checkValid Scripts.addSigs(htlcTx, localSig, remoteSig)
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