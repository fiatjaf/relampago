package com.lightning.walletapp.ln

import fr.acinq.bitcoin.Crypto._
import com.softwaremill.quicklens._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Scripts._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.ChanErrorCodes._
import com.lightning.walletapp.ln.LNParams.broadcaster._
import com.lightning.walletapp.ln.CommitmentSpec.{HtlcAndFail, HtlcAndFulfill}
import com.lightning.walletapp.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex}
import com.lightning.walletapp.ln.Helpers.Closing.{SuccessAndClaim, TimeoutAndClaim}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.{HTLCTuple, LNMessageVector, RedeemScriptAndSig}
import fr.acinq.bitcoin.{Satoshi, Transaction}
import org.bitcoinj.core.Batch
import scodec.bits.ByteVector
import fr.acinq.eclair.UInt64


sealed trait Command
case class CMDShutdown(scriptPubKey: Option[ByteVector] = None) extends Command
case class CMDChannelUpdate(upd: ChannelUpdate) extends Command
case class CMDStateOverride(so: StateOverride) extends Command
case class CMDConfirmed(tx: Transaction) extends Command
case class CMDFunding(tx: Transaction) extends Command
case class CMDSpent(tx: Transaction) extends Command
case class CMDFeerate(sat: Long) extends Command
case object CMDChainTipKnown extends Command
case object CMDSocketOffline extends Command
case object CMDSocketOnline extends Command
case object CMDHTLCProcess extends Command
case object CMDProceed extends Command

case class CMDOpenChannel(localParams: LocalParams, tempChanId: ByteVector, initialFeeratePerKw: Long, batch: Batch,
                          fundingSat: Long, channelFlags: ChannelFlags = ChannelFlags(0), pushMsat: Long = 0L) extends Command

case class CMDFailMalformedHtlc(id: Long, onionHash: ByteVector, code: Int) extends Command
case class CMDFulfillHtlc(add: UpdateAddHtlc, preimage: ByteVector) extends Command
case class CMDFailHtlc(id: Long, reason: ByteVector) extends Command

// CHANNEL DATA

sealed trait ChannelData { val announce: NodeAnnouncement }
sealed trait HasNormalCommits extends ChannelData { val commitments: NormalCommits }
case class InitData(announce: NodeAnnouncement) extends ChannelData

// HOSTED CHANNEL

case class WaitTheirHostedReply(announce: NodeAnnouncement, refundScriptPubKey: ByteVector) extends ChannelData {
  require(Helpers isValidFinalScriptPubkey refundScriptPubKey, "Invalid refundScriptPubKey when opening a hosted channel")
  lazy val initMsg = InvokeHostedChannel(LNParams.chainHash, refundScriptPubKey)
}

case class WaitTheirStateUpdate(announce: NodeAnnouncement, refundScriptPubKey: ByteVector,
                                clientFirstUpdate: StateUpdate, init: InitHostedChannel) extends ChannelData

// INCOMING CHANNEL

case class WaitFundingCreatedRemote(announce: NodeAnnouncement, localParams: LocalParams,
                                    remoteParams: AcceptChannel, open: OpenChannel) extends ChannelData

// OUTGOING CHANNEL

case class WaitAcceptData(announce: NodeAnnouncement, cmd: CMDOpenChannel) extends ChannelData
case class WaitFundingData(announce: NodeAnnouncement, cmd: CMDOpenChannel, accept: AcceptChannel) extends ChannelData

// Funding tx may arrive locally or from external funder
case class WaitFundingSignedCore(localParams: LocalParams, channelId: ByteVector, channelFlags: Option[ChannelFlags],
                                 remoteParams: AcceptChannel, localSpec: CommitmentSpec, remoteCommit: RemoteCommit) {

  def makeCommitments(signedLocalCommitTx: CommitTx) =
    NormalCommits(localParams, remoteParams, LocalCommit(index = 0L, localSpec, Nil, signedLocalCommitTx), remoteCommit,
      localChanges = Changes(Vector.empty, Vector.empty, Vector.empty), remoteChanges = Changes(Vector.empty, Vector.empty, Vector.empty),
      localNextHtlcId = 0L, remoteNextHtlcId = 0L, remoteNextCommitInfo = Right(Tools.randomPrivKey.toPoint), signedLocalCommitTx.input,
      ShaHashesWithIndex(Map.empty, None), channelId, updateOpt = None, channelFlags, startedAt = System.currentTimeMillis)
}

case class WaitFundingSignedData(announce: NodeAnnouncement, core: WaitFundingSignedCore,
                                 localCommitTx: CommitTx, fundingTx: Transaction) extends ChannelData

// ALL THE DATA BELOW WILL BE STORED

case class WaitBroadcastRemoteData(announce: NodeAnnouncement, core: WaitFundingSignedCore,
                                   commitments: NormalCommits, their: Option[FundingLocked] = None,
                                   fundingError: Option[String] = None) extends HasNormalCommits {

  def isLost: Boolean = fundingError match {
    case None => commitments.startedAt < System.currentTimeMillis - 3600 * 24 * 21 * 1000L
    case _ => commitments.startedAt < System.currentTimeMillis - 3600 * 2 * 21 * 1000L
  }
}

case class WaitFundingDoneData(announce: NodeAnnouncement, our: Option[FundingLocked], their: Option[FundingLocked],
                               fundingTx: Transaction, commitments: NormalCommits) extends HasNormalCommits {

  def doubleSpendsFunding(that: Transaction) = {
    val thatInputOutPoints = that.txIn.map(_.outPoint)
    val fundingInputOutPoints = fundingTx.txIn.map(_.outPoint)
    val sameOuts = thatInputOutPoints.intersect(fundingInputOutPoints)
    that.txid != fundingTx.txid && sameOuts.nonEmpty
  }
}

case class NormalData(announce: NodeAnnouncement, commitments: NormalCommits, localShutdown: Option[Shutdown] = None,
                      remoteShutdown: Option[Shutdown] = None, unknownSpend: Option[Transaction] = None) extends HasNormalCommits

case class ClosingTxProposed(unsignedTx: ClosingTx, localClosingSigned: ClosingSigned)
case class NegotiationsData(announce: NodeAnnouncement, commitments: NormalCommits, localShutdown: Shutdown, remoteShutdown: Shutdown,
                            localProposals: Seq[ClosingTxProposed], lastSignedTx: Option[ClosingTx] = None) extends HasNormalCommits

case class RefundingData(announce: NodeAnnouncement, remoteLatestPoint: Option[Point],
                         commitments: NormalCommits) extends HasNormalCommits

case class ClosingData(announce: NodeAnnouncement,
                       commitments: NormalCommits, localProposals: Seq[ClosingTxProposed] = Nil,
                       mutualClose: Seq[Transaction] = Nil, localCommit: Seq[LocalCommitPublished] = Nil,
                       remoteCommit: Seq[RemoteCommitPublished] = Nil, nextRemoteCommit: Seq[RemoteCommitPublished] = Nil,
                       refundRemoteCommit: Seq[RemoteCommitPublished] = Nil, revokedCommit: Seq[RevokedCommitPublished] = Nil,
                       closedAt: Long = System.currentTimeMillis) extends HasNormalCommits {

  lazy val commitTxs = realTier12Closings.map(_.commitTx)
  lazy val realTier12Closings = revokedCommit ++ localCommit ++ remoteCommit ++ nextRemoteCommit ++ refundRemoteCommit
  def canBeRemoved: Boolean = if (System.currentTimeMillis > closedAt + 1000L * 3600 * 24 * 28) true else bestClosing match {
    case MutualCommitPublished(closingTx) => getStatus(closingTx.txid) match { case confs \ isDead => confs > minDepth || isDead }
    case info => info.getState.map(_.txn.txid).map(getStatus) forall { case confs \ isDead => confs > minDepth || isDead }
  }

  def bestClosing: CommitPublished = {
    // At least one closing is guaranteed to be here
    val mutualWrappers = mutualClose map MutualCommitPublished
    mutualWrappers ++ realTier12Closings maxBy { commitPublished =>
      val txDepth \ isDead = getStatus(commitPublished.commitTx.txid)
      if (isDead) -txDepth else txDepth
    }
  }

  // Not a lazy val because results depend on blockchain state
  def tier12States = realTier12Closings.flatMap(_.getState)
}

sealed trait CommitPublished {
  def getState: Seq[PublishStatus] = Nil
  val commitTx: Transaction
}

case class LocalCommitPublished(claimMainDelayed: Seq[ClaimDelayedOutputTx], claimHtlcSuccess: Seq[SuccessAndClaim],
                                claimHtlcTimeout: Seq[TimeoutAndClaim], commitTx: Transaction) extends CommitPublished {

  override def getState = {
    val success = for (tier1 \ tier2 <- claimHtlcSuccess) yield HideReady(tier1.tx) :: csvShowDelayed(tier1, tier2, commitTx) :: Nil
    val timeout = for (t1 \ t2 <- claimHtlcTimeout) yield HideDelayed(cltv(commitTx, t1.tx), t1.tx) :: csvShowDelayed(t1, t2, commitTx) :: Nil
    val main = for (t1 <- claimMainDelayed) yield ShowDelayed(csv(commitTx, t1.tx), t1.tx, commitTx, t1 -- t1, t1.tx.allOutputsAmount) :: Nil
    main.flatten ++ success.flatten ++ timeout.flatten
  }
}

case class RemoteCommitPublished(claimMain: Seq[ClaimP2WPKHOutputTx], claimHtlcSuccess: Seq[ClaimHtlcSuccessTx],
                                 claimHtlcTimeout: Seq[ClaimHtlcTimeoutTx], commitTx: Transaction) extends CommitPublished {

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

  def spendsFromRevoked(htlcTx: Transaction): Boolean =
    htlcTx.txIn.map(_.outPoint.txid).contains(commitTx.txid)

  override def getState = {
    val main = for (t1 <- claimMain) yield ShowReady(t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    val their = for (t1 <- claimTheirMainPenalty) yield ShowReady(t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    val penalty = for (t1 <- htlcPenalty) yield ShowReady(t1.tx, t1 -- t1, t1.tx.allOutputsAmount)
    main ++ their ++ penalty
  }
}

case class RevocationInfo(redeemScriptsToSigs: List[RedeemScriptAndSig],
                          claimMainTxSig: Option[ByteVector], claimPenaltyTxSig: Option[ByteVector], feeRate: Long,
                          dustLimit: Long, finalScriptPubKey: ByteVector, toSelfDelay: Int, localPubKey: PublicKey,
                          remoteRevocationPubkey: PublicKey, remoteDelayedPaymentKey: PublicKey) {

  lazy val dustLim = Satoshi(dustLimit)
  def makeClaimP2WPKHOutput(tx: Transaction) =
    Scripts.makeClaimP2WPKHOutputTx(tx, localPubKey,
      finalScriptPubKey, feeRate, dustLim)

  def makeHtlcPenalty(finder: PubKeyScriptIndexFinder)(redeemScript: ByteVector) =
    Scripts.makeHtlcPenaltyTx(finder, redeemScript, finalScriptPubKey, feeRate, dustLim)

  def makeMainPenalty(tx: Transaction) =
    Scripts.makeMainPenaltyTx(tx, remoteRevocationPubkey, finalScriptPubKey,
      toSelfDelay, remoteDelayedPaymentKey, feeRate, dustLim)
}

// COMMITMENTS

case class Htlc(incoming: Boolean, add: UpdateAddHtlc) {
  lazy val tuple: HTLCTuple = (incoming, add.id, add.amountMsat, add.paymentHash, add.expiry)
}

case class CommitmentSpec(feeratePerKw: Long, toLocalMsat: Long, toRemoteMsat: Long,
                          htlcs: Set[Htlc] = Set.empty, fulfilled: Set[HtlcAndFulfill] = Set.empty,
                          failed: Set[HtlcAndFail] = Set.empty, malformed: Set[Htlc] = Set.empty) {

  lazy val fulfilledIncoming = fulfilled collect { case Htlc(true, add) \ _ => add }
  lazy val fulfilledOutgoing = fulfilled collect { case Htlc(false, add) \ _ => add }

  def directedHtlcsAndSum(incoming: Boolean) = {
    val filtered = htlcs.filter(_.incoming == incoming)
    filtered -> filtered.map(_.add.amountMsat).sum
  }
}

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
                       defaultFinalScriptPubKey: ByteVector, dustLimit: Satoshi,
                       shaSeed: ByteVector, isFunder: Boolean) {

  lazy val delayedPaymentBasepoint = delayedPaymentKey.toPoint
  lazy val revocationBasepoint = revocationSecret.toPoint
  lazy val paymentBasepoint = paymentKey.toPoint
  lazy val htlcBasepoint = htlcKey.toPoint
}

case class WaitingForRevocation(nextRemoteCommit: RemoteCommit, sent: CommitSig, localCommitIndexSnapshot: Long)
case class LocalCommit(index: Long, spec: CommitmentSpec, htlcTxsAndSigs: Seq[HtlcTxAndSigs], commitTx: CommitTx)
case class RemoteCommit(index: Long, spec: CommitmentSpec, txOpt: Option[Transaction], remotePerCommitmentPoint: Point)
case class HtlcTxAndSigs(txinfo: TransactionWithInputInfo, localSig: ByteVector, remoteSig: ByteVector)
case class Changes(proposed: LNMessageVector, signed: LNMessageVector, acked: LNMessageVector)

sealed trait Commitments {
  val updateOpt: Option[ChannelUpdate]
  val localSpec: CommitmentSpec
  val myFullBalanceMsat: Long
  val channelId: ByteVector
  val startedAt: Long
}

case class ReducedState(spec: CommitmentSpec, canSendMsat: Long, canReceiveMsat: Long, myFeeSat: Long)
case class NormalCommits(localParams: LocalParams, remoteParams: AcceptChannel, localCommit: LocalCommit,
                         remoteCommit: RemoteCommit, localChanges: Changes, remoteChanges: Changes, localNextHtlcId: Long,
                         remoteNextHtlcId: Long, remoteNextCommitInfo: Either[WaitingForRevocation, Point], commitInput: InputInfo,
                         remotePerCommitmentSecrets: ShaHashesWithIndex, channelId: ByteVector, updateOpt: Option[ChannelUpdate],
                         channelFlags: Option[ChannelFlags], startedAt: Long) extends Commitments { me =>

  lazy val reducedRemoteState = {
    val reduced = CommitmentSpec.reduce(latestRemoteCommit.spec, remoteChanges.acked, localChanges.proposed)
    val commitFeeSat = Scripts.commitTxFee(remoteParams.dustLimitSat, reduced).amount
    val theirFeeSat = if (localParams.isFunder) 0L else commitFeeSat
    val myFeeSat = if (localParams.isFunder) commitFeeSat else 0L

    val canSendMsat = reduced.toRemoteMsat - (myFeeSat + remoteParams.channelReserveSatoshis) * 1000L
    val canReceiveMsat = reduced.toLocalMsat - (theirFeeSat + localParams.channelReserveSat) * 1000L
    ReducedState(reduced, canSendMsat, canReceiveMsat, myFeeSat)
  }

  lazy val localSpec = localCommit.spec
  lazy val myFullBalanceMsat = reducedRemoteState.spec.toRemoteMsat
  def latestRemoteCommit = remoteNextCommitInfo.left.toOption.map(_.nextRemoteCommit) getOrElse remoteCommit
  def localHasUnsignedOutgoing = localChanges.proposed.collectFirst { case u: UpdateAddHtlc => u }.isDefined
  def remoteHasUnsignedOutgoing = remoteChanges.proposed.collectFirst { case u: UpdateAddHtlc => u }.isDefined
  def addRemoteProposal(lm: LightningMessage) = me.modify(_.remoteChanges.proposed).using(_ :+ lm)
  def addLocalProposal(lm: LightningMessage) = me.modify(_.localChanges.proposed).using(_ :+ lm)
  def nextDummyReduced = addLocalProposal(Tools.nextDummyHtlc).reducedRemoteState

  def getHtlcCrossSigned(incomingRelativeToLocal: Boolean, htlcId: Long) = for {
    _ <- CommitmentSpec.findHtlcById(latestRemoteCommit.spec, htlcId, !incomingRelativeToLocal)
    htlcOut <- CommitmentSpec.findHtlcById(localSpec, htlcId, incomingRelativeToLocal)
  } yield htlcOut.add

  def ensureSenderCanAffordChange = {
    val reduced = CommitmentSpec.reduce(localSpec, localChanges.acked, remoteChanges.proposed)
    val feesSat = if (localParams.isFunder) 0L else Scripts.commitTxFee(localParams.dustLimit, reduced).amount
    if (reduced.toRemoteMsat - (feesSat + localParams.channelReserveSat) * 1000L < 0L) throw new LightningException
    me -> reduced
  }

  def sendFee(ratePerKw: Long) = {
    if (!localParams.isFunder) throw new LightningException
    val updateFeeMessage = UpdateFee(channelId, ratePerKw)
    val c1 = addLocalProposal(lm = updateFeeMessage)
    if (c1.reducedRemoteState.canSendMsat < 0L) None
    else Some(c1 -> updateFeeMessage)
  }

  def receiveFee(fee: UpdateFee) = {
    if (localParams.isFunder) throw new LightningException
    if (fee.feeratePerKw < minFeeratePerKw) throw new LightningException
    val c1 \ _ = addRemoteProposal(fee).ensureSenderCanAffordChange
    c1
  }

  def sendAdd(rd: RoutingData) = {
    // Let's compute the current commitment transaction *as seen by remote peer* with this change taken into account
    val add = UpdateAddHtlc(channelId, localNextHtlcId, rd.lastMsat, rd.pr.paymentHash, rd.lastExpiry, rd.onion.packet)
    val c1 = addLocalProposal(add).modify(_.localNextHtlcId).using(_ + 1)

    // This is their point of view so our outgoing HTLCs are their incoming
    val outHtlcs \ inFlight = c1.reducedRemoteState.spec.directedHtlcsAndSum(incoming = true)
    if (c1.reducedRemoteState.canSendMsat < 0L) throw CMDAddImpossible(rd, ERR_LOCAL_AMOUNT_HIGH)
    if (!c1.localParams.isFunder && c1.reducedRemoteState.canReceiveMsat < 0L) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_HIGH)
    if (UInt64(inFlight) > remoteParams.maxHtlcValueInFlightMsat) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_HIGH)
    if (rd.firstMsat < remoteParams.htlcMinimumMsat) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_LOW)
    if (outHtlcs.size > remoteParams.maxAcceptedHtlcs) throw CMDAddImpossible(rd, ERR_TOO_MANY_HTLC)
    c1 -> add
  }

  def receiveAdd(add: UpdateAddHtlc) = {
    // We should both check if WE can accept another HTLC and if PEER can send another HTLC
    // let's compute the current commitment *as seen by us* with this payment change taken into account
    val c1 \ reduced = addRemoteProposal(add).modify(_.remoteNextHtlcId).using(_ + 1).ensureSenderCanAffordChange
    // This is our point of view because `ensureSenderCanAffordChange` returns reduced local commits
    val inHtlcs \ inFlight = reduced.directedHtlcsAndSum(incoming = true)

    if (add.id != remoteNextHtlcId) throw new LightningException
    if (add.amountMsat < minHtlcValue.amount) throw new LightningException
    if (inHtlcs.size > localParams.maxAcceptedHtlcs) throw new LightningException
    if (UInt64(inFlight) > localParams.maxHtlcValueInFlightMsat) throw new LightningException
    c1
  }

  def receiveFulfill(fulfill: UpdateFulfillHtlc) =
    getHtlcCrossSigned(incomingRelativeToLocal = false, fulfill.id) match {
      case Some(add) if fulfill.paymentHash == add.paymentHash => addRemoteProposal(fulfill)
      case None => throw new LightningException("Peer has fulfilled non-cross-signed payment")
    }

  def sendFail(cmd: CMDFailHtlc) = {
    val fail = UpdateFailHtlc(channelId, cmd.id, cmd.reason)
    val notFound = getHtlcCrossSigned(incomingRelativeToLocal = true, cmd.id).isEmpty
    if (notFound) throw new LightningException else addLocalProposal(fail) -> fail
  }

  def sendFailMalformed(cmd: CMDFailMalformedHtlc) = {
    val failMalformed = UpdateFailMalformedHtlc(channelId, cmd.id, cmd.onionHash, cmd.code)
    val notFound = getHtlcCrossSigned(incomingRelativeToLocal = true, htlcId = cmd.id).isEmpty
    if (notFound) throw new LightningException else addLocalProposal(failMalformed) -> failMalformed
  }

  def receiveFail(fail: UpdateFailHtlc) = {
    val notFound = getHtlcCrossSigned(incomingRelativeToLocal = false, fail.id).isEmpty
    if (notFound) throw new LightningException else addRemoteProposal(fail)
  }

  def receiveFailMalformed(fail: UpdateFailMalformedHtlc) = {
    val notFound = getHtlcCrossSigned(incomingRelativeToLocal = false, fail.id).isEmpty
    if (fail.failureCode.&(FailureMessageCodecs.BADONION) == 0) throw new LightningException
    if (notFound) throw new LightningException else addRemoteProposal(fail)
  }

  def sendCommit(remoteNextPerCommitmentPoint: Point) = {
    val htlcKey = Generators.derivePrivKey(localParams.htlcKey, remoteNextPerCommitmentPoint)
    val spec = CommitmentSpec.reduce(remoteCommit.spec, remoteChanges.acked, localChanges.proposed)

    val (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs, _, _) =
      Helpers.makeRemoteTxs(remoteCommit.index + 1, localParams, remoteParams,
        commitInput, remoteNextPerCommitmentPoint, spec)

    // Generate signatures
    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(htlcKey)(info)

    // Update commitment data
    val remoteChanges1 = remoteChanges.copy(acked = Vector.empty, signed = remoteChanges.acked)
    val localChanges1 = localChanges.copy(proposed = Vector.empty, signed = localChanges.proposed)
    val commitSig = CommitSig(channelId, Scripts.sign(localParams.fundingPrivKey)(remoteCommitTx), htlcSigs.toList)
    val remoteCommit1 = RemoteCommit(remoteCommit.index + 1, spec, Some(remoteCommitTx.tx), remoteNextPerCommitmentPoint)
    val wait = WaitingForRevocation(nextRemoteCommit = remoteCommit1, commitSig, localCommitIndexSnapshot = localCommit.index)
    copy(remoteNextCommitInfo = Left(wait), localChanges = localChanges1, remoteChanges = remoteChanges1) -> commitSig
  }

  def receiveCommit(commit: CommitSig) = {
    val spec = CommitmentSpec.reduce(localSpec, localChanges.acked, remoteChanges.proposed)
    val localPerCommitmentSecret = Generators.perCommitSecret(localParams.shaSeed, localCommit.index)
    val localPerCommitmentPoint = Generators.perCommitPoint(localParams.shaSeed, localCommit.index + 1)
    val localNextPerCommitmentPoint = Generators.perCommitPoint(localParams.shaSeed, localCommit.index + 2)
    val remoteHtlcPubkey = Generators.derivePubKey(remoteParams.htlcBasepoint, localPerCommitmentPoint)
    val localHtlcKey = Generators.derivePrivKey(localParams.htlcKey, localPerCommitmentPoint)

    val (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
      Helpers.makeLocalTxs(localCommit.index + 1, localParams,
        remoteParams, commitInput, localPerCommitmentPoint, spec)

    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val signedLocalCommitTx = Scripts.addSigs(localCommitTx, localParams.fundingPrivKey.publicKey,
      remoteParams.fundingPubkey, Scripts.sign(localParams.fundingPrivKey)(localCommitTx), commit.signature)

    if (commit.htlcSignatures.size != sortedHtlcTxs.size) throw new LightningException
    if (Scripts.checkValid(signedLocalCommitTx).isFailure) throw new LightningException
    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(localHtlcKey)(info)
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

    val localCommit1 = LocalCommit(localCommit.index + 1, spec, htlcTxsAndSigs, signedLocalCommitTx)
    val remoteChanges1 = remoteChanges.copy(proposed = Vector.empty, acked = remoteChanges.acked ++ remoteChanges.proposed)
    val c1 = copy(localChanges = localChanges.copy(acked = Vector.empty), remoteChanges = remoteChanges1, localCommit = localCommit1)
    c1 -> RevokeAndAck(channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
  }

  def receiveRevocation(rev: RevokeAndAck) = remoteNextCommitInfo match {
    case Left(_) if remoteCommit.remotePerCommitmentPoint != rev.perCommitmentSecret.toPoint =>
      throw new LightningException("Peer has supplied a wrong per commitment secret")

    case Left(wait) =>
      val nextIndex = ShaChain.largestTxIndex - remoteCommit.index
      val secrets1 = ShaChain.addHash(remotePerCommitmentSecrets, rev.perCommitmentSecret.toBin.toArray, nextIndex)
      val localChanges1 = localChanges.copy(signed = Vector.empty, acked = localChanges.acked ++ localChanges.signed)
      val remoteChanges1 = remoteChanges.copy(signed = Vector.empty)

      copy(localChanges = localChanges1, remoteChanges = remoteChanges1, remoteCommit = wait.nextRemoteCommit,
        remoteNextCommitInfo = Right(rev.nextPerCommitmentPoint), remotePerCommitmentSecrets = secrets1)

    case _ =>
      // Unexpected revocation
      throw new LightningException
  }
}

case class HostedCommits(announce: NodeAnnouncement, lastCrossSignedState: LastCrossSignedState, clientUpdatesSoFar: Long,
                         hostUpdatesSoFar: Long, reSentUpdates: Int, clientChanges: LNMessageVector, hostChanges: LNMessageVector,
                         localSpec: CommitmentSpec, updateOpt: Option[ChannelUpdate], localError: Option[Error], remoteError: Option[Error],
                         startedAt: Long) extends Commitments with ChannelData { me =>

  def isInErrorState = localError.isDefined || remoteError.isDefined
  def addHostProposal(lm: LightningMessage) = copy(hostChanges = hostChanges :+ lm, hostUpdatesSoFar = hostUpdatesSoFar + 1, reSentUpdates = 0)
  def addClientProposal(lm: LightningMessage) = copy(clientChanges = clientChanges :+ lm, clientUpdatesSoFar = clientUpdatesSoFar + 1, reSentUpdates = 0)
  lazy val initMsg = InvokeHostedChannel(chainHash, lastCrossSignedState.lastRefundScriptPubKey)
  val myFullBalanceMsat = localSpec.toLocalMsat
  val channelId = announce.hostedChanId
  val mustReply = reSentUpdates == 0

  def sendAdd(rd: RoutingData) = {
    // Let's add this change and see if the new state violates any of the constraints including those imposed by host on us
    val add = UpdateAddHtlc(channelId, clientUpdatesSoFar + 1, rd.lastMsat, rd.pr.paymentHash, rd.lastExpiry, rd.onion.packet)
    val reduced = CommitmentSpec.reduce(localSpec, clientChanges :+ add, hostChanges)
    val inHtlcs \ inFlight = reduced.directedHtlcsAndSum(incoming = false)

    if (reduced.toLocalMsat < 0L) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_HIGH)
    if (UInt64(inFlight) > lastCrossSignedState.initHostedChannel.maxHtlcValueInFlightMsat) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_HIGH)
    if (rd.firstMsat < lastCrossSignedState.initHostedChannel.htlcMinimumMsat) throw CMDAddImpossible(rd, ERR_REMOTE_AMOUNT_LOW)
    if (inHtlcs.size > lastCrossSignedState.initHostedChannel.maxAcceptedHtlcs) throw CMDAddImpossible(rd, ERR_TOO_MANY_HTLC)
    addClientProposal(add) -> add
  }

  def receiveAdd(add: UpdateAddHtlc) = {
    val reduced = CommitmentSpec.reduce(localSpec, clientChanges, hostChanges :+ add)
    val inHtlcs \ inFlight = reduced.directedHtlcsAndSum(incoming = true)

    if (reduced.toRemoteMsat < 0L) throw new LightningException
    if (add.id != hostUpdatesSoFar + 1) throw new LightningException
    if (inHtlcs.size > lastCrossSignedState.initHostedChannel.maxAcceptedHtlcs) throw new LightningException
    if (UInt64(inFlight) > lastCrossSignedState.initHostedChannel.maxHtlcValueInFlightMsat) throw new LightningException
    addHostProposal(add)
  }

  def makeSignedStateUpdate = {
    val reduced = CommitmentSpec.reduce(localSpec, clientChanges, hostChanges)
    val so = StateOverride(reduced.toLocalMsat, broadcaster.currentBlockDay, clientUpdatesSoFar, hostUpdatesSoFar)
    val htlcs: List[HTLCTuple] = for (Htlc(incoming, add) <- reduced.htlcs.toList) yield (incoming, add.id, add.amountMsat, add.paymentHash, add.expiry)
    StateUpdate(so, htlcs).signed(announce.hostedChanId, lastCrossSignedState.lastRefundScriptPubKey, lastCrossSignedState.initHostedChannel, nodePrivateKey)
  }

  def receiveFulfill(fulfill: UpdateFulfillHtlc) =
    CommitmentSpec.findHtlcById(localSpec, fulfill.id, isIncoming = false) match {
      case Some(htlc) if fulfill.paymentHash == htlc.add.paymentHash => addHostProposal(fulfill)
      case None => throw new LightningException("Peer has fulfilled non-existing payment")
    }

  def receiveFail(fail: UpdateFailHtlc) = {
    val notFound = CommitmentSpec.findHtlcById(localSpec, fail.id, isIncoming = false).isEmpty
    if (notFound) throw new LightningException else addHostProposal(fail)
  }

  def receiveFailMalformed(fail: UpdateFailMalformedHtlc) = {
    val notFound = CommitmentSpec.findHtlcById(localSpec, fail.id, isIncoming = false).isEmpty
    if (fail.failureCode.&(FailureMessageCodecs.BADONION) == 0) throw new LightningException
    if (notFound) throw new LightningException else addHostProposal(fail)
  }
}