package com.lightning.walletapp.ln

import com.softwaremill.quicklens._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.ln.NormalChannel._
import com.lightning.walletapp.ln.ChanErrorCodes._
import fr.acinq.bitcoin.Protocol.Zeroes
import java.util.concurrent.Executors
import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector
import scala.util.Success

import com.lightning.walletapp.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex, Sphinx}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import com.lightning.walletapp.ln.Helpers.{Closing, Funding}
import com.lightning.walletapp.ln.Tools.{none, runAnd}
import fr.acinq.bitcoin.Crypto.{Point, Scalar}
import fr.acinq.bitcoin.{Satoshi, Transaction}


abstract class Channel(val isHosted: Boolean) extends StateMachine[ChannelData] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def process(change: Any) = Future(me doProcess change) onFailure { case runtimeFailure => events onException me -> runtimeFailure }
  def shouldRenew(u: ChannelUpdate) = getCommits.flatMap(_.updateOpt).exists(u0 => u0.shortChannelId == u.shortChannelId && u0.timestamp < u.timestamp)

  def getCommits: Option[Commitments] = data match {
    case normal: HasNormalCommits => Some(normal.commitments)
    case hosted: HostedCommits => Some(hosted)
    case _ => None
  }

  def BECOME(data1: ChannelData, state1: String) = runAnd(me) {
    // Transition must always be defined before vars are updated
    val trans = Tuple4(me, data1, state, state1)
    super.become(data1, state1)
    events onBecome trans
  }

  def UPDATA(d1: ChannelData) = BECOME(d1, state)
  def STORE(data: ChannelData): ChannelData
  def SEND(msg: LightningMessage): Unit

  def inFlightHtlcs: Set[Htlc]
  def estimateCanReceive: Long
  def estimateCanSend: Long

  def estimateNextUsefulCapacity =
    // In-flight HTLCs are subtracted here
    estimateCanSend + estimateCanReceive

  var permanentOffline: Boolean = true
  var listeners: Set[ChannelListener] = _
  val events: ChannelListener = new ChannelListener {
    override def onProcessSuccess = { case ps => for (lst <- listeners if lst.onProcessSuccess isDefinedAt ps) lst onProcessSuccess ps }
    override def onException = { case failure => for (lst <- listeners if lst.onException isDefinedAt failure) lst onException failure }
    override def onBecome = { case transition => for (lst <- listeners if lst.onBecome isDefinedAt transition) lst onBecome transition }
    override def fulfillReceived(upd: UpdateFulfillHtlc) = for (lst <- listeners) lst fulfillReceived upd
    override def outPaymentAccepted(rd: RoutingData) = for (lst <- listeners) lst outPaymentAccepted rd
    override def onSettled(cs: Commitments) = for (lst <- listeners) lst.onSettled(cs)
  }
}

abstract class NormalChannel extends Channel(isHosted = false) { me =>
  def fundTxId = data match {case hasSome: HasNormalCommits => hasSome.commitments.commitInput.outPoint.txid case _ => ByteVector.empty }
  def estimateCanSend = getCommits collect { case nc: NormalCommits => nc.nextDummyReduced.canSendMsat + LNParams.minCapacityMsat } getOrElse 0L
  def inFlightHtlcs = getCommits collect { case nc: NormalCommits => nc.reducedRemoteState.spec.htlcs } getOrElse Set.empty[Htlc]
  def estimateCanReceive = getCommits collect { case nc: NormalCommits => nc.nextDummyReduced.canReceiveMsat } getOrElse 0L

  def CLOSEANDWATCH(close: ClosingData): Unit
  def ASKREFUNDPEER(some: HasNormalCommits, point: Point): Unit
  def GETREV(cs: NormalCommits, tx: Transaction): Option[RevokedCommitPublished]
  def REV(cs: NormalCommits, rev: RevokeAndAck): Unit

  def doProcess(change: Any) = {
    Tuple3(data, change, state) match {
      case (InitData(announce), cmd: CMDOpenChannel, WAIT_FOR_INIT) =>
        BECOME(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT) SEND OpenChannel(LNParams.chainHash, cmd.tempChanId,
          cmd.fundingSat, cmd.pushMsat, cmd.localParams.dustLimit.amount, cmd.localParams.maxHtlcValueInFlightMsat,
          cmd.localParams.channelReserveSat, LNParams.minHtlcValue.amount, cmd.initialFeeratePerKw, cmd.localParams.toSelfDelay,
          cmd.localParams.maxAcceptedHtlcs, cmd.localParams.fundingPrivKey.publicKey, cmd.localParams.revocationBasepoint,
          cmd.localParams.paymentBasepoint, cmd.localParams.delayedPaymentBasepoint, cmd.localParams.htlcBasepoint,
          Generators.perCommitPoint(cmd.localParams.shaSeed, index = 0L), cmd.channelFlags)


      case (InitData(announce), Tuple2(localParams: LocalParams, open: OpenChannel), WAIT_FOR_INIT) =>
        if (LNParams.chainHash != open.chainHash) throw new LightningException("They have provided a wrong chain hash")
        if (open.channelFlags.isPublic) throw new LightningException("They are offering a public channel and we only support private ones")
        if (open.pushMsat > 1000L * open.fundingSatoshis) throw new LightningException("They are trying to push more than proposed capacity")
        if (open.dustLimitSatoshis > open.channelReserveSatoshis) throw new LightningException("Their dust limit exceeds their channel reserve")
        if (open.feeratePerKw < LNParams.minFeeratePerKw) throw new LightningException("Their proposed opening on-chain fee is too small")
        if (open.toSelfDelay > LNParams.maxToSelfDelay) throw new LightningException("Their toSelfDelay is too high")
        if (open.dustLimitSatoshis < 546L) throw new LightningException("Their on-chain dust limit is too low")
        if (open.maxAcceptedHtlcs > 483) throw new LightningException("They can accept too many payments")
        if (open.pushMsat < 0) throw new LightningException("Their pushMsat is negative")

        val toLocalMsat \ toRemoteMsat = (open.pushMsat, open.fundingSatoshis * 1000L - open.pushMsat)
        if (toLocalMsat <= open.channelReserveSatoshis * 1000L && toRemoteMsat <= open.channelReserveSatoshis * 1000L)
          throw new LightningException("Both toLocal and toRemote amounts are less than total channel reserve")

        if (open.fundingSatoshis / open.channelReserveSatoshis < LNParams.channelReserveToFundingRatio / 5)
          throw new LightningException("Their proposed channel reserve is too high relative to capacity")

        val remoteParams = AcceptChannel(open.temporaryChannelId, open.dustLimitSatoshis, open.maxHtlcValueInFlightMsat,
          open.channelReserveSatoshis, open.htlcMinimumMsat, minimumDepth = 6, open.toSelfDelay, open.maxAcceptedHtlcs,
          open.fundingPubkey, open.revocationBasepoint, open.paymentBasepoint, open.delayedPaymentBasepoint,
          open.htlcBasepoint, open.firstPerCommitmentPoint)

        val firstPerCommitPoint = Generators.perCommitPoint(localParams.shaSeed, 0L)
        val wait = WaitFundingCreatedRemote(announce, localParams, remoteParams, open)
        BECOME(wait, WAIT_FOR_FUNDING) SEND AcceptChannel(open.temporaryChannelId, localParams.dustLimit.amount,
          localParams.maxHtlcValueInFlightMsat, localParams.channelReserveSat, LNParams.minHtlcValue.amount,
          minimumDepth = LNParams.minDepth, localParams.toSelfDelay, localParams.maxAcceptedHtlcs,
          localParams.fundingPrivKey.publicKey, localParams.revocationBasepoint,
          localParams.paymentBasepoint, localParams.delayedPaymentBasepoint,
          localParams.htlcBasepoint, firstPerCommitPoint)


      case (WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT) if accept.temporaryChannelId == cmd.tempChanId =>
        if (accept.dustLimitSatoshis > cmd.localParams.channelReserveSat) throw new LightningException("Our channel reserve is less than their dust")
        if (UInt64(100000000L) > accept.maxHtlcValueInFlightMsat) throw new LightningException("Their maxHtlcValueInFlightMsat is too low")
        if (accept.channelReserveSatoshis > cmd.fundingSat / 10) throw new LightningException("Their proposed reserve is too high")
        if (accept.toSelfDelay > LNParams.maxToSelfDelay) throw new LightningException("Their toSelfDelay is too high")
        if (accept.dustLimitSatoshis < 546L) throw new LightningException("Their on-chain dust limit is too low")
        if (accept.htlcMinimumMsat > 100000L) throw new LightningException("Their htlcMinimumMsat is too high")
        if (accept.maxAcceptedHtlcs > 483) throw new LightningException("They can accept too many payments")
        if (accept.maxAcceptedHtlcs < 1) throw new LightningException("They can accept too few payments")
        if (accept.minimumDepth > 6L) throw new LightningException("Their minimumDepth is too high")
        BECOME(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING)


      // They have proposed us a channel, we have agreed to their terms and now they have created a funding tx which we should check
      case (WaitFundingCreatedRemote(announce, localParams, accept, open), FundingCreated(_, txid, outIndex, remoteSig), WAIT_FOR_FUNDING) =>

        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
          Funding.makeFirstCommitTxs(localParams, open.fundingSatoshis, open.pushMsat,
            open.feeratePerKw, accept, txid, outIndex, open.firstPerCommitmentPoint)

        val signedLocalCommitTx =
          Scripts.addSigs(localCommitTx, localParams.fundingPrivKey.publicKey,
            accept.fundingPubkey, Scripts.sign(localParams.fundingPrivKey)(localCommitTx), remoteSig)

        if (Scripts.checkValid(signedLocalCommitTx).isSuccess) {
          val localSigOfRemoteTx = Scripts.sign(localParams.fundingPrivKey)(remoteCommitTx)
          val fundingSigned = FundingSigned(Tools.toLongId(txid, outIndex), localSigOfRemoteTx)
          val rc = RemoteCommit(index = 0L, remoteSpec, Some(remoteCommitTx.tx), open.firstPerCommitmentPoint)
          val core = WaitFundingSignedCore(localParams, fundingSigned.channelId, Some(open.channelFlags), accept, localSpec, rc)
          BECOME(WaitBroadcastRemoteData(announce, core, core makeCommitments signedLocalCommitTx), WAIT_FUNDING_DONE) SEND fundingSigned
        } else throw new LightningException


      // LOCAL FUNDER FLOW


      case (WaitFundingData(announce, cmd, accept), CMDFunding(fundTx), WAIT_FOR_FUNDING) =>
        // We are the funder and user made a funding, let peer sign a first commit so we can broadcast a funding later
        if (fundTx.txOut(cmd.batch.fundOutIdx).amount.amount != cmd.batch.fundingAmountSat) throw new LightningException
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Funding.makeFirstCommitTxs(cmd.localParams, cmd.fundingSat,
          cmd.pushMsat, cmd.initialFeeratePerKw, accept, fundTx.hash, cmd.batch.fundOutIdx, accept.firstPerCommitmentPoint)

        val longId = Tools.toLongId(fundTx.hash, cmd.batch.fundOutIdx)
        val localSigOfRemoteTx = Scripts.sign(cmd.localParams.fundingPrivKey)(remoteCommitTx)
        val fundingCreated = FundingCreated(cmd.tempChanId, fundTx.hash, cmd.batch.fundOutIdx, localSigOfRemoteTx)
        val firstRemoteCommit = RemoteCommit(index = 0L, remoteSpec, Some(remoteCommitTx.tx), accept.firstPerCommitmentPoint)
        val core = WaitFundingSignedCore(cmd.localParams, longId, Some(cmd.channelFlags), accept, localSpec, firstRemoteCommit)
        BECOME(WaitFundingSignedData(announce, core, localCommitTx, fundTx), WAIT_FUNDING_SIGNED) SEND fundingCreated


      // They have signed our first commit, we can broadcast a local funding tx
      case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED) =>
        val localSignature = Scripts.sign(wait.core.localParams.fundingPrivKey)(wait.localCommitTx)
        val localKey \ remoteKey = wait.core.localParams.fundingPrivKey.publicKey -> wait.core.remoteParams.fundingPubkey
        val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, localKey, remoteKey, localSignature, remote.signature)
        val wait1 = WaitFundingDoneData(wait.announce, None, None, wait.fundingTx, wait.core makeCommitments signedLocalCommitTx)
        if (Scripts.checkValid(signedLocalCommitTx).isSuccess) BECOME(wait1, WAIT_FUNDING_DONE) else throw new LightningException


      // BECOMING OPEN


      // We have agreed to proposed incoming channel and they have published a funding tx, we now wait for confirmation
      case (wait: WaitBroadcastRemoteData, CMDSpent(fundTx), WAIT_FUNDING_DONE | SLEEPING) if fundTxId == fundTx.txid =>
        val wait1 = me STORE WaitFundingDoneData(wait.announce, None, wait.their, fundTx, wait.commitments)
        val isZeroConfSpendablePush = wait.commitments.channelFlags.exists(_.isZeroConfSpendablePush)
        if (isZeroConfSpendablePush) me UPDATA wait1 doProcess CMDConfirmed(fundTx)
        else me UPDATA wait1


      // We have agreed to proposed incoming channel and they have published a funding tx, we see it included in a block right away
      case (wait: WaitBroadcastRemoteData, CMDConfirmed(fundTx), WAIT_FUNDING_DONE | SLEEPING) if fundTxId == fundTx.txid =>
        val wait1 = me STORE WaitFundingDoneData(wait.announce, None, wait.their, fundTx, wait.commitments)
        // Send CMDConfirmed again so we can properly react in new state
        me UPDATA wait1 doProcess CMDConfirmed(fundTx)


      case (waitBroadcast: WaitBroadcastRemoteData, their: FundingLocked, WAIT_FUNDING_DONE) =>
        // For turbo chans, their fundingLocked may arrive faster than onchain event, keep it
        // no need to store their fundingLocked because it gets re-sent on reconnect
        me UPDATA waitBroadcast.copy(their = Some apply their)


      case (wait: WaitFundingDoneData, their: FundingLocked, WAIT_FUNDING_DONE) =>
        // No need to store their fundingLocked because it gets re-sent on reconnect
        if (wait.our.isEmpty) me UPDATA wait.copy(their = Some apply their)
        else becomeOpen(wait, their)


      case (wait: WaitFundingDoneData, CMDConfirmed(fundTx), SLEEPING) if fundTxId == fundTx.txid =>
        // We have got an idempotent on-chain event while peer is offline, store it for further broadcast
        val ourFirstFundingLockedOpt = Some apply makeFirstFundingLocked(wait)
        val wait1 = wait.copy(our = ourFirstFundingLockedOpt)
        me UPDATA STORE(wait1)


      case (wait: WaitFundingDoneData, CMDConfirmed(fundTx), WAIT_FUNDING_DONE) if fundTxId == fundTx.txid =>
        // We have got an idempotent on-chain event while peer is online, inform peer and maybe become open

        val ourFirstFundingLocked = makeFirstFundingLocked(wait)
        val wait1 = wait.copy(our = Some apply ourFirstFundingLocked)
        if (wait.their.isEmpty) me UPDATA STORE(wait1) SEND ourFirstFundingLocked
        else becomeOpen(wait, wait.their.get) SEND ourFirstFundingLocked


      case (wait: WaitFundingDoneData, CMDConfirmed(otherTx), _) if wait doubleSpendsFunding otherTx =>
        // Other tx has successfully double-spent our channel funding, let user know on UI
        throw new Exception("Channel funding transaction was double-spent!")


      // OPEN MODE


      case (norm: NormalData, CMDChannelUpdate(upd), OPEN | SLEEPING) =>
        // Got an empty ChannelUpdate with shortChannelId or a final one
        val d1 = norm.modify(_.commitments.updateOpt) setTo Some(upd)
        // Do not trigger listeners for this update
        data = me STORE d1


      case (norm: NormalData, add: UpdateAddHtlc, OPEN) =>
        // Got new incoming HTLC, put it to changes without storing for now
        me UPDATA norm.copy(commitments = norm.commitments receiveAdd add)


      case (norm: NormalData, fulfill: UpdateFulfillHtlc, OPEN) =>
        // Got a fulfill for an outgoing HTLC we have sent to them earlier
        me UPDATA norm.copy(commitments = norm.commitments receiveFulfill fulfill)
        events fulfillReceived fulfill


      case (norm: NormalData, fail: UpdateFailHtlc, OPEN) =>
        // Got a failure for an outgoing HTLC we have sent to them earlier
        me UPDATA norm.copy(commitments = norm.commitments receiveFail fail)


      case (norm: NormalData, fail: UpdateFailMalformedHtlc, OPEN) =>
        // Got 'malformed' failure for an outgoing HTLC we have sent to them earlier
        me UPDATA norm.copy(commitments = norm.commitments receiveFailMalformed fail)


      case (norm: NormalData, rd: RoutingData, OPEN) if isOperational(me) =>
        // We can send a new HTLC when channel is both operational and online

        val c1 \ updateAddHtlc = norm.commitments sendAdd rd
        me UPDATA norm.copy(commitments = c1) SEND updateAddHtlc
        events outPaymentAccepted rd
        doProcess(CMDProceed)


      case (norm: NormalData, cmd: CMDFulfillHtlc, OPEN) =>
        // We're fulfilling an HTLC we got from them earlier

        for {
          // this is a special case where we don't throw if cross signed HTLC is not found
          add <- norm.commitments.getHtlcCrossSigned(incomingRelativeToLocal = true, cmd.add.id)
          // such a case may happen when we have already fulfilled it just before connection got lost
          updateFulfillHtlc = UpdateFulfillHtlc(norm.commitments.channelId, cmd.add.id, cmd.preimage)

          if updateFulfillHtlc.paymentHash == add.paymentHash
          c1 = norm.commitments addLocalProposal updateFulfillHtlc
        } me UPDATA norm.copy(commitments = c1) SEND updateFulfillHtlc


      case (norm: NormalData, cmd: CMDFailHtlc, OPEN) =>
        val c1 \ updateFailHtlc = norm.commitments sendFail cmd
        me UPDATA norm.copy(commitments = c1) SEND updateFailHtlc


      case (norm: NormalData, cmd: CMDFailMalformedHtlc, OPEN) =>
        val c1 \ updateFailMalformedHtlс = norm.commitments sendFailMalformed cmd
        me UPDATA norm.copy(commitments = c1) SEND updateFailMalformedHtlс


      case (norm: NormalData, CMDHTLCProcess, OPEN) =>
        // Fail or fulfill incoming HTLCs

        for {
          Htlc(false, add) <- norm.commitments.remoteCommit.spec.htlcs
          // We don't want to receive a payment into a channel we have originally sent it from in an attempt to rebalance
          isLoop = norm.commitments.localSpec.htlcs.exists(htlc => !htlc.incoming && htlc.add.paymentHash == add.paymentHash)
        } me doProcess resolveHtlc(LNParams.nodePrivateKey, add, LNParams.bag, isLoop)
        // And sign changes once done because CMDFail/Fulfill above don't do that
        doProcess(CMDProceed)


      case (norm: NormalData, CMDProceed, OPEN)
        // Only if we have a point and something to sign
        if norm.commitments.remoteNextCommitInfo.isRight &&
          (norm.commitments.localChanges.proposed.nonEmpty ||
            norm.commitments.remoteChanges.acked.nonEmpty) =>

        // Propose new remote commit via commit tx sig
        val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
        val c1 \ commitSig = norm.commitments sendCommit nextRemotePoint
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 SEND commitSig


      case (norm: NormalData, sig: CommitSig, OPEN) =>
        // We received a commit sig from them, can update local commit
        val c1 \ revokeAndAck = norm.commitments receiveCommit sig
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 SEND revokeAndAck
        // Clear remote commit first
        doProcess(CMDProceed)
        events.onSettled(c1)


      case (norm: NormalData, rev: RevokeAndAck, OPEN) =>
        // We received a revocation because we sent a commit sig
        val c1 = norm.commitments receiveRevocation rev
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 doProcess CMDHTLCProcess
        // We should use an old commit here
        REV(norm.commitments, rev)


      case (norm: NormalData, fee: UpdateFee, OPEN) if !norm.commitments.localParams.isFunder =>
        // It is their duty to update fees when we are a fundee, this will be persisted later
        me UPDATA norm.copy(commitments = norm.commitments receiveFee fee)


      case (norm: NormalData, CMDFeerate(satPerKw), OPEN) if norm.commitments.localParams.isFunder =>
        val shouldUpdate = LNParams.shouldUpdateFee(satPerKw, norm.commitments.localSpec.feeratePerKw)
        if (shouldUpdate) norm.commitments sendFee satPerKw foreach { case c1 \ feeUpdateMessage =>
          // We send a fee update if current chan unspendable reserve + commitTx fee can afford it
          // otherwise we fail silently in hope that fee will drop or we will receive a payment
          me UPDATA norm.copy(commitments = c1) SEND feeUpdateMessage
          doProcess(CMDProceed)
        }


      // SHUTDOWN in WAIT_FUNDING_DONE


      case (wait: WaitFundingDoneData, CMDShutdown(scriptPubKey), WAIT_FUNDING_DONE) =>
        val finalScriptPubKey = scriptPubKey getOrElse wait.commitments.localParams.defaultFinalScriptPubKey
        val localShutdown = Shutdown(wait.commitments.channelId, scriptPubKey = finalScriptPubKey)
        val norm = NormalData(wait.announce, wait.commitments, Some(localShutdown), None)
        BECOME(me STORE norm, OPEN) SEND localShutdown


      case (wait: WaitFundingDoneData, CMDShutdown(scriptPubKey), SLEEPING) =>
        val finalScriptPubKey = scriptPubKey getOrElse wait.commitments.localParams.defaultFinalScriptPubKey
        val localShutdown = Shutdown(wait.commitments.channelId, scriptPubKey = finalScriptPubKey)
        val norm = NormalData(wait.announce, wait.commitments, Some(localShutdown), None)
        BECOME(me STORE norm, SLEEPING) SEND localShutdown


      case (wait: WaitFundingDoneData, remote: Shutdown, WAIT_FUNDING_DONE) =>
        val localShutdown = Shutdown(wait.commitments.channelId, wait.commitments.localParams.defaultFinalScriptPubKey)
        val norm = NormalData(wait.announce, wait.commitments, Some(localShutdown), Some(remote), None)
        BECOME(me STORE norm, OPEN) SEND localShutdown
        doProcess(CMDProceed)


      // SHUTDOWN in OPEN


      case (norm @ NormalData(announce, commitments, our, their, txOpt), CMDShutdown(scriptPubKey), OPEN | SLEEPING) =>
        if (commitments.localHasUnsignedOutgoing || our.isDefined || their.isDefined) startLocalClose(some = norm) else {
          val shutdown = Shutdown(commitments.channelId, scriptPubKey getOrElse commitments.localParams.defaultFinalScriptPubKey)
          val norm1 = me STORE NormalData(announce, commitments, Some(shutdown), their, txOpt)
          me UPDATA norm1 SEND shutdown
        }


      // Either they initiate a shutdown or respond to the one we have sent
      // should sign our unsigned outgoing HTLCs if present and then proceed with shutdown
      case (NormalData(announce, commitments, our, None, txOpt), remote: Shutdown, OPEN) =>
        val norm = NormalData(announce, commitments, our, remoteShutdown = Some(remote), txOpt)
        if (commitments.remoteHasUnsignedOutgoing) startLocalClose(norm)
        else me UPDATA norm doProcess CMDProceed


      // We have nothing to sign so check for valid shutdown state, only consider this if we have nothing in-flight
      case (NormalData(announce, commitments, our, their, txOpt), CMDProceed, OPEN) if inFlightHtlcs.isEmpty =>

        our -> their match {
          case Some(ourSig) \ Some(theirSig) if commitments.localParams.isFunder =>
            // Got both shutdowns without HTLCs in-flight so can send a first closing since we are the funder
            val firstProposed = Closing.makeFirstClosing(commitments, ourSig.scriptPubKey, theirSig.scriptPubKey)
            val neg = NegotiationsData(announce, commitments, ourSig, theirSig, firstProposed :: Nil)
            BECOME(me STORE neg, NEGOTIATIONS) SEND firstProposed.localClosingSigned

          case Some(ourSig) \ Some(theirSig) =>
            // Got both shutdowns without HTLCs in-flight so wait for funder's proposal
            val neg = NegotiationsData(announce, commitments, ourSig, theirSig, Nil)
            BECOME(me STORE neg, NEGOTIATIONS)

          case None \ Some(theirSig) =>
            // We have previously received their Shutdown so can respond
            // send CMDProceed once to make sure we still have nothing to sign
            val localShutdown = Shutdown(commitments.channelId, commitments.localParams.defaultFinalScriptPubKey)
            val norm1 = me STORE NormalData(announce, commitments, Some(localShutdown), Some(theirSig), txOpt)
            me UPDATA norm1 SEND localShutdown
            doProcess(CMDProceed)

          // Not ready
          case _ =>
        }


      // SYNC and REFUNDING MODE


      case (ref: RefundingData, cr: ChannelReestablish, REFUNDING) =>
        cr.myCurrentPerCommitmentPoint -> ref.remoteLatestPoint match {
          case _ \ Some(ourSavedPoint) => ASKREFUNDPEER(ref, ourSavedPoint)
          case Some(theirPoint) \ _ => ASKREFUNDPEER(ref, theirPoint)
          case _ => // They don't support data-loss-protect
        }


      case (some: HasNormalCommits, cr: ChannelReestablish, SLEEPING)
        // GUARD: their nextRemoteRevocationNumber is unexpectedly too far away
        if some.commitments.localCommit.index < cr.nextRemoteRevocationNumber && cr.myCurrentPerCommitmentPoint.isDefined =>
        val secret = Generators.perCommitSecret(some.commitments.localParams.shaSeed, cr.nextRemoteRevocationNumber - 1)
        if (cr.yourLastPerCommitmentSecret contains secret) ASKREFUNDPEER(some, cr.myCurrentPerCommitmentPoint.get)
        else throw new LightningException


      case (norm: NormalData, cr: ChannelReestablish, SLEEPING) =>
        // If next_local_commitment_number is 1 in both the channel_reestablish it sent
        // and received, then the node MUST retransmit funding_locked, otherwise it MUST NOT
        if (cr.nextLocalCommitmentNumber == 1 && norm.commitments.localCommit.index == 0)
          if (norm.localShutdown.isEmpty && norm.remoteShutdown.isEmpty)
            me SEND makeFirstFundingLocked(norm)

        // First we clean up unacknowledged updates
        val localDelta = norm.commitments.localChanges.proposed collect { case _: UpdateAddHtlc => true }
        val remoteDelta = norm.commitments.remoteChanges.proposed collect { case _: UpdateAddHtlc => true }
        val c1 = norm.commitments.modifyAll(_.localChanges.proposed, _.remoteChanges.proposed).setTo(Vector.empty)
          .modify(_.remoteNextHtlcId).using(_ - remoteDelta.size).modify(_.localNextHtlcId).using(_ - localDelta.size)

        def maybeResendRevocation = if (c1.localCommit.index == cr.nextRemoteRevocationNumber + 1) {
          val localPerCommitmentSecret = Generators.perCommitSecret(c1.localParams.shaSeed, c1.localCommit.index - 1)
          val localNextPerCommitmentPoint = Generators.perCommitPoint(c1.localParams.shaSeed, c1.localCommit.index + 1)
          me SEND RevokeAndAck(channelId = c1.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
        } else if (c1.localCommit.index != cr.nextRemoteRevocationNumber) throw new LightningException

        c1.remoteNextCommitInfo match {
          // We had sent a new sig and were waiting for their revocation
          // they didn't receive the new sig because disconnection happened
          // we resend the same updates and sig, also be careful about revocation
          case Left(wait) if wait.nextRemoteCommit.index == cr.nextLocalCommitmentNumber =>
            val revocationWasSentLast = c1.localCommit.index > wait.localCommitIndexSnapshot

            if (!revocationWasSentLast) maybeResendRevocation
            c1.localChanges.signed :+ wait.sent foreach SEND
            if (revocationWasSentLast) maybeResendRevocation

          // We had sent a new sig and were waiting for their revocation, they had received
          // the new sig but their revocation was lost during the disconnection, they'll resend us the revocation
          case Left(wait) if wait.nextRemoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendRevocation
          case Right(_) if c1.remoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendRevocation
          case _ => throw new LightningException("Sync error")
        }

        BECOME(norm.copy(commitments = c1), OPEN)
        norm.localShutdown foreach SEND
        // We may have HTLC to fulfill
        doProcess(CMDHTLCProcess)


      // We're exiting a sync state while funding tx is still not provided
      case (remote: WaitBroadcastRemoteData, _: ChannelReestablish, SLEEPING) =>
        // Need to check whether a funding is present in a listener
        BECOME(remote, WAIT_FUNDING_DONE)


      // We're exiting a sync state while waiting for their FundingLocked
      case (wait: WaitFundingDoneData, _: ChannelReestablish, SLEEPING) =>
        BECOME(wait, WAIT_FUNDING_DONE)
        wait.our foreach SEND


      // No in-flight HTLCs here, just proceed with negotiations
      case (neg: NegotiationsData, _: ChannelReestablish, SLEEPING) =>
        // Last closing signed may be empty if we are not a funder of this channel
        val lastClosingSignedOpt = neg.localProposals.headOption.map(_.localClosingSigned)
        neg.localShutdown +: lastClosingSignedOpt.toVector foreach SEND
        BECOME(neg, NEGOTIATIONS)


      // SYNC: ONLINE/SLEEPING


      case (some: HasNormalCommits, CMDSocketOnline, SLEEPING) =>
        // According to BOLD a first message on connection should be reestablish
        // will specifically NOT work in REFUNDING to not let them know beforehand
        me SEND makeReestablish(some, some.commitments.localCommit.index + 1)


      case (some: HasNormalCommits, newAnn: NodeAnnouncement, SLEEPING)
        if some.announce.nodeId == newAnn.nodeId && Announcements.checkSig(newAnn) =>
        // Node was SLEEPING for a long time, do not trigger listeners for this update
        data = me STORE some.modify(_.announce).setTo(newAnn)


      case (some: HasNormalCommits, newAnn: NodeAnnouncement, REFUNDING)
        if some.announce.nodeId == newAnn.nodeId && Announcements.checkSig(newAnn) =>
        // Remote peer's address may have changed since a channel backup has been made
        // we need to update data for next reconnect attempt to use it, but not save it
        data = some.modify(_.announce).setTo(newAnn)


      case (wait: WaitBroadcastRemoteData, CMDSocketOffline, WAIT_FUNDING_DONE) => BECOME(wait, SLEEPING)
      case (wait: WaitFundingDoneData, CMDSocketOffline, WAIT_FUNDING_DONE) => BECOME(wait, SLEEPING)
      case (negs: NegotiationsData, CMDSocketOffline, NEGOTIATIONS) => BECOME(negs, SLEEPING)
      case (norm: NormalData, CMDSocketOffline, OPEN) => BECOME(norm, SLEEPING)


      // NEGOTIATIONS MODE


      case (neg @ NegotiationsData(_, commitments, localShutdown, remoteShutdown, localProposals, _),
        ClosingSigned(_, remoteClosingFee, remoteClosingSig), NEGOTIATIONS) =>

        val lastLocalFee = localProposals.headOption.map(_.localClosingSigned.feeSatoshis) getOrElse {
          // If we are fundee and we were waiting for them to send their first proposal, we don't have a lastLocalClosingFee
          val closing = Closing.makeFirstClosing(commitments, localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)
          closing.localClosingSigned.feeSatoshis
        }

        val ClosingTxProposed(closing, closingSigned) =
          Closing.makeClosing(commitments, Satoshi(remoteClosingFee),
            localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)

        val signedClose =
          Scripts.addSigs(closing, commitments.localParams.fundingPrivKey.publicKey,
            commitments.remoteParams.fundingPubkey, closingSigned.signature, remoteClosingSig)

        Scripts.checkValid(txWithInputInfo = signedClose) match {
          case Success(okClose) if remoteClosingFee == lastLocalFee =>
            // Our current and their proposed fees are equal for this tx
            startMutualClose(neg, okClose.tx)

          case Success(okClose) =>
            val nextCloseFee = Satoshi(lastLocalFee + remoteClosingFee) / 4 * 2
            val nextProposed = Closing.makeClosing(commitments, nextCloseFee, localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)
            if (remoteClosingFee == nextCloseFee.amount) startMutualClose(neg, okClose.tx) SEND nextProposed.localClosingSigned else {
              val d1 = me STORE neg.copy(lastSignedTx = Some(okClose), localProposals = nextProposed +: neg.localProposals)
              me UPDATA d1 SEND nextProposed.localClosingSigned
            }

          case _ =>
            startLocalClose(neg)
            // Nothing left to do here so at least inform user
            throw new LightningException("Remote shutdown signature check failed")
        }


      case (negs: NegotiationsData, _: CMDShutdown, NEGOTIATIONS | SLEEPING) =>
        // Disregard custom scriptPubKey and always refund to local wallet
        startLocalClose(negs)


      // HANDLE FUNDING SPENT


      case (RefundingData(announce, Some(remoteLatestPoint), commitments), CMDSpent(spendTx), REFUNDING)
        // GUARD: we have got a remote commit which we asked them to spend and we have their point
        if spendTx.txIn.exists(input => commitments.commitInput.outPoint == input.outPoint) =>
        val rcp = Closing.claimRemoteMainOutput(commitments, remoteLatestPoint, spendTx)
        val d1 = ClosingData(announce, commitments, refundRemoteCommit = rcp :: Nil)
        BECOME(me STORE d1, CLOSING)


      case (cd: ClosingData, CMDSpent(htlcTx), CLOSING)
        // This may be one of our own 1st tier transactions
        // or they may broadcast their 1st tier, catch all of them
        if cd.revokedCommit.exists(_ spendsFromRevoked htlcTx) =>

        for {
          revCommitPublished <- cd.revokedCommit if revCommitPublished spendsFromRevoked htlcTx
          perCommitmentSecret <- Helpers.Closing.extractCommitmentSecret(cd.commitments, revCommitPublished.commitTx)
          punishTx <- Closing.claimRevokedHtlcTxOutputs(commitments = cd.commitments, htlcTx, perCommitmentSecret)
          punishTxj = com.lightning.walletapp.lnutils.ImplicitConversions.bitcoinLibTx2bitcoinjTx(punishTx)
        } com.lightning.walletapp.Utils.app.kit blockSend punishTxj


      case (some: HasNormalCommits, CMDSpent(tx), _)
        // GUARD: tx which spends our funding is broadcasted, must react
        if tx.txIn.exists(input => some.commitments.commitInput.outPoint == input.outPoint) =>
        val nextRemoteCommitLeft = some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit)

        Tuple3(GETREV(some.commitments, tx), nextRemoteCommitLeft, some) match {
          case (_, _, close: ClosingData) if close.refundRemoteCommit.nonEmpty => Tools log s"Existing refund $tx"
          case (_, _, close: ClosingData) if close.mutualClose.exists(_.txid == tx.txid) => Tools log s"Existing mutual $tx"
          case (_, _, close: ClosingData) if close.localCommit.exists(_.commitTx.txid == tx.txid) => Tools log s"Existing local $tx"
          case (_, _, close: ClosingData) if close.localProposals.exists(_.unsignedTx.tx.txid == tx.txid) => startMutualClose(close, tx)
          case (_, _, negs: NegotiationsData) if negs.localProposals.exists(_.unsignedTx.tx.txid == tx.txid) => startMutualClose(negs, tx)
          case (Some(claim), _, closingData: ClosingData) => me CLOSEANDWATCH closingData.modify(_.revokedCommit).using(claim +: _)
          case (Some(claim), _, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, revokedCommit = claim :: Nil)
          case (_, Left(nextRemote), _) if nextRemote.txOpt.exists(_.txid == tx.txid) => startRemoteNextClose(some, nextRemote)
          case _ if some.commitments.remoteCommit.txOpt.exists(_.txid == tx.txid) => startRemoteCurrentClose(some)
          case _ if some.commitments.localCommit.commitTx.tx.txid == tx.txid => startLocalClose(some)

          case (_, _, norm: NormalData) =>
            // Example: old snapshot is used two times in a row
            val d1 = norm.copy(unknownSpend = Some apply tx)
            me UPDATA STORE(d1)

          case _ =>
            // Nothing left to do here so at least inform user
            throw new LightningException("Unknown spend detected")
        }


      // HANDLE INITIALIZATION


      case (null, ref: RefundingData, null) => super.become(ref, REFUNDING)
      case (null, close: ClosingData, null) => super.become(close, CLOSING)
      case (null, init: InitData, null) => super.become(init, WAIT_FOR_INIT)
      case (null, wait: WaitFundingDoneData, null) => super.become(wait, SLEEPING)
      case (null, wait: WaitBroadcastRemoteData, null) => super.become(wait, SLEEPING)
      case (null, negs: NegotiationsData, null) => super.become(negs, SLEEPING)
      case (null, norm: NormalData, null) => super.become(norm, SLEEPING)
      case _ =>
    }

    // Change has been processed without failures
    events onProcessSuccess Tuple3(me, data, change)
  }

  def makeReestablish(some: HasNormalCommits, nextLocalCommitmentNumber: Long) = {
    val ShaHashesWithIndex(hashes, lastIndex) = some.commitments.remotePerCommitmentSecrets
    val yourLastPerCommitmentSecret = lastIndex.map(ShaChain.moves).flatMap(ShaChain getHash hashes).map(ByteVector.view)
    val myCurrentPerCommitmentPoint = Generators.perCommitPoint(some.commitments.localParams.shaSeed, some.commitments.localCommit.index)
    val myCurrentPerCommitmentPointOpt = Some(myCurrentPerCommitmentPoint)

    ChannelReestablish(some.commitments.channelId, nextLocalCommitmentNumber, some.commitments.remoteCommit.index,
      yourLastPerCommitmentSecret.map(Scalar.apply) orElse Some(Zeroes), myCurrentPerCommitmentPointOpt)
  }

  private def makeFirstFundingLocked(some: HasNormalCommits) = {
    val first = Generators.perCommitPoint(some.commitments.localParams.shaSeed, 1L)
    FundingLocked(some.commitments.channelId, nextPerCommitmentPoint = first)
  }

  private def becomeOpen(wait: WaitFundingDoneData, their: FundingLocked) = {
    val theirFirstPerCommitmentPoint = Right apply their.nextPerCommitmentPoint
    val c1 = wait.commitments.copy(remoteNextCommitInfo = theirFirstPerCommitmentPoint)
    BECOME(me STORE NormalData(wait.announce, c1), OPEN)
  }

  private def startMutualClose(some: HasNormalCommits, tx: Transaction) = some match {
    case closingData: ClosingData => BECOME(me STORE closingData.copy(mutualClose = tx +: closingData.mutualClose), CLOSING)
    case neg: NegotiationsData => BECOME(me STORE ClosingData(neg.announce, neg.commitments, neg.localProposals, tx :: Nil), CLOSING)
    case _ => BECOME(me STORE ClosingData(some.announce, some.commitments, Nil, tx :: Nil), CLOSING)
  }

  def startLocalClose(some: HasNormalCommits): Unit =
    // Something went wrong and we decided to spend our CURRENT commit transaction
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, LNParams.bag) -> some match {
      case (_, neg: NegotiationsData) if neg.lastSignedTx.isDefined => startMutualClose(neg, neg.lastSignedTx.get.tx)
      case (localClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(localCommit = localClaim :: Nil)
      case (localClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, localCommit = localClaim :: Nil)
    }

  private def startRemoteCurrentClose(some: HasNormalCommits) =
    // They've decided to spend their CURRENT commit tx, we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, some.commitments.remoteCommit, LNParams.bag) -> some match {
      case (remoteClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(remoteCommit = remoteClaim :: Nil)
      case (remoteClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, remoteCommit = remoteClaim :: Nil)
    }

  private def startRemoteNextClose(some: HasNormalCommits, nextRemoteCommit: RemoteCommit) =
    // They've decided to spend their NEXT commit tx, once again we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, nextRemoteCommit, LNParams.bag) -> some match {
      case (remoteClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(nextRemoteCommit = remoteClaim :: Nil)
      case (remoteClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, nextRemoteCommit = remoteClaim :: Nil)
    }
}

abstract class HostedChannelClient extends Channel(isHosted = true) { me =>
  def estimateCanSend = getCommits collect { case nc: HostedCommits => nc.localSpec.toLocalMsat } getOrElse 0L
  def estimateCanReceive = getCommits collect { case nc: HostedCommits => nc.localSpec.toRemoteMsat } getOrElse 0L
  def inFlightHtlcs = getCommits collect { case nc: HostedCommits => nc.localSpec.htlcs } getOrElse Set.empty[Htlc]

  private var isChainHeightKnown: Boolean = false
  private var isSocketConnected: Boolean = false

  def doProcess(change: Any) = {
    Tuple3(data, change, state) match {
      case (wait: WaitTheirHostedReply, CMDSocketOnline, WAIT_FOR_INIT) =>
        if (isChainHeightKnown) BECOME(wait, WAIT_FOR_ACCEPT) SEND wait.initMsg
        isSocketConnected = true


      case (wait: WaitTheirHostedReply, CMDChainTipKnown, WAIT_FOR_INIT) =>
        if (isSocketConnected) BECOME(wait, WAIT_FOR_ACCEPT) SEND wait.initMsg
        isChainHeightKnown = true


      case (WaitTheirHostedReply(announce, scriptPubKey), init: InitHostedChannel, WAIT_FOR_ACCEPT) =>
        if (init.liabilityDeadlineBlockdays < LNParams.minHostedLiabilityBlockdays) throw new LightningException("Their liability deadline is too low")
        if (init.channelCapacitySatoshis < LNParams.minCapacityMsat / 1000L) throw new LightningException("Their proposed channel capacity is too low")
        if (init.initialClientBalanceSatoshis > init.channelCapacitySatoshis) throw new LightningException("Client init balance is larger than capacity")
        if (init.minimalOnchainRefundAmountSatoshis > 100000L) throw new LightningException("Their minimal on-chain refund amount is too low")
        if (UInt64(100000000L) > init.maxHtlcValueInFlightMsat) throw new LightningException("Their maxHtlcValueInFlightMsat is too low")
        if (init.htlcMinimumMsat > 100000L) throw new LightningException("Their htlcMinimumMsat is too high")
        if (init.maxAcceptedHtlcs < 1) throw new LightningException("They can accept too few payments")

        val so = StateOverride(init.initialClientBalanceSatoshis, LNParams.broadcaster.currentBlockDay)
        val su = StateUpdate(so, Nil).signed(scriptPubKey, init, LNParams.nodePrivateKey)
        me UPDATA WaitTheirStateUpdate(announce, scriptPubKey, su, init) SEND su


      case (WaitTheirStateUpdate(announce, refundScriptPubKey, client, init), host: StateUpdate, WAIT_FOR_ACCEPT) =>
        val validationError = validate(client, host, refundScriptPubKey, init, announce).flatMap(ChanErrorCodes.hostedErrors.get)
        for (errorMessage <- validationError) throw new LightningException(com.lightning.walletapp.Utils.app getString errorMessage)

        val initHostBalance = init.channelCapacitySatoshis - init.initialClientBalanceSatoshis
        val localSpec = CommitmentSpec(feeratePerKw = 0L, init.initialClientBalanceSatoshis, initHostBalance)
        val lcss = LastCrossSignedState(refundScriptPubKey, init, client, host)

        BECOME(me STORE HostedCommits(announce, lcss, clientUpdatesSoFar = 0L, hostUpdatesSoFar = 0L,
          reSentUpdates = 0, Vector.empty, Vector.empty, localSpec, updateOpt = None, localError = None,
          remoteError = None, startedAt = System.currentTimeMillis), OPEN) SEND lcss


      case (wait: WaitTheirHostedReply, lcss @ LastCrossSignedState(refund, init, client, host), WAIT_FOR_ACCEPT) =>
        // We have expected StateUpdate but got LastCrossSignedState which means this channel exists already on host side
        // make sure our signature and other parameters match and if so then become OPEN using host supplied state data

        validate(client, host, refund, init, wait.announce) match {
          case None => BECOME(me STORE commitsFromCrossSigned(lcss, wait.announce), OPEN) SEND lcss
          case Some(errCode) => localSuspend(commitsFromCrossSigned(lcss, wait.announce), errCode)
        }

        // We may have HTLC to fulfill
        doProcess(CMDHTLCProcess)


      // CHANNEL IS ESTABLISHED


      case (hc: HostedCommits, addHtlc: UpdateAddHtlc, OPEN) =>
        // They have sent us an incoming payment, do not store yet
        me UPDATA hc.receiveAdd(addHtlc)


      case (hc: HostedCommits, fulfill: UpdateFulfillHtlc, OPEN) =>
        // Got a fulfill for an outgoing HTLC, do not store yet
        me UPDATA hc.receiveFulfill(fulfill)
        events fulfillReceived fulfill


      case (hc: HostedCommits, fail: UpdateFailHtlc, OPEN) =>
        // Got a failure for an outgoing HTLC, do not store yet
        me UPDATA hc.receiveFail(fail)


      case (hc: HostedCommits, fail: UpdateFailMalformedHtlc, OPEN) =>
        // Got 'malformed' failure for an outgoing HTLC, do not store yet
        me UPDATA hc.receiveFailMalformed(fail)


      case (hc: HostedCommits, rd: RoutingData, OPEN) =>
        val hc1 \ updateAddHtlc = hc.sendAdd(rd)
        me UPDATA hc1 SEND updateAddHtlc
        events outPaymentAccepted rd
        doProcess(CMDProceed)


      case (hc: HostedCommits, host1 @ StateUpdate(soHost, _), OPEN) =>
        val client1 @ StateUpdate(soClient, _) = hc.makeSignedStateUpdate
        val LastCrossSignedState(refund, init, _, _) = hc.lastCrossSignedState
        val mustResend = soHost.isBehind(soClient) || soClient.isBehind(soHost)

        if (mustResend) doProcess(CMDProceed) else {
          val validationResult = validate(client1, host1, refund, init, hc.announce)
          if (validationResult.isDefined) localSuspend(hc, validationResult.get) else {
            val localSpec1 = CommitmentSpec.reduce(hc.localSpec, hc.clientChanges, hc.hostChanges)
            val lcss1 = hc.lastCrossSignedState.copy(lastClientStateUpdate = client1, lastHostStateUpdate = host1)
            val hc1 = client1.stateOverride rewind hc.copy(lastCrossSignedState = lcss1, localSpec = localSpec1)
            // This means they have sent a signature first or we have sent/received Add/Fail/Fulfill since then
            if (hc.mustReply) me SEND client1
            BECOME(me STORE hc1, OPEN)
            events.onSettled(hc1)
          }
        }

        // We may have HTLC to fulfill
        doProcess(CMDHTLCProcess)


      case (hc: HostedCommits, CMDHTLCProcess, OPEN) =>
        // Fail or fulfill incoming HTLCs

        for {
          Htlc(true, add) <- hc.localSpec.htlcs
          // We don't want to receive a payment into a channel we have originally sent it from in an attempt to rebalance
          isLoop = hc.localSpec.htlcs.exists(htlc => !htlc.incoming && htlc.add.paymentHash == add.paymentHash)
        } me doProcess resolveHtlc(LNParams.nodePrivateKey, add, LNParams.bag, isLoop)
        // And sign changes once done because CMDFail/Fulfill above don't do that
        doProcess(CMDProceed)


      case (hc: HostedCommits, CMDProceed, OPEN) if hc.reSentUpdates > 20 =>
        // GUARD: prevent endless update exchange loop by suspending a channel
        localSuspend(hc, ERR_HOSTED_TOO_MANY_UPDATES)


      case (hc: HostedCommits, CMDProceed, OPEN)
        // GUARD: only send update if we have unsigned changes
        if hc.clientChanges.nonEmpty || hc.hostChanges.nonEmpty =>
        val hc1 = hc.copy(reSentUpdates = hc.reSentUpdates + 1)
        me UPDATA hc1 SEND hc.makeSignedStateUpdate


      case (hc: HostedCommits, cmd: CMDFulfillHtlc, OPEN) =>
        val fulfillHtlc = UpdateFulfillHtlc(hc.channelId, cmd.add.id, cmd.preimage)
        CommitmentSpec.findHtlcById(hc.localSpec, cmd.add.id, isIncoming = true) match {
          case Some(htlc) => me UPDATA hc.addClientProposal(fulfillHtlc) SEND fulfillHtlc
          case None => throw new LightningException
        }


      case (hc: HostedCommits, cmd: CMDFailHtlc, OPEN) =>
        val fail = UpdateFailHtlc(hc.channelId, cmd.id, cmd.reason)
        val notFound = CommitmentSpec.findHtlcById(hc.localSpec, cmd.id, isIncoming = true).isEmpty
        if (notFound) throw new LightningException else me UPDATA hc.addClientProposal(fail) SEND fail


      case (hc: HostedCommits, cmd: CMDFailMalformedHtlc, OPEN) =>
        val fail = UpdateFailMalformedHtlc(hc.channelId, cmd.id, cmd.onionHash, cmd.code)
        val notFound = CommitmentSpec.findHtlcById(hc.localSpec, cmd.id, isIncoming = true).isEmpty
        if (notFound) throw new LightningException else me UPDATA hc.addClientProposal(fail) SEND fail


      case (hc: HostedCommits, CMDSocketOffline, OPEN) =>
        isSocketConnected = false
        BECOME(hc, SLEEPING)


      case (hc: HostedCommits, CMDSocketOnline, SLEEPING) =>
        if (isChainHeightKnown) me SEND hc.initMsg
        isSocketConnected = true


      case (hc: HostedCommits, CMDChainTipKnown, SLEEPING) =>
        if (isSocketConnected) me SEND hc.initMsg
        isChainHeightKnown = true


      case (hc: HostedCommits, lcss @ LastCrossSignedState(refund, init, client, host), SLEEPING) =>
        val hostCrossSignedStateValidationResult = validate(client, host, refund, init, hc.announce)
        val clientSo = hc.lastCrossSignedState.lastClientStateUpdate.stateOverride

        if (hostCrossSignedStateValidationResult.isDefined) localSuspend(hc, hostCrossSignedStateValidationResult.get)
        else if (clientSo isBehind host.stateOverride) BECOME(me STORE commitsFromCrossSigned(lcss, hc.announce), OPEN) SEND lcss
        else BECOME(clientSo rewind hc, OPEN) SEND hc.lastCrossSignedState
        // We may have HTLC to fulfill
        doProcess(CMDHTLCProcess)


      case (hc: HostedCommits, newAnn: NodeAnnouncement, SLEEPING)
        if hc.announce.nodeId == newAnn.nodeId && Announcements.checkSig(newAnn) =>
        // Node was SLEEPING for a long time, do not trigger listeners for this update
        data = me STORE hc.modify(_.announce).setTo(newAnn)


      case (hc: HostedCommits, CMDChannelUpdate(upd), OPEN | SLEEPING) =>
        val Tuple3(blockHeight, _, _) = Tools.fromShortId(upd.shortChannelId)
        if (blockHeight > LNParams.maxHostedBlockHeight) localSuspend(hc, ERR_HOSTED_UPDATE_BLOCK_TOO_HIGH)
        if (upd.cltvExpiryDelta < LNParams.minHostedCltvDelta) localSuspend(hc, ERR_HOSTED_UPDATE_CLTV_TOO_LOW)
        val d1 = hc.modify(_.updateOpt) setTo Some(upd)
        // Do not trigger listeners for this update
        data = me STORE d1


      case (hc: HostedCommits, remoteError: Error, OPEN | SLEEPING) =>
        val cs1 = hc.modify(_.remoteError) setTo Some(remoteError)
        BECOME(me STORE cs1, SUSPENDED)


      case (hc: HostedCommits, CMDStateOverride(so), OPEN | SLEEPING | SUSPENDED) =>
        val LastCrossSignedState(scriptPubKey, initHostedChannel, lastClientStateUpdate, _) = hc.lastCrossSignedState
        val isRightClientUpdateNumber = so.clientUpdatesSoFar == lastClientStateUpdate.stateOverride.clientUpdatesSoFar
        val isRightHostUpdateNumber = so.hostUpdatesSoFar == lastClientStateUpdate.stateOverride.hostUpdatesSoFar
        val overrideSigOk = StateUpdate(so, Nil).verify(scriptPubKey, initHostedChannel, hc.announce.nodeId)
        val isBalanceOk = so.updatedClientBalanceSatoshis <= initHostedChannel.channelCapacitySatoshis
        val isBlockdayAcceptable = math.abs(so.blockDay - LNParams.broadcaster.currentBlockDay) <= 1

        if (!isBlockdayAcceptable) throw new LightningException("Provided blockday from StateOverride is not acceptable")
        if (!isRightClientUpdateNumber) throw new LightningException("Provided Client update number from StateOverride is wrong")
        if (!isRightHostUpdateNumber) throw new LightningException("Provided Host update number from StateOverride is wrong")
        if (!isBalanceOk) throw new LightningException("Provided Client updated balance is larger than capacity")
        if (!overrideSigOk) throw new LightningException("Provided StateOverride signature is wrong")

        val hostStateUpdate = StateUpdate(so, inFlightHtlcs = Nil)
        val clientStateUpdate = StateUpdate(so, Nil).signed(scriptPubKey, initHostedChannel, LNParams.nodePrivateKey)
        val lcss = LastCrossSignedState(scriptPubKey, initHostedChannel, clientStateUpdate, hostStateUpdate)
        BECOME(me STORE commitsFromCrossSigned(lcss, hc.announce), OPEN) SEND lcss


      case (null, wait: WaitTheirHostedReply, null) => super.become(wait, WAIT_FOR_INIT)
      case (null, hc: HostedCommits, null) if hc.isInErrorState => super.become(hc, SUSPENDED)
      case (null, hc: HostedCommits, null) => super.become(hc, SLEEPING)
      case _ =>
    }

    // Change has been processed without failures
    events onProcessSuccess Tuple3(me, data, change)
  }

  def validate(client: StateUpdate, host: StateUpdate, scriptPubKey: ByteVector,
               init: InitHostedChannel, ann: NodeAnnouncement): Option[ByteVector] = {

    val isSameHtlcsInFlight = client.inFlightHtlcs.toSet == host.inFlightHtlcs.toSet
    val isBlockdayAcceptable = math.abs(client.stateOverride.blockDay - host.stateOverride.blockDay) <= 1
    val isSameBalance = client.stateOverride.updatedClientBalanceSatoshis == host.stateOverride.updatedClientBalanceSatoshis
    val isSameClientUpdatesSoFar = client.stateOverride.clientUpdatesSoFar == host.stateOverride.clientUpdatesSoFar
    val isSameHostUpdatesSoFar = client.stateOverride.hostUpdatesSoFar == host.stateOverride.hostUpdatesSoFar
    val clientSigOk = client.verify(scriptPubKey, init, LNParams.nodePublicKey)
    val hostSigOk = host.verify(scriptPubKey, init, ann.nodeId)

    if (!isSameBalance) Some(ERR_HOSTED_WRONG_BALANCE)
    else if (!isSameHtlcsInFlight) Some(ERR_HOSTED_WRONG_IN_FLIGHT)
    else if (!isBlockdayAcceptable) Some(ERR_HOSTED_WRONG_BLOCKDAY)
    else if (!isSameClientUpdatesSoFar) Some(ERR_HOSTED_WRONG_CLIENT_NUMBER)
    else if (!isSameHostUpdatesSoFar) Some(ERR_HOSTED_WRONG_HOST_NUMBER)
    else if (!clientSigOk) Some(ERR_HOSTED_WRONG_CLIENT_SIG)
    else if (!hostSigOk) Some(ERR_HOSTED_WRONG_HOST_SIG)
    else None
  }

  def commitsFromCrossSigned(lcss: LastCrossSignedState, announce: NodeAnnouncement): HostedCommits = {
    val inFlightHtlcs = for (Tuple5(incoming, id, amountMsat, paymentHash, expiry) <- lcss.lastClientStateUpdate.inFlightHtlcs) yield {
      val addHtlc = UpdateAddHtlc(announce.hostedChanId, id, amountMsat, paymentHash, expiry, onionRoutingPacket = Sphinx.emptyOnionPacket)
      Htlc(incoming, addHtlc)
    }

    val so = lcss.lastClientStateUpdate.stateOverride
    val hostBalance = lcss.initHostedChannel.channelCapacitySatoshis - so.updatedClientBalanceSatoshis
    HostedCommits(announce, lcss, so.clientUpdatesSoFar, so.hostUpdatesSoFar, 0, Vector.empty, Vector.empty,
      CommitmentSpec(feeratePerKw = 0L, so.updatedClientBalanceSatoshis, hostBalance, inFlightHtlcs.toSet),
      updateOpt = None, localError = None, remoteError = None, startedAt = System.currentTimeMillis)
  }

  def localSuspend(hc: HostedCommits, errCode: ByteVector) = {
    val localError = Error(channelId = hc.channelId, data = errCode)
    val hc1 = hc.modify(_.localError) setTo Some(localError)
    BECOME(me STORE hc1, SUSPENDED) SEND localError
  }
}

object NormalChannel {
  val WAIT_FOR_INIT = "WAIT-FOR-INIT"
  val WAIT_FOR_ACCEPT = "WAIT-FOR-ACCEPT"
  val WAIT_FOR_FUNDING = "WAIT-FOR-FUNDING"
  val WAIT_FUNDING_SIGNED = "WAIT-FUNDING-SIGNED"

  val WAIT_FUNDING_DONE = "OPENING"
  val NEGOTIATIONS = "NEGOTIATIONS"
  val SLEEPING = "SLEEPING"
  val OPEN = "OPEN"

  // No tears, only dreams now
  val SUSPENDED = "SUSPENDED"
  val REFUNDING = "REFUNDING"
  val CLOSING = "CLOSING"

  def isOpeningOrOperational(chan: Channel) = isOperational(chan) || isOpening(chan)
  def isOpening(chan: Channel) = chan.data.isInstanceOf[WaitFundingDoneData]

  def isOperational(chan: Channel) = chan.data match {
    case _: HostedCommits => chan.state != SUSPENDED
    case NormalData(_, _, None, None, _) => true
    case _ => false
  }

  def channelAndHop(chan: Channel) = for {
    update <- chan.getCommits.flatMap(_.updateOpt)
    hop = update.toHop(chan.data.announce.nodeId)
  } yield chan -> Vector(hop)
}

trait ChannelListener {
  def nullOnBecome(chan: Channel) = {
    val nullTransition = Tuple4(chan, chan.data, null, chan.state)
    if (onBecome isDefinedAt nullTransition) onBecome(nullTransition)
  }

  type Malfunction = (Channel, Throwable)
  type Incoming = (Channel, ChannelData, Any)
  type Transition = (Channel, ChannelData, String, String)
  def onProcessSuccess: PartialFunction[Incoming, Unit] = none
  def onException: PartialFunction[Malfunction, Unit] = none
  def onBecome: PartialFunction[Transition, Unit] = none

  def fulfillReceived(upd: UpdateFulfillHtlc): Unit = none
  def outPaymentAccepted(rd: RoutingData): Unit = none
  def onSettled(cs: Commitments): Unit = none
}