package com.lightning.wallet.ln

import octopus.dsl._
import octopus.syntax._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.AddErrorCodes._
import java.util.concurrent.Executors
import scala.collection.mutable
import fr.acinq.eclair.UInt64
import scala.util.Success

import com.lightning.wallet.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}
import com.lightning.wallet.ln.Helpers.{Closing, Funding}
import fr.acinq.bitcoin.Crypto.{PrivateKey, Scalar}
import com.lightning.wallet.ln.Tools.{none, runAnd}


abstract class Channel extends StateMachine[ChannelData] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def apply[T](ex: Commitments => T) = Some(data) collect { case some: HasCommitments => ex apply some.commitments }
  def process(change: Any) = Future(me doProcess change) onFailure { case err => events onError me -> err }
  def isOperational = data match { case NormalData(_, _, None, None) => true case _ => false }
  val listeners: mutable.Set[ChannelListener]

  private[this] val events = new ChannelListener {
    override def onError = { case malfunction => for (lst <- listeners if lst.onError isDefinedAt malfunction) lst onError malfunction }
    override def onBecome = { case transition => for (lst <- listeners if lst.onBecome isDefinedAt transition) lst onBecome transition }
    override def onProcess = { case incoming => for (lst <- listeners if lst.onProcess isDefinedAt incoming) lst onProcess incoming }
  }

  def SEND(msg: LightningMessage): Unit
  def CLOSEANDWATCH(close: ClosingData): Unit
  def STORE(content: HasCommitments): HasCommitments
  def UPDATE(d1: ChannelData): Channel = BECOME(d1, state)
  def BECOME(data1: ChannelData, state1: String) = runAnd(me) {
    // Transition should always be defined before vars are updated
    val trans = Tuple4(me, data1, state, state1)
    super.become(data1, state1)
    events onBecome trans
  }

  def doProcess(change: Any) = {
    Tuple3(data, change, state) match {
      case (InitData(announce), cmd @ CMDOpenChannel(localParams, tempId,
        initialFeeratePerKw, pushMsat, _, fundingSat), WAIT_FOR_INIT) =>

        BECOME(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT) SEND OpenChannel(LNParams.chainHash,
          tempId, fundingSat, pushMsat, LNParams.dustLimit.amount, localParams.maxHtlcValueInFlightMsat,
          localParams.channelReserveSat, LNParams.minHtlcValue.amount, initialFeeratePerKw, localParams.toSelfDelay,
          localParams.maxAcceptedHtlcs, localParams.fundingPrivKey.publicKey, localParams.revocationBasepoint,
          localParams.paymentBasepoint, localParams.delayedPaymentBasepoint, localParams.htlcBasepoint,
          Generators.perCommitPoint(localParams.shaSeed, index = 0L), channelFlags = 0.toByte)


      case (wait @ WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT)
        if accept.temporaryChannelId == cmd.temporaryChannelId =>

        val acceptChannelValidator = Validator[AcceptChannel]
          .rule(_.minimumDepth <= 6L, "Their minimumDepth is too high")
          .rule(_.htlcMinimumMsat <= 200000L, "Their htlcMinimumMsat too high")
          .rule(_.toSelfDelay <= cmd.localParams.toSelfDelay * 2, "Their toSelfDelay is too high")
          .rule(_.maxHtlcValueInFlightMsat > UInt64(LNParams.maxHtlcValue.amount / 5), "Their maxHtlcValueInFlightMsat is too low")
          .rule(_.channelReserveSatoshis.toDouble / cmd.fundingAmountSat < LNParams.maxReserveToFundingRatio, "Their reserve is too high")
          .rule(_.maxAcceptedHtlcs > 0, "They can accept too few incoming HTLCs")
          .rule(_.dustLimitSatoshis >= 546L, "Their dust limit is too low")

        val result = accept.validate(acceptChannelValidator)
        if (result.isValid) BECOME(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING) else BECOME(wait, CLOSING)
        if (!result.isValid) Tools.log(result.toFieldErrMapping map { case _ \ error => s"- $error" } mkString "\n")


      // They have accepted our proposal, now let them sign a first commit tx
      case (WaitFundingData(announce, cmd, accept), (fundTx: Transaction, outIndex: Int), WAIT_FOR_FUNDING) =>
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Funding.makeFirstFunderCommitTxs(cmd, accept,
          fundTx.hash, outIndex, accept.firstPerCommitmentPoint)

        val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, cmd.localParams.fundingPrivKey)
        val fundingCreated = FundingCreated(cmd.temporaryChannelId, fundTx.hash, outIndex, localSigOfRemoteTx)
        val firstRemoteCommit = RemoteCommit(0L, remoteSpec, remoteCommitTx.tx.txid, accept.firstPerCommitmentPoint)
        BECOME(WaitFundingSignedData(announce, cmd.localParams, Tools.toLongId(fundTx.hash, outIndex), accept, fundTx,
          localSpec, localCommitTx, firstRemoteCommit), WAIT_FUNDING_SIGNED) SEND fundingCreated


      // They have signed our first commit tx, we can broadcast a funding tx
      case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED)
        if remote.channelId == wait.channelId =>

        val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, wait.localParams.fundingPrivKey.publicKey,
          wait.remoteParams.fundingPubkey, Scripts.sign(wait.localCommitTx, wait.localParams.fundingPrivKey), remote.signature)

        if (Scripts.checkSpendable(signedLocalCommitTx).isEmpty) BECOME(wait, CLOSING) else {
          val localCommit = LocalCommit(0L, wait.localSpec, htlcTxsAndSigs = Nil, signedLocalCommitTx)
          val localChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty)
          val remoteChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty)
          val dummy = PrivateKey(data = Tools.random getBytes 32, compressed = true).toPoint

          val commitments = Commitments(wait.localParams, wait.remoteParams, localCommit,
            remoteCommit = wait.remoteCommit, localChanges, remoteChanges, localNextHtlcId = 0L,
            remoteNextHtlcId = 0L, remoteNextCommitInfo = Right(dummy), wait.localCommitTx.input,
            remotePerCommitmentSecrets = ShaHashesWithIndex(Map.empty, None), wait.channelId)

          BECOME(WaitFundingDoneData(wait.announce, None, None,
            wait.fundingTx, commitments), WAIT_FUNDING_DONE)
        }


      // FUNDING TX IS BROADCASTED AT THIS POINT


      // We have not yet sent a FundingLocked but just got one from them so we save it and keep waiting
      case (wait @ WaitFundingDoneData(_, None, _, _, _), their: FundingLocked, WAIT_FUNDING_DONE)
        if their.channelId == wait.commitments.channelId =>

        // Not storing since they will re-send on restart
        val d1 = wait.modify(_.their) setTo Some(their)
        me UPDATE d1


      // We have already sent them a FundingLocked and now we got one from them so we can enter normal state now
      case (wait @ WaitFundingDoneData(_, Some(our), _, _, _), their: FundingLocked, WAIT_FUNDING_DONE)
        if their.channelId == wait.commitments.channelId =>

        val point = Right(their.nextPerCommitmentPoint)
        val c1 = wait.commitments.copy(remoteNextCommitInfo = point)
        BECOME(me STORE NormalData(wait.announce, c1), NORMAL)


      // We got our lock but their is not yet present so we save ours and just keep waiting for their
      case (wait @ WaitFundingDoneData(_, _, None, _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        val d1 = wait.modify(_.our) setTo Some(our)
        me UPDATE STORE(d1) SEND our


      // We got our lock when their is already present so we can safely enter normal state now
      case (wait @ WaitFundingDoneData(_, _, Some(their), _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        val point = Right(their.nextPerCommitmentPoint)
        val c1 = wait.commitments.copy(remoteNextCommitInfo = point)
        BECOME(me STORE NormalData(wait.announce, c1), NORMAL) SEND our


      // NORMAL MODE


      case (norm: NormalData, add: UpdateAddHtlc, NORMAL)
        if add.channelId == norm.commitments.channelId =>

        // Got new incoming HTLC so put it to changes for now
        val c1 = Commitments.receiveAdd(norm.commitments, add)
        me UPDATE norm.copy(commitments = c1)


      case (norm: NormalData, fulfill: UpdateFulfillHtlc, NORMAL)
        if fulfill.channelId == norm.commitments.channelId =>

        // Got a fulfill for an outgoing HTLC we sent them earlier
        val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
        me UPDATE norm.copy(commitments = c1)


      case (norm: NormalData, fail: UpdateFailHtlc, NORMAL)
        if fail.channelId == norm.commitments.channelId =>

        // Got a failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFail(norm.commitments, fail)
        me UPDATE norm.copy(commitments = c1)


      case (norm: NormalData, fail: UpdateFailMalformedHtlc, NORMAL)
        if fail.channelId == norm.commitments.channelId =>

        // Got 'malformed' failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFailMalformed(norm.commitments, fail)
        me UPDATE norm.copy(commitments = c1)


      // We can only accept a new HTLC when shutdown is not active
      case (norm: NormalData, cmd: CMDAddHtlc, NORMAL) if isOperational =>
        // AND this HTLC is not already in-flight AND it has not been fulfilled already
        val activeHtlcs = Commitments.actualRemoteCommit(norm.commitments).spec.htlcs
        val active = activeHtlcs.exists(_.add.paymentHash == cmd.rpi.pr.paymentHash)

        if (active) throw CMDAddExcept(cmd, ERR_IN_FLIGHT)
        LNParams.bag getPaymentInfo cmd.rpi.pr.paymentHash match {
          // When re-sending an already fulfilled HTLC a peer may provide us with a preimage without routing a payment
          case Success(info: PaymentInfo) if info.actualStatus == SUCCESS => throw CMDAddExcept(cmd, ERR_FULFILLED)
          case Success(info: PaymentInfo) if info.incoming == 1 => throw CMDAddExcept(cmd, ERR_FAILED)

          case _ =>
            // This may be a FAILURE payment which is fine
            val c1 \ updateAddHtlc = Commitments.sendAdd(norm.commitments, cmd)
            me UPDATE norm.copy(commitments = c1) SEND updateAddHtlc
            doProcess(CMDProceed)
        }


      // We're fulfilling an HTLC we got earlier
      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFulfillHtlc, NORMAL) =>
        val c1 \ updateFulfillHtlc = Commitments.sendFulfill(commitments, cmd)
        me UPDATE norm.copy(commitments = c1) SEND updateFulfillHtlc


      // Failing an HTLC we got earlier
      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFailHtlc, NORMAL) =>
        val c1 \ updateFailHtlc = Commitments.sendFail(commitments, cmd)
        me UPDATE norm.copy(commitments = c1) SEND updateFailHtlc


      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFailMalformedHtlc, NORMAL) =>
        val c1 \ updateFailMalformedHtlс = Commitments.sendFailMalformed(commitments, cmd)
        me UPDATE norm.copy(commitments = c1) SEND updateFailMalformedHtlс


      // Fail or fulfill incoming HTLCs
      case (norm: NormalData, CMDHTLCProcess, NORMAL) =>
        val minExpiry = LNParams.broadcaster.currentHeight + 3L
        for (Htlc(false, add) <- norm.commitments.remoteCommit.spec.htlcs)
          me doProcess resolveHtlc(LNParams.nodePrivateKey, add, LNParams.bag, minExpiry)

        // And sign once done
        doProcess(CMDProceed)


      case (norm: NormalData, CMDProceed, NORMAL)
        // Only if we have a point and something to sign
        if norm.commitments.remoteNextCommitInfo.isRight &&
          (norm.commitments.localChanges.proposed.nonEmpty ||
          norm.commitments.remoteChanges.acked.nonEmpty) =>

        // Propose new remote commit via commit tx sig
        val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
        val c1 \ commitSig = Commitments.sendCommit(norm.commitments, nextRemotePoint)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATE d1 SEND commitSig


      case (norm: NormalData, sig: CommitSig, NORMAL)
        if sig.channelId == norm.commitments.channelId =>

        // We received a commit sig from them, now we can update our local commit
        val c1 \ revokeAndAck = Commitments.receiveCommit(norm.commitments, sig)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATE d1 SEND revokeAndAck
        doProcess(CMDProceed)


      case (norm: NormalData, rev: RevokeAndAck, NORMAL)
        if rev.channelId == norm.commitments.channelId =>

        // We received a revocation because we sent a commit sig
        val c1 = Commitments.receiveRevocation(norm.commitments, rev)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATE d1 doProcess CMDHTLCProcess


      case (norm: NormalData, CMDBestHeight(height), NORMAL | SYNC)
        // GUARD: close channel uncooperatively if outdated HTLC is large enough to be included in a commit tx
        // or if outdated HTLC is small and thus filtered out of commit tx but an additional period of 288 blocks has passed
        if norm.commitments.localCommit.htlcTxsAndSigs.isEmpty && Commitments.hasOutdatedOutgoing(norm.commitments, height - 7 * 144)
          || norm.commitments.localCommit.htlcTxsAndSigs.nonEmpty && Commitments.hasOutdatedOutgoing(norm.commitments, height) =>

        startLocalCurrentClose(norm)


      // SHUTDOWN in WAIT_FUNDING_DONE and NORMAL


      case (wait: WaitFundingDoneData, CMDShutdown, WAIT_FUNDING_DONE) =>
        // We have decided to close our channel before it reached a min depth
        me startShutdown NormalData(wait.announce, wait.commitments)


      case (wait: WaitFundingDoneData, remote: Shutdown, WAIT_FUNDING_DONE)
        // They have decided to close our channel before it reached a min depth
        if remote.channelId == wait.commitments.channelId =>

        val norm = NormalData(wait.announce, wait.commitments)
        val norm1 = norm.modify(_.remoteShutdown) setTo Some(remote)
        // We have their Shutdown so we add ours and start negotiations
        me startShutdown norm1
        doProcess(CMDProceed)


      case (norm @ NormalData(_, commitments, our, their), CMDShutdown, NORMAL) =>
        // We have unsigned outgoing HTLCs or already have tried to close this channel cooperatively
        val nope = our.isDefined | their.isDefined | Commitments.localHasUnsignedOutgoing(commitments)
        if (nope) startLocalCurrentClose(norm) else me startShutdown norm


      case (norm @ NormalData(_, commitments, _, None), remote: Shutdown, NORMAL)
        // GUARD: Either they initiate a shutdown or respond to the one we have sent
        if remote.channelId == norm.commitments.channelId =>

        val d1 = norm.modify(_.remoteShutdown) setTo Some(remote)
        val nope = Commitments.remoteHasUnsignedOutgoing(commitments)
        // Can't close cooperatively if they have unsigned outgoing HTLCs
        // we should clear our unsigned outgoing HTLCs and then start a shutdown
        if (nope) startLocalCurrentClose(norm) else me UPDATE d1 doProcess CMDProceed


      case (norm @ NormalData(_, commitments, None, their), CMDProceed, NORMAL)
        // GUARD: got their shutdown, have no unsigned HTLCs, send ours and proceed
        if Commitments.hasNoPendingHtlc(commitments) && their.isDefined =>

        me startShutdown norm
        doProcess(CMDProceed)


      case (NormalData(announce, commitments, our, their), CMDProceed, NORMAL)
        // GUARD: got both shutdowns, have no unsigned HTLCs so we can start negotiations
        if Commitments.hasNoPendingHtlc(commitments) && our.isDefined && their.isDefined =>

        val _ \ sig = Closing.makeFirstClosing(commitments, our.get.scriptPubKey, their.get.scriptPubKey)
        val neg = NegotiationsData(announce, commitments, localClosingSigned = sig, our.get, their.get)
        BECOME(me STORE neg, NEGOTIATIONS) SEND sig


      // SYNC and REFUNDING MODE


      // We may get this message any time so just save it here
      case (wait: WaitFundingDoneData, CMDConfirmed(tx), SYNC)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        val wait1 = wait.modify(_.our) setTo Some(our)
        me UPDATE STORE(wait1)


      // We're exiting a sync state while waiting for their FundingLocked
      case (wait: WaitFundingDoneData, cr: ChannelReestablish, SYNC)
        if cr.channelId == wait.commitments.channelId =>

        BECOME(wait, WAIT_FUNDING_DONE)
        wait.our foreach SEND


      // No in-flight HTLCs here, just proceed with negotiations
      case (neg: NegotiationsData, cr: ChannelReestablish, SYNC)
        if cr.channelId == neg.commitments.channelId =>

        BECOME(neg, NEGOTIATIONS)
        me SEND neg.localClosingSigned


      case (norm: NormalData, cr: ChannelReestablish, SYNC)
        if cr.channelId == norm.commitments.channelId =>

        // If next_local_commitment_number is 1 in both the channel_reestablish it sent
        // and received, then the node MUST retransmit funding_locked, otherwise it MUST NOT
        if (cr.nextLocalCommitmentNumber == 1 && norm.commitments.localCommit.index == 0)
          me SEND makeFundingLocked(norm.commitments)

        // First we clean up unacknowledged updates
        val localDelta = norm.commitments.localChanges.proposed collect { case u: UpdateAddHtlc => true }
        val remoteDelta = norm.commitments.remoteChanges.proposed collect { case u: UpdateAddHtlc => true }
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
          case _ => throw new LightningException
        }

        BECOME(norm.copy(commitments = c1), NORMAL)
        norm.localShutdown foreach SEND
        doProcess(CMDHTLCProcess)


      // We just close a channel in any kind of irregular state
      case (some: HasCommitments, cr: ChannelReestablish, SYNC)
        if cr.channelId == some.commitments.channelId =>
        startLocalCurrentClose(some)


      // SYNC: ONLINE/OFFLINE


      case (some: HasCommitments, CMDOnline, SYNC) =>
        me SEND ChannelReestablish(some.commitments.channelId,
          some.commitments.localCommit.index + 1, some.commitments.remoteCommit.index)


      case (wait: WaitFundingDoneData, CMDOffline, WAIT_FUNDING_DONE) => BECOME(wait, SYNC)
      case (negs: NegotiationsData, CMDOffline, NEGOTIATIONS) => BECOME(negs, SYNC)
      case (norm: NormalData, CMDOffline, NORMAL) => BECOME(norm, SYNC)


      // NEGOTIATIONS MODE


      case (neg: NegotiationsData, ClosingSigned(channelId, feeSatoshis, remoteSig), NEGOTIATIONS)
        if channelId == neg.commitments.channelId =>

        val Seq(closeFeeSat, remoteFeeSat) = Seq(neg.localClosingSigned.feeSatoshis, feeSatoshis) map Satoshi
        val Seq(localScript, remoteScript) = Seq(neg.localShutdown.scriptPubKey, neg.remoteShutdown.scriptPubKey)
        val lastCommitAmountSat = neg.commitments.localCommit.commitTx.tx.txOut.map(_.amount.amount).sum
        val lastCommitFeeSat = neg.commitments.commitInput.txOut.amount.amount - lastCommitAmountSat
        if (remoteFeeSat.amount > lastCommitFeeSat) throw new LightningException

        val nextCloseFee = (closeFeeSat + remoteFeeSat) / 4 * 2
        val (closingTx, signed) = Closing.makeClosing(neg.commitments, localScript, remoteScript, remoteFeeSat)
        val closing = Scripts.addSigs(closingTx, neg.commitments.localParams.fundingPrivKey.publicKey,
          neg.commitments.remoteParams.fundingPubkey, signed.signature, remoteSig)

        Scripts checkSpendable closing match {
          case None => throw new LightningException
          case Some(closingInfo) if closeFeeSat == remoteFeeSat => startMutualClose(neg, closingInfo.tx)
          case Some(closingInfo) if nextCloseFee == remoteFeeSat => startMutualClose(neg, closingInfo.tx)

          case _ =>
            // Fee has been further corrected and we can offer a next version of mutual closing transaction
            val _ \ nextClosingSigned = Closing.makeClosing(neg.commitments, localScript, remoteScript, nextCloseFee)
            me UPDATE neg.copy(localClosingSigned = nextClosingSigned) SEND nextClosingSigned
        }


      // HANDLE FUNDING SPENT


      case (ref: RefundingData, CMDSpent(spendTx), REFUNDING)
        // GUARD: we have lost our state and asked them to spend their local commit
        if spendTx.txIn.exists(_.outPoint == ref.commitments.commitInput.outPoint) =>
        // `commitments.remoteCommit` has to be updated to their `myCurrentPerCommitmentPoint` at this point
        val rcp = Closing.claimRemoteMainOutput(ref.commitments, ref.commitments.remoteCommit, spendTx)
        val d1 = me STORE ClosingData(ref.announce, ref.commitments, remoteCommit = rcp :: Nil)
        me UPDATE d1


      case (close: ClosingData, CMDSpent(spendTx), _)
        if close.mutualClose.exists(_.txid == spendTx.txid) =>
        Tools log "Disregarding locally broadcasted mutual closing tx"


      case (close: ClosingData, CMDSpent(spendTx), _)
        if close.localCommit.exists(_.commitTx.txid == spendTx.txid) =>
        Tools log "Disregarding locally broadcasted uncooperative commit tx"


      case (some: HasCommitments, CMDSpent(spendTx), _)
        // GUARD: something which spends our funding is broadcasted, must react
        if spendTx.txIn.exists(_.outPoint == some.commitments.commitInput.outPoint) =>

        // First, we have to check if it's remote or next remote closing
        some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit) match {
          case Left(nextRemote) if nextRemote.txid == spendTx.txid => startRemoteNextClose(some, nextRemote)
          case _ if some.commitments.remoteCommit.txid == spendTx.txid => startRemoteCurrentClose(some)
          case _ => startOtherClose(some, spendTx)
        }


      // HANDLE INITIALIZATION


      case (null, ref: RefundingData, null) => BECOME(ref, REFUNDING)
      case (null, close: ClosingData, null) => BECOME(close, CLOSING)
      case (null, init: InitData, null) => BECOME(init, WAIT_FOR_INIT)
      case (null, wait: WaitFundingDoneData, null) => BECOME(wait, SYNC)
      case (null, negs: NegotiationsData, null) => BECOME(negs, SYNC)
      case (null, norm: NormalData, null) => BECOME(norm, SYNC)


      // MISC


      case (some, CMDShutdown | _: Error, WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FOR_FUNDING | WAIT_FUNDING_SIGNED) =>
        // This may only happen when we cancel opening of new channel or get their remote error, we lose nothing here
        BECOME(some, CLOSING)


      case (some: HasCommitments, err: Error, WAIT_FUNDING_DONE | NEGOTIATIONS | NORMAL | SYNC)
        // GUARD: we only react on connection level remote errors or those related to our channel
        if err.channelId == some.commitments.channelId || err.channelId == BinaryData("00" * 32) =>
        startLocalCurrentClose(some)


      // CMDShutdown in WAIT_FUNDING_DONE and NORMAL is handled differently above
      case (some: HasCommitments, CMDShutdown, NEGOTIATIONS | SYNC) => startLocalCurrentClose(some)
      case (_: NormalData, add: CMDAddHtlc, SYNC) => throw CMDAddExcept(add, ERR_OFFLINE)
      case _ =>
    }

    // Change has been successfully processed
    events onProcess Tuple3(me, data, change)
  }

  def sendFeeUpdate(norm: NormalData, updatedFeeRate: Long) =
    Commitments.sendFee(norm.commitments, updatedFeeRate) foreach { case c1 \ msg =>
      // We only send a fee update if our current chan reserve + commit tx fee can afford it
      // otherwise we fail silently in hope that fee will drop or we will receive a payment
      me UPDATE norm.copy(commitments = c1) SEND msg
      doProcess(CMDProceed)
    }

  private def makeFundingLocked(cs: Commitments) = {
    val point = Generators.perCommitPoint(cs.localParams.shaSeed, 1L)
    FundingLocked(cs.channelId, nextPerCommitmentPoint = point)
  }

  private def startShutdown(norm: NormalData) = {
    val finalScriptPubKey = norm.commitments.localParams.defaultFinalScriptPubKey
    val localShutdown = Shutdown(norm.commitments.channelId, finalScriptPubKey)
    val norm1 = norm.modify(_.localShutdown) setTo Some(localShutdown)
    BECOME(norm1, NORMAL) SEND localShutdown
  }

  private def startMutualClose(neg: NegotiationsData, closeTx: Transaction) =
    BECOME(me STORE ClosingData(neg.announce, neg.commitments, closeTx :: Nil), CLOSING)

  private def startLocalCurrentClose(some: HasCommitments) =
    // Something went wrong and we decided to spend our CURRENT commit transaction
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, LNParams.bag) -> some match {
      case (claim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(localCommit = claim :: Nil)
      case (claim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, localCommit = claim :: Nil)
    }

  private def startRemoteCurrentClose(some: HasCommitments) =
    // They've decided to spend their CURRENT commit tx, we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, some.commitments.remoteCommit, LNParams.bag) -> some match {
      case (remoteClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(remoteCommit = remoteClaim :: Nil)
      case (remoteClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, remoteCommit = remoteClaim :: Nil)
    }

  private def startRemoteNextClose(some: HasCommitments, nextRemoteCommit: RemoteCommit) =
    // They've decided to spend their NEXT commit tx, once again we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, nextRemoteCommit, LNParams.bag) -> some match {
      case (remoteClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(nextRemoteCommit = remoteClaim :: Nil)
      case (remoteClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, nextRemoteCommit = remoteClaim :: Nil)
    }

  private def startOtherClose(some: HasCommitments, spendTx: Transaction) =
    Closing.claimRevokedRemoteCommitTxOutputs(some.commitments, spendTx) -> some match {
      case (Some(claim), close: ClosingData) => BECOME(me STORE close.modify(_.revokedCommit).using(claim +: _), CLOSING)
      case (Some(claim), _) => BECOME(me STORE ClosingData(some.announce, some.commitments, revokedCommit = claim :: Nil), CLOSING)
      // Local, remote and revoked checks have failed and we are negotiating so this must be a peer broadcasted mutual closing tx
      case (None, neg: NegotiationsData) => startMutualClose(neg, spendTx)
      // This is weird, try to spend local commit and hope we're lucky
      case _ => startLocalCurrentClose(some)
    }
}

object Channel {
  val WAIT_FOR_INIT = "WaitForInit"
  val WAIT_FOR_ACCEPT = "WaitForAccept"
  val WAIT_FOR_FUNDING = "WaitForFunding"
  val WAIT_FUNDING_SIGNED = "WaitFundingSigned"
  val WAIT_FUNDING_DONE = "WaitFundingDone"
  val NEGOTIATIONS = "Negotiations"
  val NORMAL = "Normal"
  val SYNC = "Sync"

  // No tears, only dreams now
  val REFUNDING = "Refunding"
  val CLOSING = "Closing"
}

trait ChannelListener {
  def reloadOnBecome(chan: Channel): Unit = {
    // For listener to reload itself without affecting others
    val nullTransition = Tuple4(chan, chan.data, null, chan.state)
    if (onBecome isDefinedAt nullTransition) onBecome(nullTransition)
  }

  type Malfunction = (Channel, Throwable)
  type Incoming = (Channel, ChannelData, Any)
  type Transition = (Channel, ChannelData, String, String)
  def onError: PartialFunction[Malfunction, Unit] = none
  // These two are only called when there is no exception
  def onBecome: PartialFunction[Transition, Unit] = none
  def onProcess: PartialFunction[Incoming, Unit] = none
}