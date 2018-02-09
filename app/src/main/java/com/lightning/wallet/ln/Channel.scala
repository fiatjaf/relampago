package com.lightning.wallet.ln

import octopus.dsl._
import octopus.syntax._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.AddErrorCodes._
import java.util.concurrent.Executors
import fr.acinq.eclair.UInt64
import scala.util.Success

import com.lightning.wallet.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex, Sphinx}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import fr.acinq.bitcoin.Crypto.{Point, PrivateKey, Scalar}
import com.lightning.wallet.ln.Helpers.{Closing, Funding}
import com.lightning.wallet.ln.Tools.{none, runAnd}
import fr.acinq.bitcoin.{Satoshi, Transaction}


abstract class Channel extends StateMachine[ChannelData] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def apply[T](ex: Commitments => T) = Some(data) collect { case some: HasCommitments => ex apply some.commitments }
  def process(change: Any) = Future(me doProcess change) onFailure { case err => events onError me -> err }
  def isOperational = data match { case NormalData(_, _, None, None) => true case _ => false }
  def isOpening = data match { case _: WaitFundingDoneData => true case _ => false }
  var listeners: Set[ChannelListener] = _

  private[this] val events = new ChannelListener {
    override def onError = { case malfunction => for (lst <- listeners if lst.onError isDefinedAt malfunction) lst onError malfunction }
    override def onBecome = { case transition => for (lst <- listeners if lst.onBecome isDefinedAt transition) lst onBecome transition }
    override def onProcess = { case incoming => for (lst <- listeners if lst.onProcess isDefinedAt incoming) lst onProcess incoming }
  }

  def SEND(msg: LightningMessage): Unit
  def CLOSEANDWATCH(close: ClosingData): Unit
  def STORE(content: HasCommitments): HasCommitments
  def UPDATA(d1: ChannelData): Channel = BECOME(d1, state)
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
          .rule(_.htlcMinimumMsat <= 20000L, "Their htlcMinimumMsat too high")
          .rule(_.toSelfDelay <= cmd.localParams.toSelfDelay * 10, "Their toSelfDelay is too high")
          .rule(_.maxHtlcValueInFlightMsat > UInt64(LNParams.maxHtlcValue.amount / 5), "Their maxHtlcValueInFlightMsat is too low")
          .rule(_.channelReserveSatoshis.toDouble / cmd.fundingAmountSat < LNParams.maxReserveToFundingRatio, "Their reserve is too high")
          .rule(_.maxAcceptedHtlcs > 0, "They can accept too few incoming HTLCs")
          .rule(_.dustLimitSatoshis >= 546L, "Their dust limit is too low")

        val result = accept.validate(acceptChannelValidator)
        if (result.isValid) BECOME(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING)
        else throw new LightningException(result.toFieldErrMapping.map(_._1) mkString "\n")


      case (WaitFundingData(announce, cmd, accept), (fundTx: Transaction, outIndex: Int), WAIT_FOR_FUNDING) =>
        // They have accepted our proposal, now let them sign a first commit so we can safely broadcast a funding

        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
          Funding.makeFirstFunderCommitTxs(cmd, accept, fundTx.hash,
            outIndex, accept.firstPerCommitmentPoint)

        val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, cmd.localParams.fundingPrivKey)
        val fundingCreated = FundingCreated(cmd.temporaryChannelId, fundTx.hash, outIndex, localSigOfRemoteTx)
        val firstRemoteCommit = RemoteCommit(0L, remoteSpec, remoteCommitTx.tx.txid, accept.firstPerCommitmentPoint)
        BECOME(WaitFundingSignedData(announce, cmd.localParams, Tools.toLongId(fundTx.hash, outIndex), accept, fundTx,
          localSpec, localCommitTx, firstRemoteCommit), WAIT_FUNDING_SIGNED) SEND fundingCreated


      // They have signed our first commit, we can broadcast a funding tx
      case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED) =>
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
      case (wait @ WaitFundingDoneData(_, None, _, _, _), their: FundingLocked, WAIT_FUNDING_DONE) =>
        // Not storing since they will re-send on restart
        val d1 = wait.modify(_.their) setTo Some(their)
        me UPDATA d1


      // We have already sent them a FundingLocked and now we got one from them so can enter normal state
      case (wait @ WaitFundingDoneData(_, Some(our), _, _, _), their: FundingLocked, WAIT_FUNDING_DONE) =>
        val c1 = wait.commitments.modify(_.remoteNextCommitInfo) setTo Right(their.nextPerCommitmentPoint)
        BECOME(me STORE NormalData(wait.announce, c1), OPEN)


      // We got our lock but their is not yet present so we save ours and just keep waiting for their
      case (wait @ WaitFundingDoneData(_, _, None, _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        val d1 = wait.modify(_.our) setTo Some(our)
        me UPDATA STORE(d1) SEND our


      // We got our lock when their is already present so we can safely enter normal state now
      case (wait @ WaitFundingDoneData(_, _, Some(their), _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
        if wait.fundingTx.txid == tx.txid =>

        val c1 = wait.commitments.modify(_.remoteNextCommitInfo) setTo Right(their.nextPerCommitmentPoint)
        BECOME(me STORE NormalData(wait.announce, c1), OPEN) SEND makeFundingLocked(wait.commitments)


      // OPEN MODE


      case (norm: NormalData, add: UpdateAddHtlc, OPEN) =>
        // Got new incoming HTLC so put it to changes for now
        val c1 = Commitments.receiveAdd(norm.commitments, add)
        me UPDATA norm.copy(commitments = c1)


      case (norm: NormalData, fulfill: UpdateFulfillHtlc, OPEN) =>
        // Got a fulfill for an outgoing HTLC we have sent them earlier
        val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
        me UPDATA norm.copy(commitments = c1)


      case (norm: NormalData, fail: UpdateFailHtlc, OPEN) =>
        // Got a failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFail(norm.commitments, fail)
        me UPDATA norm.copy(commitments = c1)


      case (norm: NormalData, fail: UpdateFailMalformedHtlc, OPEN) =>
        // Got 'malformed' failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFailMalformed(norm.commitments, fail)
        me UPDATA norm.copy(commitments = c1)


      // We can only accept a new HTLC when shutdown is not active
      case (norm: NormalData, cmd: CMDAddHtlc, OPEN) if isOperational =>
        // AND this HTLC is not already in-flight AND it has not been fulfilled already
        val activeHtlcs = Commitments.latestRemoteCommit(norm.commitments).spec.htlcs
        val active = activeHtlcs.exists(_.add.paymentHash == cmd.rpi.pr.paymentHash)

        if (active) throw CMDAddExcept(cmd, ERR_IN_FLIGHT)
        LNParams.bag getPaymentInfo cmd.rpi.pr.paymentHash match {
          // When re-sending an already fulfilled HTLC a peer may provide us with a preimage without routing a payment
          case Success(info: PaymentInfo) if info.actualStatus == SUCCESS => throw CMDAddExcept(cmd, ERR_FULFILLED)
          case Success(info: PaymentInfo) if info.incoming == 1 => throw CMDAddExcept(cmd, ERR_FAILED)

          case _ =>
            // This may be a FAILURE payment which is fine
            val c1 \ updateAddHtlc = Commitments.sendAdd(norm.commitments, cmd)
            me UPDATA norm.copy(commitments = c1) SEND updateAddHtlc
            doProcess(CMDProceed)
        }


      // We're fulfilling an HTLC we got earlier
      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFulfillHtlc, OPEN) =>
        val c1 \ updateFulfillHtlc = Commitments.sendFulfill(commitments, cmd)
        me UPDATA norm.copy(commitments = c1) SEND updateFulfillHtlc


      // Failing an HTLC we got earlier
      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFailHtlc, OPEN) =>
        val c1 \ updateFailHtlc = Commitments.sendFail(commitments, cmd)
        me UPDATA norm.copy(commitments = c1) SEND updateFailHtlc


      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFailMalformedHtlc, OPEN) =>
        val c1 \ updateFailMalformedHtlс = Commitments.sendFailMalformed(commitments, cmd)
        me UPDATA norm.copy(commitments = c1) SEND updateFailMalformedHtlс


      // Fail or fulfill incoming HTLCs
      case (norm: NormalData, CMDHTLCProcess, OPEN) =>
        val minExpiry = LNParams.broadcaster.currentHeight + 6L
        for (Htlc(false, add) <- norm.commitments.remoteCommit.spec.htlcs)
          me doProcess resolveHtlc(LNParams.nodePrivateKey, add, LNParams.bag, minExpiry)

        // And sign once done
        doProcess(CMDProceed)


      case (norm: NormalData, CMDProceed, OPEN)
        // Only if we have a point and something to sign
        if norm.commitments.remoteNextCommitInfo.isRight &&
          (norm.commitments.localChanges.proposed.nonEmpty ||
          norm.commitments.remoteChanges.acked.nonEmpty) =>

        // Propose new remote commit via commit tx sig
        val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
        val c1 \ commitSig = Commitments.sendCommit(norm.commitments, nextRemotePoint)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 SEND commitSig


      case (norm: NormalData, sig: CommitSig, OPEN) =>
        // We received a commit sig from them, now we can update our local commit
        val c1 \ revokeAndAck = Commitments.receiveCommit(norm.commitments, sig)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 SEND revokeAndAck
        doProcess(CMDProceed)


      case (norm: NormalData, rev: RevokeAndAck, OPEN) =>
        // We received a revocation because we sent a commit sig
        val c1 = Commitments.receiveRevocation(norm.commitments, rev)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 doProcess CMDHTLCProcess


      case (norm: NormalData, CMDBestHeight(height), OPEN | SYNC)
        // GUARD: break channel if expired outgoing HTLC exists + 576 blocks of grace period have passed
        if norm.commitments.localCommit.spec.htlcs.exists(htlc => !htlc.incoming && height - 576 >= htlc.add.expiry) ||
          norm.commitments.remoteCommit.spec.htlcs.exists(htlc => htlc.incoming && height - 576 >= htlc.add.expiry) ||
          Commitments.latestRemoteCommit(norm.commitments).spec.htlcs.exists(htlc => htlc.incoming &&
            height - 576 >= htlc.add.expiry) =>

        startLocalClose(norm)


      // SHUTDOWN in WAIT_FUNDING_DONE and OPEN


      case (wait: WaitFundingDoneData, CMDShutdown, WAIT_FUNDING_DONE) =>
        // We have decided to close our channel before it reached a min depth
        me startShutdown NormalData(wait.announce, wait.commitments)


      case (wait: WaitFundingDoneData, remote: Shutdown, WAIT_FUNDING_DONE) =>
        // They have decided to close our channel before it reached a min depth

        val norm = NormalData(wait.announce, wait.commitments)
        val norm1 = norm.modify(_.remoteShutdown) setTo Some(remote)
        // We have their Shutdown so we add ours and start negotiations
        me startShutdown norm1
        doProcess(CMDProceed)


      case (norm @ NormalData(_, commitments, our, their), CMDShutdown, OPEN) =>
        // We have unsigned outgoing HTLCs or already have tried to close this channel cooperatively
        val nope = our.isDefined | their.isDefined | Commitments.localHasUnsignedOutgoing(commitments)
        if (nope) startLocalClose(norm) else me startShutdown norm


      case (norm @ NormalData(_, commitments, _, None), remote: Shutdown, OPEN) =>
        // GUARD: Either they initiate a shutdown or respond to the one we have sent

        val d1 = norm.modify(_.remoteShutdown) setTo Some(remote)
        val nope = Commitments.remoteHasUnsignedOutgoing(commitments)
        // Can't close cooperatively if they have unsigned outgoing HTLCs
        // we should clear our unsigned outgoing HTLCs and then start a shutdown
        if (nope) startLocalClose(norm) else me UPDATA d1 doProcess CMDProceed


      case (norm @ NormalData(_, commitments, None, their), CMDProceed, OPEN)
        // GUARD: got their shutdown, have no unsigned HTLCs, send our and go
        if Commitments.hasNoPendingHtlc(commitments) && their.isDefined =>

        me startShutdown norm
        doProcess(CMDProceed)


      case (NormalData(announce, commitments, our, their), CMDProceed, OPEN)
        // GUARD: got both shutdowns, have no unsigned HTLCs so we can start negotiations
        if Commitments.hasNoPendingHtlc(commitments) && our.isDefined && their.isDefined =>

        val firstProposed = Closing.makeFirstClosing(commitments, our.get.scriptPubKey, their.get.scriptPubKey)
        val neg = NegotiationsData(announce, commitments, our.get, their.get, firstProposed :: Nil)
        BECOME(me STORE neg, NEGOTIATIONS) SEND firstProposed.localClosingSigned


      // SYNC and REFUNDING MODE


      case (refund: RefundingData, cr: ChannelReestablish, REFUNDING)
        // GUARD: We have started a channel with REFUNDING data
        // and explicitly wait for their ChannelReestablish
        if cr.myCurrentPerCommitmentPoint.isDefined =>

        savePointSendError(refund, cr.myCurrentPerCommitmentPoint.get)


      case (norm: NormalData, cr: ChannelReestablish, SYNC)
        // GUARD: normal state but their nextRemoteRevocationNumber is too far away
        if norm.commitments.localCommit.index < cr.nextRemoteRevocationNumber &&
          cr.myCurrentPerCommitmentPoint.isDefined =>

        val refunding = RefundingData(norm.announce, norm.commitments)
        // And they have proved this by providing a correct yourLastPerCommitmentSecret
        val secret = Generators.perCommitSecret(norm.commitments.localParams.shaSeed, cr.nextRemoteRevocationNumber - 1)
        if (cr.yourLastPerCommitmentSecret contains secret) savePointSendError(refunding, cr.myCurrentPerCommitmentPoint.get)
        else throw new LightningException


      case (norm: NormalData, cr: ChannelReestablish, SYNC) =>
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

        BECOME(norm.copy(commitments = c1), OPEN)
        norm.localShutdown foreach SEND
        doProcess(CMDHTLCProcess)


      // We may get this message any time so just save it here
      case (wait: WaitFundingDoneData, CMDConfirmed(tx), SYNC)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        val wait1 = wait.modify(_.our) setTo Some(our)
        me UPDATA STORE(wait1)


      // We're exiting a sync state while waiting for their FundingLocked
      case (wait: WaitFundingDoneData, cr: ChannelReestablish, SYNC) =>
        BECOME(wait, WAIT_FUNDING_DONE)
        wait.our foreach SEND


      // No in-flight HTLCs here, just proceed with negotiations
      case (neg: NegotiationsData, cr: ChannelReestablish, SYNC) =>
        // According to spec we need to re-send a last closing sig here
        val lastSigned = neg.localProposals.head.localClosingSigned
        BECOME(neg, NEGOTIATIONS) SEND lastSigned


      // SYNC: ONLINE/OFFLINE


      case (some: HasCommitments, CMDOnline, SYNC) =>
        val ShaHashesWithIndex(hashes, lastIndex) = some.commitments.remotePerCommitmentSecrets
        val yourLastPerCommitmentSecret = lastIndex.map(ShaChain.moves).flatMap(ShaChain getHash hashes).getOrElse(Sphinx zeroes 32)
        val myCurrentPerCommitmentPoint = Generators.perCommitPoint(some.commitments.localParams.shaSeed, some.commitments.localCommit.index)
        me SEND ChannelReestablish(some.commitments.channelId, some.commitments.localCommit.index + 1, some.commitments.remoteCommit.index,
          Some apply Scalar(yourLastPerCommitmentSecret), Some apply myCurrentPerCommitmentPoint)


      case (wait: WaitFundingDoneData, CMDOffline, WAIT_FUNDING_DONE) => BECOME(wait, SYNC)
      case (negs: NegotiationsData, CMDOffline, NEGOTIATIONS) => BECOME(negs, SYNC)
      case (norm: NormalData, CMDOffline, OPEN) => BECOME(norm, SYNC)


      // NEGOTIATIONS MODE


      case (neg @ NegotiationsData(_, commitments,
        localShutdown, remoteShutdown, ClosingTxProposed(_, localClosingSigned) +: _, _),
        ClosingSigned(channelId, remoteClosingFee, remoteClosingSig), NEGOTIATIONS) =>

        val ClosingTxProposed(closing, closingSigned) = Closing.makeClosing(commitments,
          Satoshi(remoteClosingFee), localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)

        val signedClose = Scripts.addSigs(closing, commitments.localParams.fundingPrivKey.publicKey,
          commitments.remoteParams.fundingPubkey, closingSigned.signature, remoteClosingSig)

        Scripts checkSpendable signedClose match {
          case None => throw new LightningException
          case Some(okClose) if remoteClosingFee == localClosingSigned.feeSatoshis =>
            // Our current and their proposed fees are equal for this tx, can broadcast
            startMutualClose(neg, okClose.tx)

          case Some(okClose) =>
            val nextCloseFee = Satoshi(localClosingSigned.feeSatoshis + remoteClosingFee) / 4 * 2
            val nextProposed = Closing.makeClosing(commitments, nextCloseFee, localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)
            if (remoteClosingFee == nextCloseFee.amount) startMutualClose(neg, okClose.tx) SEND nextProposed.localClosingSigned else {
              val d1 = me STORE neg.copy(lastSignedTx = Some(okClose), localProposals = nextProposed +: neg.localProposals)
              me UPDATA d1 SEND nextProposed.localClosingSigned
            }
        }


      // HANDLE FUNDING SPENT


      case (refund: RefundingData, CMDSpent(spendTx), REFUNDING)
        // GUARD: we have lost our state and asked them to spend their local commit
        if spendTx.txIn.exists(_.outPoint == refund.commitments.commitInput.outPoint) =>
        // commitments.remoteCommit has to be updated to their myCurrentPerCommitmentPoint at this point
        val rcp = Closing.claimRemoteMainOutput(refund.commitments, refund.commitments.remoteCommit, spendTx)
        BECOME(me STORE ClosingData(refund.announce, refund.commitments, remoteCommit = rcp :: Nil), CLOSING)


      case (some: HasCommitments, CMDSpent(tx), _)
        // GUARD: something which spends our funding is broadcasted, must react
        if tx.txIn.exists(_.outPoint == some.commitments.commitInput.outPoint) =>
        val revokedCommitOpt = Closing.claimRevokedRemoteCommitTxOutputs(some.commitments, tx)
        val nextRemoteCommitEither = some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit)

        Tuple3(revokedCommitOpt, nextRemoteCommitEither, some) match {
          case (_, _, close: ClosingData) if close.mutualClose.exists(_.txid == tx.txid) => Tools log s"Existing mutual $tx"
          case (_, _, close: ClosingData) if close.localCommit.exists(_.commitTx.txid == tx.txid) => Tools log s"Existing local $tx"
          case (_, _, close: ClosingData) if close.localProposals.exists(_.unsignedTx.tx.txid == tx.txid) => startMutualClose(close, tx)
          case (_, _, negs: NegotiationsData) if negs.localProposals.exists(_.unsignedTx.tx.txid == tx.txid) => startMutualClose(negs, tx)
          case (Some(claim), _, closingData: ClosingData) => BECOME(me STORE closingData.modify(_.revokedCommit).using(claim +: _), CLOSING)
          case (Some(claim), _, _) => BECOME(me STORE ClosingData(some.announce, some.commitments, revokedCommit = claim :: Nil), CLOSING)
          case (_, Left(nextRemote), _) if nextRemote.txid == tx.txid => startRemoteNextClose(some, nextRemote)
          case _ if some.commitments.remoteCommit.txid == tx.txid => startRemoteCurrentClose(some)
          case _ => startLocalClose(some)
        }


      // HANDLE INITIALIZATION


      case (null, ref: RefundingData, null) => BECOME(ref, REFUNDING)
      case (null, close: ClosingData, null) => BECOME(close, CLOSING)
      case (null, init: InitData, null) => BECOME(init, WAIT_FOR_INIT)
      case (null, wait: WaitFundingDoneData, null) => BECOME(wait, SYNC)
      case (null, neg: NegotiationsData, null) => BECOME(neg, SYNC)
      case (null, norm: NormalData, null) => BECOME(norm, SYNC)


      // MISC


      case (_, err: Error, WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FOR_FUNDING | WAIT_FUNDING_SIGNED) =>
        // May happen if remote peer does not like any of our proposed parameters, nothing to lose here
        val remoteErrorMessage = new String(err.data, "UTF-8")
        throw new LightningException(remoteErrorMessage)


      case (some: HasCommitments, err: Error, WAIT_FUNDING_DONE | NEGOTIATIONS | OPEN | SYNC) =>
        // GUARD: we only react on connection level remote errors or those related to our channel
        // REFUNDING is an exception here, we CAN NOT start a local close in that state
        startLocalClose(some)


      // CMDShutdown in WAIT_FUNDING_DONE and OPEN is handled differently above
      case (some: HasCommitments, CMDShutdown, NEGOTIATIONS | SYNC) => startLocalClose(some)
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
      me UPDATA norm.copy(commitments = c1) SEND msg
      doProcess(CMDProceed)
    }

  private def makeFundingLocked(cs: Commitments) = {
    val point = Generators.perCommitPoint(cs.localParams.shaSeed, 1L)
    FundingLocked(cs.channelId, point)
  }

  private def startShutdown(norm: NormalData) = {
    val finalScriptPubKey = norm.commitments.localParams.defaultFinalScriptPubKey
    val localShutdown = Shutdown(norm.commitments.channelId, finalScriptPubKey)
    val norm1 = norm.modify(_.localShutdown) setTo Some(localShutdown)
    BECOME(norm1, OPEN) SEND localShutdown
  }

  private def savePointSendError(refund: RefundingData, point: Point) = {
    val d1 = refund.modify(_.commitments.remoteCommit.remotePerCommitmentPoint) setTo point
    val msg = "Please be so kind as to spend your local commit" getBytes "UTF-8"
    BECOME(d1, REFUNDING) SEND Error(refund.commitments.channelId, msg)
  }

  private def startMutualClose(some: HasCommitments, tx: Transaction) = some match {
    case closingData: ClosingData => BECOME(me STORE closingData.copy(mutualClose = tx +: closingData.mutualClose), CLOSING)
    case neg: NegotiationsData => BECOME(me STORE ClosingData(neg.announce, neg.commitments, neg.localProposals, tx :: Nil), CLOSING)
    case _ => BECOME(me STORE ClosingData(some.announce, some.commitments, Nil, tx :: Nil), CLOSING)
  }

  private def startLocalClose(some: HasCommitments) =
    // Something went wrong and we decided to spend our CURRENT commit transaction
    // BUT if we're at negotiations AND we have a signed mutual closing tx then send it
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, LNParams.bag) -> some match {
      case (_, neg: NegotiationsData) if neg.lastSignedTx.isDefined => startMutualClose(neg, neg.lastSignedTx.get.tx)
      case (localClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(localCommit = localClaim :: Nil)
      case (localClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, localCommit = localClaim :: Nil)
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
}

object Channel {
  val WAIT_FOR_INIT = "WAIT-FOR-INIT"
  val WAIT_FOR_ACCEPT = "WAIT-FOR-ACCEPT"
  val WAIT_FOR_FUNDING = "WAIT-FOR-FUNDING"
  val WAIT_FUNDING_SIGNED = "WAIT-FUNDING-SIGNED"
  val WAIT_FUNDING_DONE = "WAIT-FUNDING-DONE"
  val NEGOTIATIONS = "NEGOTIATIONS"
  val OPEN = "OPEN"
  val SYNC = "SYNC"

  // No tears, only dreams now
  val REFUNDING = "REFUNDING"
  val CLOSING = "CLOSING"

  def myBalance(chan: Channel) =
    chan(_.localCommit.spec.toLocalMsat)
}

trait ChannelListener {
  def nullOnBecome(chan: Channel): Unit = {
    // For listener to reload itself without affecting others
    val nullTransition = Tuple4(chan, chan.data, null, chan.state)
    if (onBecome isDefinedAt nullTransition) onBecome(nullTransition)
  }

  type Malfunction = (Channel, Throwable)
  type Incoming = (Channel, ChannelData, Any)
  type Transition = (Channel, ChannelData, String, String)
  def onError: PartialFunction[Malfunction, Unit] = none
  def onBecome: PartialFunction[Transition, Unit] = none
  def onProcess: PartialFunction[Incoming, Unit] = none
}