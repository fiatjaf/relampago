package com.lightning.walletapp.ln

import com.softwaremill.quicklens._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.PaymentInfo._
import java.util.concurrent.Executors
import fr.acinq.eclair.UInt64

import com.lightning.walletapp.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex, Sphinx}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import com.lightning.walletapp.ln.Helpers.{Closing, Funding}
import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}
import com.lightning.walletapp.ln.Tools.{none, runAnd}
import fr.acinq.bitcoin.Crypto.{Point, Scalar}
import scala.util.{Failure, Success}


abstract class Channel extends StateMachine[ChannelData] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def apply[T](ex: Commitments => T) = Some(data) collect { case some: HasCommitments => ex apply some.commitments }
  def process(change: Any): Unit = Future(me doProcess change) onFailure { case err => events onException me -> err }
  var listeners: Set[ChannelListener] = _

  private[this] val events = new ChannelListener {
    override def onProcessSuccess = { case ps => for (lst <- listeners if lst.onProcessSuccess isDefinedAt ps) lst onProcessSuccess ps }
    override def onException = { case failure => for (lst <- listeners if lst.onException isDefinedAt failure) lst onException failure }
    override def onBecome = { case transition => for (lst <- listeners if lst.onBecome isDefinedAt transition) lst onBecome transition }

    override def outPaymentAccepted(rd: RoutingData) = for (lst <- listeners) lst outPaymentAccepted rd
    override def fulfillReceived(ok: UpdateFulfillHtlc) = for (lst <- listeners) lst fulfillReceived ok
    override def sentSig(cs: Commitments) = for (lst <- listeners) lst sentSig cs
    override def settled(cs: Commitments) = for (lst <- listeners) lst settled cs
  }

  def ASKREFUNDTX(ref: RefundingData): Unit
  def ASKREFUNDPEER(some: HasCommitments, point: Point)

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
      case (InitData(announce), cmd: CMDOpenChannel, WAIT_FOR_INIT) =>
        BECOME(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT) SEND OpenChannel(LNParams.chainHash, cmd.tempChanId,
          cmd.fundingSat, cmd.pushMsat, cmd.localParams.dustLimit.amount, cmd.localParams.maxHtlcValueInFlightMsat,
          cmd.localParams.channelReserveSat, LNParams.minHtlcValue.amount, cmd.initialFeeratePerKw, cmd.localParams.toSelfDelay,
          cmd.localParams.maxAcceptedHtlcs, cmd.localParams.fundingPrivKey.publicKey, cmd.localParams.revocationBasepoint,
          cmd.localParams.paymentBasepoint, cmd.localParams.delayedPaymentBasepoint, cmd.localParams.htlcBasepoint,
          Generators.perCommitPoint(cmd.localParams.shaSeed, index = 0L), channelFlags = 0.toByte)


      case (InitData(announce), Tuple2(localParams: LocalParams, open: OpenChannel), WAIT_FOR_INIT) =>
        if (LNParams.chainHash != open.chainHash) throw new LightningException("They have provided a wrong chain hash")
        if (open.fundingSatoshis < LNParams.minCapacitySat) throw new LightningException("Their proposed capacity is too small")
        if (open.channelFlags << ~0 < 0) throw new LightningException("They are offering a public channel and we only support private ones")
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

        val firstPerCommitPoint = Generators.perCommitPoint(localParams.shaSeed, 0L)
        val accept = AcceptChannel(open.temporaryChannelId, localParams.dustLimit.amount,
          localParams.maxHtlcValueInFlightMsat, localParams.channelReserveSat, LNParams.minDepth,
          LNParams.minHtlcValue.amount, localParams.toSelfDelay, localParams.maxAcceptedHtlcs,
          localParams.fundingPrivKey.publicKey, localParams.revocationBasepoint,
          localParams.paymentBasepoint, localParams.delayedPaymentBasepoint,
          localParams.htlcBasepoint, firstPerCommitPoint)

        BECOME(WaitFundingCreatedRemote(announce, localParams, remoteParams = AcceptChannel(open.temporaryChannelId, open.dustLimitSatoshis,
          open.maxHtlcValueInFlightMsat, open.channelReserveSatoshis, open.htlcMinimumMsat, minimumDepth = 6, open.toSelfDelay, open.maxAcceptedHtlcs,
          open.fundingPubkey, open.revocationBasepoint, open.paymentBasepoint, open.delayedPaymentBasepoint, open.htlcBasepoint,
          open.firstPerCommitmentPoint), open), WAIT_FOR_FUNDING) SEND accept


      case (wait @ WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT) if accept.temporaryChannelId == cmd.tempChanId =>
        if (accept.dustLimitSatoshis > cmd.localParams.channelReserveSat) throw new LightningException("Our channel reserve is less than their dust")
        if (UInt64(10000L) > accept.maxHtlcValueInFlightMsat) throw new LightningException("Their maxHtlcValueInFlightMsat is too low")
        if (accept.channelReserveSatoshis > cmd.fundingSat / 10) throw new LightningException("Their proposed reserve is too high")
        if (accept.toSelfDelay > LNParams.maxToSelfDelay) throw new LightningException("Their toSelfDelay is too high")
        if (accept.dustLimitSatoshis < 546L) throw new LightningException("Their on-chain dust limit is too low")
        if (accept.maxAcceptedHtlcs > 483) throw new LightningException("They can accept too many payments")
        if (accept.htlcMinimumMsat > 10000L) throw new LightningException("Their htlcMinimumMsat too high")
        if (accept.maxAcceptedHtlcs < 1) throw new LightningException("They can accept too few payments")
        if (accept.minimumDepth > 6L) throw new LightningException("Their minimumDepth is too high")
        BECOME(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING)


      case (WaitFundingCreatedRemote(announce, localParams, accept, open),
        FundingCreated(_, txHash, outIndex, remoteSig), WAIT_FOR_FUNDING) =>

        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
          Funding.makeFirstCommitTxs(localParams, open.fundingSatoshis, open.pushMsat,
            open.feeratePerKw, accept, txHash, outIndex, open.firstPerCommitmentPoint)

        val signedLocalCommitTx = Scripts.addSigs(localCommitTx, localParams.fundingPrivKey.publicKey,
          accept.fundingPubkey, Scripts.sign(localCommitTx, localParams.fundingPrivKey), remoteSig)

        if (Scripts.checkValid(signedLocalCommitTx).isSuccess) {
          val channelId = Tools.toLongId(fundingHash = txHash, outIndex)
          val rc = RemoteCommit(index = 0L, remoteSpec, remoteCommitTx.tx.txid, open.firstPerCommitmentPoint)
          val wfcs = WaitFundingSignedCore(localParams, channelId, accept, localSpec, signedLocalCommitTx, rc)
          val wait = WaitBroadcastRemoteData(announce, wfcs, txHash, wfcs makeCommitments signedLocalCommitTx)
          val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, localParams.fundingPrivKey)
          val fundingSigned = FundingSigned(channelId, localSigOfRemoteTx)
          BECOME(wait, WAIT_FUNDING_DONE) SEND fundingSigned
        } else throw new LightningException


      // LOCAL FUNDER FLOW


      case (WaitFundingData(announce, cmd, accept), CMDFunding(fundTx), WAIT_FOR_FUNDING) =>
        // They have accepted our proposal, let them sign a first commit so we can broadcast a funding later
        if (fundTx.txOut(cmd.batch.fundOutIdx).amount.amount != cmd.batch.fundingAmountSat) throw new LightningException
        val core \ fundingCreatedMessage = signLocalFunding(cmd, accept, txHash = fundTx.hash, outIndex = cmd.batch.fundOutIdx)
        BECOME(WaitFundingSignedData(announce, core, fundTx), WAIT_FUNDING_SIGNED) SEND fundingCreatedMessage


      // They have signed our first commit, we can broadcast a local funding tx
      case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED) =>
        verifyTheirFirstRemoteCommitTxSig(core = wait.core, remoteSig = remote.signature) match {
          case Success(commitTx) => BECOME(wait makeWaitFundingDoneData commitTx, WAIT_FUNDING_DONE)
          case _ => BECOME(wait, CLOSING)
        }


      // REMOTE FUNDER/FUNDEE FLOW


      // We have asked an external funder to prepare a funding tx and got a positive response
      case (WaitFundingData(announce, cmd, accept), ready: FundingTxReady, WAIT_FOR_FUNDING) =>
        val core \ fundingCreatedMessage = signLocalFunding(cmd, accept, ready.txHash, ready.outIndex)
        val data = WaitFundingSignedRemoteData(announce, core, txHash = ready.txHash)
        BECOME(data, WAIT_FUNDING_SIGNED) SEND fundingCreatedMessage


      // We have asked a remote peer to sign our first commit and got a remote signature
      case (wait: WaitFundingSignedRemoteData, remote: FundingSigned, WAIT_FUNDING_SIGNED) =>
        verifyTheirFirstRemoteCommitTxSig(core = wait.core, remoteSig = remote.signature) match {
          case Success(commitTx) => BECOME(wait makeWaitBroadcastRemoteData commitTx, WAIT_FUNDING_DONE)
          case _ => BECOME(wait, CLOSING)
        }


      // Funder was not able to broadcast a funding tx, record their fail locally
      case (wait: WaitBroadcastRemoteData, fail: Fail, OFFLINE | WAIT_FUNDING_DONE) =>
        val d1 = me STORE wait.copy(fail = Some apply fail)
        me UPDATA d1


      // We have asked an external funder to broadcast a funding tx and got an onchain event
      case (wait: WaitBroadcastRemoteData, CMDSpent(fundTx), OFFLINE | WAIT_FUNDING_DONE) if wait.txHash == fundTx.hash =>
        val d1 = me STORE WaitFundingDoneData(wait.announce, our = None, their = None, fundTx, wait.commitments)
        me UPDATA d1


      // FUNDING TX IS BROADCASTED AT THIS POINT


      case (wait: WaitFundingDoneData, their: FundingLocked, WAIT_FUNDING_DONE) =>
        // No need to store their FundingLocked because it gets re-sent on reconnect
        if (wait.our.isEmpty) me UPDATA wait.copy(their = Some apply their)
        else becomeOpen(wait, their)


      case (wait: WaitFundingDoneData, CMDConfirmed(fundTx), WAIT_FUNDING_DONE)
        // GUARD: this funding transaction blongs to this exact channel
        if wait.fundingTx.txid == fundTx.txid =>

        // Create and store our FundingLocked
        val our = makeFundingLocked(wait.commitments)
        val wait1 = me STORE wait.copy(our = Some apply our)
        if (wait.their.isEmpty) me UPDATA wait1 SEND our
        else becomeOpen(wait, wait.their.get) SEND our


      // OPEN MODE


      case (norm: NormalData, hop: Hop, OPEN | OFFLINE) =>
        // Got either an empty Hop with shortChannelId or a final one
        // do not trigger listeners and silently update a current state
        val d1 = norm.modify(_.commitments.extraHop) setTo Some(hop)
        data = me STORE d1


      case (norm: NormalData, add: UpdateAddHtlc, OPEN) =>
        // Got new incoming HTLC so put it to changes for now
        val c1 = Commitments.receiveAdd(norm.commitments, add)
        me UPDATA norm.copy(commitments = c1)


      case (norm: NormalData, fulfill: UpdateFulfillHtlc, OPEN) =>
        // Got a fulfill for an outgoing HTLC we have sent them earlier
        val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
        me UPDATA norm.copy(commitments = c1)
        events fulfillReceived fulfill


      case (norm: NormalData, fail: UpdateFailHtlc, OPEN) =>
        // Got a failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFail(norm.commitments, fail)
        me UPDATA norm.copy(commitments = c1)


      case (norm: NormalData, fail: UpdateFailMalformedHtlc, OPEN) =>
        // Got 'malformed' failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFailMalformed(norm.commitments, fail)
        me UPDATA norm.copy(commitments = c1)


      // We can send a new HTLC when channel is both operational and online
      case (norm: NormalData, rd: RoutingData, OPEN) if isOperational(me) =>
        val c1 \ updateAddHtlc = Commitments.sendAdd(norm.commitments, rd)
        me UPDATA norm.copy(commitments = c1) SEND updateAddHtlc
        events outPaymentAccepted rd
        doProcess(CMDProceed)


      // We're fulfilling an HTLC we got from them earlier
      case (norm: NormalData, cmd: CMDFulfillHtlc, OPEN) => for {
        // this is a special case where we don't throw if cross signed HTLC is not found
        // such a case may happen when we have already fulfilled it just before connection got lost
        add <- Commitments.getHtlcCrossSigned(norm.commitments, incomingRelativeToLocal = true, cmd.id)
        updateFulfillHtlc = UpdateFulfillHtlc(norm.commitments.channelId, cmd.id, cmd.preimage)

        if updateFulfillHtlc.paymentHash == add.paymentHash
        c1 = Commitments.addLocalProposal(norm.commitments, updateFulfillHtlc)
      } me UPDATA norm.copy(commitments = c1) SEND updateFulfillHtlc


      // Failing an HTLC we got earlier
      case (norm: NormalData, cmd: CMDFailHtlc, OPEN) =>
        val c1 \ updateFailHtlc = Commitments.sendFail(norm.commitments, cmd)
        me UPDATA norm.copy(commitments = c1) SEND updateFailHtlc


      case (norm: NormalData, cmd: CMDFailMalformedHtlc, OPEN) =>
        val c1 \ updateFailMalformedHtlс = Commitments.sendFailMalformed(norm.commitments, cmd)
        me UPDATA norm.copy(commitments = c1) SEND updateFailMalformedHtlс


      // Fail or fulfill incoming HTLCs
      case (norm: NormalData, CMDHTLCProcess, OPEN) =>
        for (Htlc(false, add) <- norm.commitments.remoteCommit.spec.htlcs)
          me doProcess resolveHtlc(LNParams.nodePrivateKey, add, LNParams.bag)

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
        events sentSig c1


      case (norm: NormalData, sig: CommitSig, OPEN) =>
        // We received a commit sig from them, now we can update our local commit
        val c1 \ revokeAndAck = Commitments.receiveCommit(norm.commitments, sig)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 SEND revokeAndAck
        // Clear remote commit first
        doProcess(CMDProceed)
        events settled c1


      case (norm: NormalData, rev: RevokeAndAck, OPEN) =>
        // We received a revocation because we sent a commit sig
        val c1 = Commitments.receiveRevocation(norm.commitments, rev)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 doProcess CMDHTLCProcess


      case (norm: NormalData, fee: UpdateFee, OPEN) if !norm.commitments.localParams.isFunder =>
        val d1 = norm.modify(_.commitments) setTo Commitments.receiveFee(norm.commitments, fee)
        me UPDATA d1


      case (norm: NormalData, CMDFeerate(satPerKw), OPEN) if norm.commitments.localParams.isFunder =>
        val shouldUpdate = LNParams.shouldUpdateFee(satPerKw, norm.commitments.localCommit.spec.feeratePerKw)
        if (shouldUpdate) Commitments.sendFee(norm.commitments, satPerKw) foreach { case c1 \ feeUpdateMessage =>
          // We send a fee update if current chan unspendable reserve + commitTx fee can afford it
          // otherwise we fail silently in hope that fee will drop or we will receive a payment
          me UPDATA norm.copy(commitments = c1) SEND feeUpdateMessage
          doProcess(CMDProceed)
        }


      case (norm: NormalData, cmd: CMDBestHeight, OPEN | OFFLINE) =>
        val expiredPayment = Commitments.findExpiredHtlc(norm.commitments, cmd)
        if (expiredPayment.nonEmpty) throw HTLCExpiryException(norm, expiredPayment.get)


      // SHUTDOWN in WAIT_FUNDING_DONE


      case (wait: WaitFundingDoneData, CMDShutdown(scriptPubKey), WAIT_FUNDING_DONE) =>
        // We have decided to cooperatively close our channel before it has reached a min depth
        val finalKey = scriptPubKey getOrElse wait.commitments.localParams.defaultFinalScriptPubKey
        startShutdown(NormalData(wait.announce, wait.commitments), finalKey)


      case (wait: WaitFundingDoneData, remote: Shutdown, WAIT_FUNDING_DONE) =>
        // They have decided to close our channel before it reached a min depth

        val norm = NormalData(wait.announce, wait.commitments)
        val norm1 = norm.modify(_.remoteShutdown) setTo Some(remote)
        // We just got their Shutdown so we add ours and start negotiations
        startShutdown(norm1, wait.commitments.localParams.defaultFinalScriptPubKey)
        doProcess(CMDProceed)


      // SHUTDOWN in OPEN


      case (norm @ NormalData(_, commitments, our, their), CMDShutdown(scriptPubKey), OPEN) =>
        // We may have unsigned outgoing HTLCs or already have tried to close this channel cooperatively
        val nope = our.isDefined | their.isDefined | Commitments.localHasUnsignedOutgoing(commitments)
        val finalKey = scriptPubKey getOrElse commitments.localParams.defaultFinalScriptPubKey
        if (nope) startLocalClose(norm) else startShutdown(norm, finalKey)


      // Either they initiate a shutdown or respond to the one we have sent
      // should sign our unsigned outgoing HTLCs if present and then start shutdown
      case (norm @ NormalData(_, commitments, _, None), remote: Shutdown, OPEN) =>

        val d1 = norm.modify(_.remoteShutdown) setTo Some(remote)
        val canNotShutdown = Commitments.remoteHasUnsignedOutgoing(commitments)
        if (canNotShutdown) startLocalClose(norm) else me UPDATA d1 doProcess CMDProceed


      // We have nothing to sign so check if maybe we are in valid shutdown state
      case (norm @ NormalData(announce, commitments, our, their), CMDProceed, OPEN)
        // GUARD: only consider this if we have nothing in-flight
        if inFlightHtlcs(me).isEmpty =>

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
            startShutdown(norm, commitments.localParams.defaultFinalScriptPubKey)
            doProcess(CMDProceed)

          // Not ready
          case _ =>
        }


      // SYNC and REFUNDING MODE


      case (ref: RefundingData, cr: ChannelReestablish, REFUNDING) =>
        cr.myCurrentPerCommitmentPoint -> ref.remoteLatestPoint match {
          case _ \ Some(ourSavedPoint) => ASKREFUNDPEER(ref, ourSavedPoint)
          case Some(theirPoint) \ _ => ASKREFUNDPEER(ref, theirPoint)
          case _ =>
        }

      case (norm: NormalData, cr: ChannelReestablish, OFFLINE)
        // GUARD: we have started in NORMAL state but their nextRemoteRevocationNumber is too far away
        if norm.commitments.localCommit.index < cr.nextRemoteRevocationNumber && cr.myCurrentPerCommitmentPoint.isDefined =>
        val secret = Generators.perCommitSecret(norm.commitments.localParams.shaSeed, cr.nextRemoteRevocationNumber - 1)
        if (cr.yourLastPerCommitmentSecret contains secret) ASKREFUNDPEER(norm, cr.myCurrentPerCommitmentPoint.get)
        else throw new LightningException


      case (norm: NormalData, cr: ChannelReestablish, OFFLINE) =>
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


      // We're exiting a sync state while funding tx is still not provided
      case (remote: WaitBroadcastRemoteData, cr: ChannelReestablish, OFFLINE) =>
        // Need to check whether a funding is present in a listener
        BECOME(remote, WAIT_FUNDING_DONE)


      // We're exiting a sync state while waiting for their FundingLocked
      case (wait: WaitFundingDoneData, cr: ChannelReestablish, OFFLINE) =>
        BECOME(wait, WAIT_FUNDING_DONE)
        wait.our foreach SEND


      // No in-flight HTLCs here, just proceed with negotiations
      case (neg: NegotiationsData, cr: ChannelReestablish, OFFLINE) =>
        // Last closing signed may be empty if we are not a funder of this channel
        val lastClosingSignedOpt = neg.localProposals.headOption.map(_.localClosingSigned)
        neg.localShutdown +: lastClosingSignedOpt.toVector foreach SEND
        BECOME(neg, NEGOTIATIONS)


      // SYNC: ONLINE/OFFLINE


      case (some: HasCommitments, CMDOnline, OFFLINE) =>
        // According to BOLD a first message on connection should be reestablish
        // will specifically NOT work in REFUNDING to not let them know beforehand
        me SEND makeReestablish(some, some.commitments.localCommit.index + 1)


      case (some: HasCommitments, newAnn: NodeAnnouncement, OFFLINE)
        if some.announce.nodeId == newAnn.nodeId && Announcements.checkSig(newAnn) =>
        // Node was OFFLINE for a long time so we have initiated a new announcement search
        data = me STORE some.modify(_.announce).setTo(newAnn)


      case (wait: WaitBroadcastRemoteData, CMDOffline, WAIT_FUNDING_DONE) => BECOME(wait, OFFLINE)
      case (wait: WaitFundingDoneData, CMDOffline, WAIT_FUNDING_DONE) => BECOME(wait, OFFLINE)
      case (negs: NegotiationsData, CMDOffline, NEGOTIATIONS) => BECOME(negs, OFFLINE)
      case (norm: NormalData, CMDOffline, OPEN) => BECOME(norm, OFFLINE)


      // NEGOTIATIONS MODE


      case (neg @ NegotiationsData(_, commitments, localShutdown, remoteShutdown, localProposals, _),
        ClosingSigned(channelId, remoteClosingFee, remoteClosingSig), NEGOTIATIONS) =>

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

        Scripts checkValid signedClose match {
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

          case Failure(why) =>
            startLocalClose(neg)
            // This will show error details to user
            throw new LightningException(why.getMessage)
        }


      // HANDLE FUNDING SPENT


      case (RefundingData(announce, Some(remoteLatestPoint), commitments), CMDSpent(spendTx), REFUNDING)
        // GUARD: we have got a remote commit which we asked them to spend and we have their point
        if spendTx.txIn.exists(input => commitments.commitInput.outPoint == input.outPoint) =>
        val rcp = Closing.claimRemoteMainOutput(commitments, remoteLatestPoint, spendTx)
        val d1 = ClosingData(announce, commitments, refundRemoteCommit = rcp :: Nil)
        BECOME(me STORE d1, CLOSING)


      case (some: HasCommitments, CMDSpent(tx), _)
        // GUARD: tx which spends our funding is broadcasted, must react
        if tx.txIn.exists(input => some.commitments.commitInput.outPoint == input.outPoint) =>
        val revokedOpt = Closing.claimRevokedRemoteCommitTxOutputs(some.commitments, tx, LNParams.bag)
        val nextRemoteCommitEither = some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit)

        Tuple3(revokedOpt, nextRemoteCommitEither, some) match {
          case (_, _, close: ClosingData) if close.refundRemoteCommit.nonEmpty => Tools log s"Existing refund"
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


      case Tuple3(null, ref: RefundingData, null) =>
        if (ref.remoteLatestPoint.isDefined) ASKREFUNDTX(ref)
        super.become(ref, REFUNDING)

      case (null, close: ClosingData, null) => super.become(close, CLOSING)
      case (null, init: InitData, null) => super.become(init, WAIT_FOR_INIT)
      case (null, wait: WaitFundingDoneData, null) => super.become(wait, OFFLINE)
      case (null, wait: WaitBroadcastRemoteData, null) => super.become(wait, OFFLINE)
      case (null, negs: NegotiationsData, null) => super.become(negs, OFFLINE)
      case (null, norm: NormalData, null) => super.become(norm, OFFLINE)


      // ENDING A CHANNEL


      case (some: HasCommitments, _: CMDShutdown, NEGOTIATIONS | OFFLINE) =>
        // Disregard custom scriptPubKey and always refund to local wallet
        // CMDShutdown in WAIT_FUNDING_DONE and OPEN may be cooperative
        startLocalClose(some)


      case _ =>
    }

    // Change has been successfully processed
    events onProcessSuccess Tuple3(me, data, change)
  }

  def makeReestablish(some: HasCommitments, nextRemoteRevocationNumber: Long) = {
    val ShaHashesWithIndex(hashes, lastIndex) = some.commitments.remotePerCommitmentSecrets
    val yourLastPerCommitmentSecret = lastIndex.map(ShaChain.moves).flatMap(ShaChain getHash hashes).getOrElse(Sphinx zeroes 32)
    val myCurrentPerCommitmentPoint = Generators.perCommitPoint(some.commitments.localParams.shaSeed, some.commitments.localCommit.index)
    ChannelReestablish(some.commitments.channelId, nextRemoteRevocationNumber, some.commitments.remoteCommit.index,
      Some apply Scalar(yourLastPerCommitmentSecret), Some apply myCurrentPerCommitmentPoint)
  }

  private def signLocalFunding(cmd: CMDOpenChannel, accept: AcceptChannel, txHash: BinaryData, outIndex: Int) = {
    val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Funding.makeFirstCommitTxs(cmd.localParams, cmd.fundingSat,
      cmd.pushMsat, cmd.initialFeeratePerKw, accept, txHash, outIndex, accept.firstPerCommitmentPoint)

    val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, cmd.localParams.fundingPrivKey)
    val firstRemoteCommit = RemoteCommit(index = 0L, remoteSpec, remoteCommitTx.tx.txid, accept.firstPerCommitmentPoint)
    val wfsc = WaitFundingSignedCore(cmd.localParams, Tools.toLongId(txHash, outIndex), accept, localSpec, localCommitTx, firstRemoteCommit)
    wfsc -> FundingCreated(cmd.tempChanId, txHash, outIndex, localSigOfRemoteTx)
  }

  private def verifyTheirFirstRemoteCommitTxSig(core: WaitFundingSignedCore, remoteSig: BinaryData) =
    Scripts checkValid Scripts.addSigs(core.localCommitTx, core.localParams.fundingPrivKey.publicKey,
      core.remoteParams.fundingPubkey, Scripts.sign(core.localCommitTx, core.localParams.fundingPrivKey), remoteSig)

  private def makeFundingLocked(cs: Commitments) = {
    val first = Generators.perCommitPoint(cs.localParams.shaSeed, 1L)
    FundingLocked(cs.channelId, nextPerCommitmentPoint = first)
  }

  private def becomeOpen(wait: WaitFundingDoneData, their: FundingLocked) = {
    val theirFirstPerCommitmentPoint = Right apply their.nextPerCommitmentPoint
    val c1 = wait.commitments.copy(remoteNextCommitInfo = theirFirstPerCommitmentPoint)
    BECOME(me STORE NormalData(wait.announce, c1), OPEN)
  }

  private def startShutdown(norm: NormalData, finalScriptPubKey: BinaryData) = {
    val localShutdownMessage = Shutdown(norm.commitments.channelId, finalScriptPubKey)
    val norm1 = norm.modify(_.localShutdown) setTo Some(localShutdownMessage)
    BECOME(me STORE norm1, OPEN) SEND localShutdownMessage
  }

  private def startMutualClose(some: HasCommitments, tx: Transaction) = some match {
    case closingData: ClosingData => BECOME(me STORE closingData.copy(mutualClose = tx +: closingData.mutualClose), CLOSING)
    case neg: NegotiationsData => BECOME(me STORE ClosingData(neg.announce, neg.commitments, neg.localProposals, tx :: Nil), CLOSING)
    case _ => BECOME(me STORE ClosingData(some.announce, some.commitments, Nil, tx :: Nil), CLOSING)
  }

  def startLocalClose(some: HasCommitments): Unit =
    // Something went wrong and we decided to spend our CURRENT commit transaction
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

  // Operational
  val WAIT_FUNDING_DONE = "OPENING"
  val NEGOTIATIONS = "NEGOTIATIONS"
  val OFFLINE = "OFFLINE"
  val OPEN = "OPEN"

  // No tears, only dreams now
  val REFUNDING = "REFUNDING"
  val CLOSING = "CLOSING"

  def inFlightHtlcs(chan: Channel) = chan(_.reducedRemoteState.htlcs) getOrElse Set.empty
  def estimateCanReceiveCapped(chan: Channel) = math.min(estimateCanReceive(chan), LNParams.maxHtlcValueMsat)
  def estimateCanReceive(chan: Channel) = chan(_.reducedRemoteState.canReceiveMsat) getOrElse 0L
  def estimateCanSend(chan: Channel) = chan(_.reducedRemoteState.canSendMsat) getOrElse 0L

  def isOpening(chan: Channel): Boolean = chan.data match {
    case remote: WaitBroadcastRemoteData => remote.fail.isEmpty
    case _: WaitFundingDoneData => true
    case _ => false
  }

  def isOperational(chan: Channel) = chan.data match {
    case NormalData(_, _, None, None) => true
    case _ => false
  }

  def channelAndHop(chan: Channel) = for {
    Some(maybeDummyExtraHop) <- chan(_.extraHop)
    if maybeDummyExtraHop.htlcMinimumMsat > 0L
  } yield chan -> Vector(maybeDummyExtraHop)
}

case class ChanReport(chan: Channel, cs: Commitments) {
  def estimateFinalCanSend: Long = estimateCanSend(chan) - softReserve.amount * 1000L
  val softReserve = if (cs.localParams.isFunder) cs.commitInput.txOut.amount / 50L else Satoshi(0L)
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

  def fulfillReceived(ok: UpdateFulfillHtlc): Unit = none
  def outPaymentAccepted(rd: RoutingData): Unit = none
  def sentSig(cs: Commitments): Unit = none
  def settled(cs: Commitments): Unit = none
}