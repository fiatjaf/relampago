package com.lightning.wallet.ln

import octopus.dsl._
import octopus.syntax._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import java.util.concurrent.Executors
import fr.acinq.eclair.UInt64

import com.lightning.wallet.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex, Sphinx}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import com.lightning.wallet.ln.Helpers.{Closing, Funding}
import com.lightning.wallet.ln.Tools.{none, runAnd}
import fr.acinq.bitcoin.Crypto.{Point, Scalar}
import fr.acinq.bitcoin.{Satoshi, Transaction}


abstract class Channel extends StateMachine[ChannelData] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def apply[T](ex: Commitments => T) = Some(data) collect { case some: HasCommitments => ex apply some.commitments }
  def process(change: Any) = Future(me doProcess change) onFailure { case err => events onError me -> err }
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
      case (InitData(announce), cmd: CMDOpenChannel, WAIT_FOR_INIT) =>
        BECOME(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT) SEND OpenChannel(LNParams.chainHash, cmd.tempChanId,
          cmd.realFundingAmountSat, cmd.pushMsat, LNParams.dustLimit.amount, cmd.localParams.maxHtlcValueInFlightMsat,
          cmd.localParams.channelReserveSat, LNParams.minHtlcValue.amount, cmd.initialFeeratePerKw, cmd.localParams.toSelfDelay,
          cmd.localParams.maxAcceptedHtlcs, cmd.localParams.fundingPrivKey.publicKey, cmd.localParams.revocationBasepoint,
          cmd.localParams.paymentBasepoint, cmd.localParams.delayedPaymentBasepoint, cmd.localParams.htlcBasepoint,
          Generators.perCommitPoint(cmd.localParams.shaSeed, index = 0L), channelFlags = 0.toByte)


      case (wait @ WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT)
        if accept.temporaryChannelId == cmd.tempChanId =>

        val acceptChannelValidator = Validator[AcceptChannel]
          .rule(_.minimumDepth <= 6L, "Their minimumDepth is too high")
          .rule(_.htlcMinimumMsat <= 10000L, "Their htlcMinimumMsat too high")
          .rule(_.toSelfDelay <= cmd.localParams.toSelfDelay * 20, "Their toSelfDelay is too high")
          .rule(_.maxHtlcValueInFlightMsat > UInt64(10000L), "Their maxHtlcValueInFlightMsat is too low")
          .rule(_.channelReserveSatoshis < cmd.realFundingAmountSat / 10, "Their proposed reserve is too high")
          //.rule(_.maxAcceptedHtlcs < 483, "They can accept too many incoming HTLCs")
          .rule(_.maxAcceptedHtlcs > 0, "They can accept too few incoming HTLCs")
          .rule(_.dustLimitSatoshis >= 546L, "Their dust limit is too low")

        val result = accept.validate(acceptChannelValidator)
        if (result.isValid) BECOME(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING)
        else throw new LightningException(result.toFieldErrMapping.map(_._2) mkString "\n")


      case (WaitFundingData(announce, cmd, accept), CMDFunding(fundTx), WAIT_FOR_FUNDING) =>
        // They have accepted our proposal, let them sign a first commit so we can broadcast a funding
        if (fundTx.txOut(cmd.outIndex).amount.amount != cmd.realFundingAmountSat) throw new LightningException
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Funding.makeFirstFunderCommitTxs(cmd, accept,
          fundTx.hash, fundingTxOutputIndex = cmd.outIndex, remoteFirstPoint = accept.firstPerCommitmentPoint)

        val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, cmd.localParams.fundingPrivKey)
        val fundingCreated = FundingCreated(cmd.tempChanId, fundTx.hash, cmd.outIndex, localSigOfRemoteTx)
        val firstRemoteCommit = RemoteCommit(0L, remoteSpec, remoteCommitTx.tx.txid, accept.firstPerCommitmentPoint)
        BECOME(WaitFundingSignedData(announce, cmd.localParams, Tools.toLongId(fundTx.hash, cmd.outIndex), accept, fundTx,
          localSpec, localCommitTx, firstRemoteCommit), WAIT_FUNDING_SIGNED) SEND fundingCreated

      // They have signed our first commit, we can broadcast a funding tx
      case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED) =>
        val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, wait.localParams.fundingPrivKey.publicKey,
          wait.remoteParams.fundingPubkey, Scripts.sign(wait.localCommitTx, wait.localParams.fundingPrivKey), remote.signature)

        if (Scripts.checkSpendable(signedLocalCommitTx).isEmpty) BECOME(wait, CLOSING) else {
          val localCommit = LocalCommit(0L, wait.localSpec, htlcTxsAndSigs = Nil, signedLocalCommitTx)
          val commits = Commitments(wait.localParams, wait.remoteParams, localCommit, wait.remoteCommit,
            localChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty),
            remoteChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty),
            localNextHtlcId = 0L, remoteNextHtlcId = 0L, Right(Tools.randomPrivKey.toPoint),
            wait.localCommitTx.input, ShaHashesWithIndex(Map.empty, None), wait.channelId)

          BECOME(WaitFundingDoneData(wait.announce, None, None,
            wait.fundingTx, commits), WAIT_FUNDING_DONE)
        }


      // FUNDING TX IS BROADCASTED AT THIS POINT


      case (wait: WaitFundingDoneData, their: FundingLocked, WAIT_FUNDING_DONE) =>
        // No need to store their FundingLocked because it gets re-sent on reconnect
        if (wait.our.isEmpty) me UPDATA wait.copy(their = Some apply their)
        else becomeOpen(wait, their)


      case (wait: WaitFundingDoneData, CMDConfirmed(fundTx), WAIT_FUNDING_DONE)
        // GUARD: this funding transaction blongs to this exact channel
        if wait.fundingTx.txid == fundTx.txid =>

        // Create and store our FundingLocked
        // since CMDConfirmed only happens once
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


      case (norm: NormalData, fail: UpdateFailHtlc, OPEN) =>
        // Got a failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFail(norm.commitments, fail)
        me UPDATA norm.copy(commitments = c1)


      case (norm: NormalData, fail: UpdateFailMalformedHtlc, OPEN) =>
        // Got 'malformed' failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFailMalformed(norm.commitments, fail)
        me UPDATA norm.copy(commitments = c1)


      // We can only send a new HTLC when channel is both operational and online
      case (norm: NormalData, rpi: RuntimePaymentInfo, OPEN) if isOperational(me) =>
        val c1 \ updateAddHtlc = Commitments.sendAdd(norm.commitments, rpi)
        me UPDATA norm.copy(commitments = c1) SEND updateAddHtlc
        doProcess(CMDProceed)


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
        val minExpiry = LNParams.broadcaster.currentHeight
        for (Htlc(false, add) <- norm.commitments.remoteCommit.spec.htlcs) {
          if (add.expiry < minExpiry) throw new LightningException("Payment expiry in past")
          me doProcess resolveHtlc(LNParams.nodePrivateKey, add, LNParams.bag, minExpiry + 6L)
        }

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


      case (norm: NormalData, CMDBestHeight(height), OPEN | OFFLINE)
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


      case (norm @ NormalData(_, _, None, their), CMDProceed, OPEN)
        if inFlightOutgoingHtlcs(me).isEmpty && their.isDefined =>
        // GUARD: got their shutdown with no in-flight HTLCs
        me startShutdown norm
        doProcess(CMDProceed)


      case (NormalData(announce, commitments, our, their), CMDProceed, OPEN)
        // GUARD: got both shutdowns without in-flight HTLCs so we can start negs
        if inFlightOutgoingHtlcs(me).isEmpty && our.isDefined && their.isDefined =>

        val firstProposed = Closing.makeFirstClosing(commitments, our.get.scriptPubKey, their.get.scriptPubKey)
        val neg = NegotiationsData(announce, commitments, our.get, their.get, firstProposed :: Nil)
        BECOME(me STORE neg, NEGOTIATIONS) SEND firstProposed.localClosingSigned


      // SYNC and REFUNDING MODE


      case (ref: RefundingData, cr: ChannelReestablish, REFUNDING)
        // GUARD: We have explicitly started with a fresh REFUNDING data
        // we need to make sure their commitment point can be saved only once
        if cr.myCurrentPerCommitmentPoint.isDefined && ref.remoteLatestPoint.isEmpty =>
        storeRefund(ref, cr.myCurrentPerCommitmentPoint.get)


      case (norm: NormalData, cr: ChannelReestablish, OFFLINE)
        // GUARD: we have started in NORMAL state but their nextRemoteRevocationNumber is too far away
        if norm.commitments.localCommit.index < cr.nextRemoteRevocationNumber && cr.myCurrentPerCommitmentPoint.isDefined =>
        val secret = Generators.perCommitSecret(norm.commitments.localParams.shaSeed, cr.nextRemoteRevocationNumber - 1)
        if (cr.yourLastPerCommitmentSecret contains secret) storeRefund(norm, cr.myCurrentPerCommitmentPoint.get)
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


      // We may get this message any time so just save it here
      case (wait: WaitFundingDoneData, CMDConfirmed(tx), OFFLINE)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        val wait1 = wait.modify(_.our) setTo Some(our)
        me UPDATA STORE(wait1)


      // We're exiting a sync state while waiting for their FundingLocked
      case (wait: WaitFundingDoneData, cr: ChannelReestablish, OFFLINE) =>
        BECOME(wait, WAIT_FUNDING_DONE)
        wait.our foreach SEND


      // No in-flight HTLCs here, just proceed with negotiations
      case (neg: NegotiationsData, cr: ChannelReestablish, OFFLINE) =>
        // According to spec we need to re-send a last closing sig here
        val lastSigned = neg.localProposals.head.localClosingSigned
        List(neg.localShutdown, lastSigned) foreach SEND
        BECOME(neg, NEGOTIATIONS) SEND lastSigned


      // SYNC: ONLINE/OFFLINE


      case (some: HasCommitments, CMDOnline, OFFLINE) =>
        val ShaHashesWithIndex(hashes, lastIndex) = some.commitments.remotePerCommitmentSecrets
        val yourLastPerCommitmentSecret = lastIndex.map(ShaChain.moves).flatMap(ShaChain getHash hashes).getOrElse(Sphinx zeroes 32)
        val myCurrentPerCommitmentPoint = Generators.perCommitPoint(some.commitments.localParams.shaSeed, some.commitments.localCommit.index)
        me SEND ChannelReestablish(some.commitments.channelId, some.commitments.localCommit.index + 1, some.commitments.remoteCommit.index,
          Some apply Scalar(yourLastPerCommitmentSecret), Some apply myCurrentPerCommitmentPoint)


      case (wait: WaitFundingDoneData, CMDOffline, WAIT_FUNDING_DONE) => BECOME(wait, OFFLINE)
      case (negs: NegotiationsData, CMDOffline, NEGOTIATIONS) => BECOME(negs, OFFLINE)
      case (norm: NormalData, CMDOffline, OPEN) => BECOME(norm, OFFLINE)


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

      case (RefundingData(announce, Some(remoteLatestPoint), commitments), CMDSpent(spendTx), REFUNDING)
        // GUARD: we have got a remote commit which we asked them to spend and we have their point
        if spendTx.txIn.exists(_.outPoint == commitments.commitInput.outPoint) =>

        // Claim our main output using their point and go to CLOSING
        val rcp = Closing.claimRemoteMainOutput(commitments, remoteLatestPoint, spendTx)
        val d1 = ClosingData(announce, commitments, refundRemoteCommit = rcp :: Nil)
        BECOME(me STORE d1, CLOSING)


      case (some: HasCommitments, CMDSpent(tx), _)
        // GUARD: tx which spends our funding is broadcasted, must react
        if tx.txIn.exists(_.outPoint == some.commitments.commitInput.outPoint) =>
        val revokedCommitOpt = Closing.claimRevokedRemoteCommitTxOutputs(some.commitments, tx)
        val nextRemoteCommitEither = some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit)

        Tuple3(revokedCommitOpt, nextRemoteCommitEither, some) match {
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


      case (null, ref: RefundingData, null) => BECOME(ref, REFUNDING)
      case (null, close: ClosingData, null) => BECOME(close, CLOSING)
      case (null, init: InitData, null) => BECOME(init, WAIT_FOR_INIT)
      case (null, wait: WaitFundingDoneData, null) => BECOME(wait, OFFLINE)
      case (null, neg: NegotiationsData, null) => BECOME(neg, OFFLINE)
      case (null, norm: NormalData, null) => BECOME(norm, OFFLINE)


      // MISC


      case (_, err: Error, WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FOR_FUNDING | WAIT_FUNDING_SIGNED) =>
        // May happen if remote peer does not like any of our proposed parameters, nothing to lose here
        throw new LightningException(err.humanText)


      case (some: HasCommitments, err: Error, WAIT_FUNDING_DONE | NEGOTIATIONS | OPEN | OFFLINE) =>
        // REFUNDING is an exception here, we CAN NOT start a local close in that state
        startLocalClose(some)


      case (some: HasCommitments, CMDShutdown, NEGOTIATIONS | OFFLINE) =>
        // CMDShutdown in WAIT_FUNDING_DONE and OPEN can be cooperative
        startLocalClose(some)

      case other =>
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

  private def storeRefund(some: HasCommitments, point: Point) = {
    val msg = "please publish your local commitment" getBytes "UTF-8"
    val ref = RefundingData(some.announce, Some(point), some.commitments)
    BECOME(me STORE ref, REFUNDING) SEND Error(ref.commitments.channelId, msg)
  }

  private def makeFundingLocked(cs: Commitments) = {
    val first = Generators.perCommitPoint(cs.localParams.shaSeed, 1L)
    FundingLocked(cs.channelId, nextPerCommitmentPoint = first)
  }

  private def becomeOpen(wait: WaitFundingDoneData, their: FundingLocked) = {
    val c1 = wait.commitments.copy(remoteNextCommitInfo = Right apply their.nextPerCommitmentPoint)
    BECOME(me STORE NormalData(wait.announce, c1), OPEN)
  }

  private def startShutdown(norm: NormalData) = {
    val finalScriptPubKey = norm.commitments.localParams.defaultFinalScriptPubKey
    val localShutdown = Shutdown(norm.commitments.channelId, finalScriptPubKey)
    val norm1 = norm.modify(_.localShutdown) setTo Some(localShutdown)
    BECOME(norm1, OPEN) SEND localShutdown
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
  val OFFLINE = "OFFLINE"
  val OPEN = "OPEN"

  // No tears, only dreams now
  val REFUNDING = "REFUNDING"
  val CLOSING = "CLOSING"

  def myBalanceMsat(chan: Channel) = chan(commitments => commitments.localCommit.spec.toLocalMsat) getOrElse 0L
  def isOperational(chan: Channel) = chan.data match { case NormalData(_, _, None, None) => true case _ => false }
  def isOpening(chan: Channel) = chan.data match { case _: WaitFundingDoneData => true case _ => false }

  def inFlightOutgoingHtlcs(chan: Channel) = chan.data match {
    // Channels should always be filtered by some criteria before calling this method
    // like find all current in-flight HTLC from alive chans or all frozen from closing chans
    case some: HasCommitments => Commitments.latestRemoteCommit(some.commitments).spec.htlcs
    case _ => Set.empty[Htlc]
  }

  def estimateCanReceive(chan: Channel) = chan { cs =>
    // Somewhat counterintuitive: localParams.channelReserveSat is THEIR unspendable reseve
    // peer's balance can't go below their channel reserve, commit tx fee is always paid by us
    val canReceive = cs.localCommit.spec.toRemoteMsat - cs.localParams.channelReserveSat * 1000L
    math.min(canReceive, LNParams.maxHtlcValue.amount)
  } getOrElse 0L

  def estimateTotalCanSend(chan: Channel) = chan { cs =>
    val currentCommitFee = cs.localCommit.commitTx -- cs.localCommit.commitTx
    val currentTotalFee = cs.remoteParams.channelReserveSatoshis + currentCommitFee.amount
    // Somewhat counterintuitive: remoteParams.channelReserveSatoshis is OUR unspendable reseve
    // Sending limit consists of unspendable channel reserve + current commit tx fee
    cs.localCommit.spec.toLocalMsat - currentTotalFee * 1000L
  } getOrElse 0L

  def estimateCanSend(chan: Channel) = {
    val unconstrainedCanSend = estimateTotalCanSend(chan)
    math.min(unconstrainedCanSend, LNParams.maxHtlcValue.amount)
  }

  def getHop(chan: Channel) = for {
    Some(extraHop) <- chan(_.extraHop)
    // Ensure the hop is the final one
    if extraHop.htlcMinimumMsat > 0L
  } yield chan -> Vector(extraHop)
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