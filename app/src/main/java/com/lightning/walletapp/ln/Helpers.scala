package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Scripts._
import com.lightning.walletapp.ln.crypto.ShaChain._
import com.lightning.walletapp.ln.crypto.Generators._
import fr.acinq.bitcoin.Crypto.{Point, PublicKey, Scalar}
import scala.util.{Success, Try}
import scodec.bits.ByteVector


object Helpers {
  def makeLocalTxs(commitTxNumber: Long, localParams: LocalParams,
                   remoteParams: AcceptChannel, commitmentInput: InputInfo,
                   localPerCommitmentPoint: Point, spec: CommitmentSpec) = {

    val localHtlcPubkey = derivePubKey(localParams.htlcBasepoint, localPerCommitmentPoint)
    val localDelayedPaymentPubkey = derivePubKey(localParams.delayedPaymentBasepoint, localPerCommitmentPoint)
    val localRevocationPubkey = revocationPubKey(remoteParams.revocationBasepoint, localPerCommitmentPoint)
    val remotePaymentPubkey = derivePubKey(remoteParams.paymentBasepoint, localPerCommitmentPoint)
    val remoteHtlcPubkey = derivePubKey(remoteParams.htlcBasepoint, localPerCommitmentPoint)

    val commitTx =
      Scripts.makeCommitTx(commitmentInput, commitTxNumber, localParams.paymentBasepoint, remoteParams.paymentBasepoint,
        localParams.isFunder, localParams.dustLimit, localRevocationPubkey, remoteParams.toSelfDelay, localDelayedPaymentPubkey,
        remotePaymentPubkey, localHtlcPubkey, remoteHtlcPubkey, spec)

    val htlcTimeoutTxs \ htlcSuccessTxs =
      Scripts.makeHtlcTxs(commitTx.tx, localParams.dustLimit, localRevocationPubkey,
        remoteParams.toSelfDelay, localDelayedPaymentPubkey, localHtlcPubkey, remoteHtlcPubkey, spec)

    (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
  }

  def makeRemoteTxs(commitTxNumber: Long, localParams: LocalParams,
                    remoteParams: AcceptChannel, commitmentInput: InputInfo,
                    remotePerCommitmentPoint: Point, spec: CommitmentSpec) = {

    val localHtlcPubkey = derivePubKey(localParams.htlcBasepoint, remotePerCommitmentPoint)
    val localPaymentPubkey = derivePubKey(localParams.paymentBasepoint, remotePerCommitmentPoint)
    val remoteRevocationPubkey = revocationPubKey(localParams.revocationBasepoint, remotePerCommitmentPoint)
    val remoteDelayedPaymentPubkey = derivePubKey(remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
    val remoteHtlcPubkey = derivePubKey(remoteParams.htlcBasepoint, remotePerCommitmentPoint)

    val commitTx =
      Scripts.makeCommitTx(commitmentInput, commitTxNumber, remoteParams.paymentBasepoint, localParams.paymentBasepoint,
        !localParams.isFunder, remoteParams.dustLimitSat, remoteRevocationPubkey, localParams.toSelfDelay, remoteDelayedPaymentPubkey,
        localPaymentPubkey, remoteHtlcPubkey, localHtlcPubkey, spec)

    val htlcTimeoutTxs \ htlcSuccessTxs =
      Scripts.makeHtlcTxs(commitTx.tx, remoteParams.dustLimitSat, remoteRevocationPubkey,
        localParams.toSelfDelay, remoteDelayedPaymentPubkey, remoteHtlcPubkey, localHtlcPubkey, spec)

    (commitTx, htlcTimeoutTxs, htlcSuccessTxs,
      remoteHtlcPubkey, remoteRevocationPubkey)
  }

  def isValidFinalScriptPubkey(raw: ByteVector) = Try(Script parse raw) match {
    case Success(OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pkh, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil) => pkh.size == 20
    case Success(OP_HASH160 :: OP_PUSHDATA(scriptHash, _) :: OP_EQUAL :: Nil) => scriptHash.size == 20
    case Success(OP_0 :: OP_PUSHDATA(pubkeyHash, _) :: Nil) if pubkeyHash.length == 20 => true
    case Success(OP_0 :: OP_PUSHDATA(scriptHash, _) :: Nil) if scriptHash.length == 32 => true
    case _ => false
  }

  object Closing {
    type SuccessAndClaim = (HtlcSuccessTx, ClaimDelayedOutputTx)
    type TimeoutAndClaim = (HtlcTimeoutTx, ClaimDelayedOutputTx)

    def makeFirstClosing(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector) = {
      val closingTx = Scripts.addSigs(makeClosingTx(commitments.commitInput, localScriptPubkey, remoteScriptPubkey, Satoshi(0),
        Satoshi(0), commitments.localSpec, commitments.localParams.isFunder), commitments.localParams.fundingPrivKey.publicKey,
        commitments.remoteParams.fundingPubkey, ByteVector fromValidHex "aa" * 71, ByteVector fromValidHex "bb" * 71)

      // There is no need for a high fee in a mutual closing tx AND mutual fee can't be bigger than last commit tx fee
      val computedClosingFee = Scripts.weight2fee(LNParams.broadcaster.perKwSixSat / 2, Transaction weight closingTx.tx)
      val lastCommitFee = commitments.commitInput.txOut.amount - commitments.localCommit.commitTx.tx.allOutputsAmount
      val closingFee = if (computedClosingFee > lastCommitFee) lastCommitFee else computedClosingFee
      makeClosing(commitments, closingFee, localScriptPubkey, remoteScriptPubkey)
    }

    def makeClosing(commitments: NormalCommits, closingFee: Satoshi, local: ByteVector, remote: ByteVector) = {
      val theirDustIsHigherThanOurs: Boolean = commitments.localParams.dustLimit < commitments.remoteParams.dustLimitSat
      val dustLimit = if (theirDustIsHigherThanOurs) commitments.remoteParams.dustLimitSat else commitments.localParams.dustLimit

      val closing: ClosingTx =
        makeClosingTx(commitments.commitInput, local, remote, dustLimit,
          closingFee, commitments.localSpec, commitments.localParams.isFunder)

      val localClosingSig = Scripts.sign(commitments.localParams.fundingPrivKey)(closing)
      val closingSigned = ClosingSigned(commitments.channelId, closingFee.amount, localClosingSig)
      require(isValidFinalScriptPubkey(remote), "Invalid remoteScriptPubkey")
      require(isValidFinalScriptPubkey(local), "Invalid localScriptPubkey")
      ClosingTxProposed(closing, closingSigned)
    }

    def makeClosingTx(commitTxInput: InputInfo, localScriptPubKey: ByteVector, remoteScriptPubKey: ByteVector,
                      dustLimit: Satoshi, closingFee: Satoshi, spec: CommitmentSpec, localIsFunder: Boolean) = {

      require(spec.htlcs.isEmpty, "No pending HTLCs allowed")
      val toRemoteAmount: Satoshi = if (localIsFunder) MilliSatoshi(spec.toRemoteMsat) else MilliSatoshi(spec.toRemoteMsat) - closingFee
      val toLocalAmount: Satoshi = if (localIsFunder) MilliSatoshi(spec.toLocalMsat) - closingFee else MilliSatoshi(spec.toLocalMsat)
      val toRemoteOutput = if (toRemoteAmount < dustLimit) Nil else TxOut(toRemoteAmount, remoteScriptPubKey) :: Nil
      val toLocalOutput = if (toLocalAmount < dustLimit) Nil else TxOut(toLocalAmount, localScriptPubKey) :: Nil
      val input = TxIn(commitTxInput.outPoint, ByteVector.empty, sequence = 0xffffffffL) :: Nil
      val tx = Transaction(2, input, toLocalOutput ++ toRemoteOutput, lockTime = 0)
      ClosingTx(commitTxInput, LexicographicalOrdering sort tx)
    }

    def claimCurrentLocalCommitTxOutputs(commitments: NormalCommits, bag: PaymentInfoBag) = {
      val localPerCommitmentPoint = perCommitPoint(commitments.localParams.shaSeed, commitments.localCommit.index.toInt)
      val localRevocationPubkey = revocationPubKey(commitments.remoteParams.revocationBasepoint, localPerCommitmentPoint)
      val localDelayedPrivkey = derivePrivKey(commitments.localParams.delayedPaymentKey, localPerCommitmentPoint)
      val feeRate = LNParams.broadcaster.perKwSixSat

      def makeClaimDelayedOutput(tx: Transaction) = for {
        claimDelayed <- Scripts.makeClaimDelayedOutputTx(tx, localRevocationPubkey,
          commitments.remoteParams.toSelfDelay, localDelayedPrivkey.publicKey,
          commitments.localParams.defaultFinalScriptPubKey, feeRate,
          commitments.localParams.dustLimit)

        sig = Scripts.sign(localDelayedPrivkey)(claimDelayed)
        signed <- Scripts checkValid Scripts.addSigs(claimDelayed, sig)
      } yield signed

      val allSuccessTxs = for {
        HtlcTxAndSigs(info: HtlcSuccessTx, local, remote) <- commitments.localCommit.htlcTxsAndSigs
        paymentInfo <- bag.getPaymentInfo(hash = info.add.paymentHash).toOption
        success = Scripts.addSigs(info, local, remote, paymentInfo.preimage)
        delayed <- makeClaimDelayedOutput(success.tx).toOption
      } yield success -> delayed

      val allTimeoutTxs = for {
        HtlcTxAndSigs(info: HtlcTimeoutTx, local, remote) <- commitments.localCommit.htlcTxsAndSigs
        timeout = Scripts.addSigs(htlcTimeoutTx = info, localSig = local, remoteSig = remote)
        delayed <- makeClaimDelayedOutput(timeout.tx).toOption
      } yield timeout -> delayed

      // When local commit is spent our main output is also delayed
      val claimMainDelayedTx = makeClaimDelayedOutput(commitments.localCommit.commitTx.tx).toOption.toSeq
      LocalCommitPublished(claimMainDelayedTx, allSuccessTxs, allTimeoutTxs, commitments.localCommit.commitTx.tx)
    }

    // remoteCommit may refer to their current or next RemoteCommit, hence it is a separate parameter
    def claimRemoteCommitTxOutputs(commitments: NormalCommits, remoteCommit: RemoteCommit, bag: PaymentInfoBag) = {
      val localHtlcPrivkey = derivePrivKey(commitments.localParams.htlcKey, remoteCommit.remotePerCommitmentPoint)
      val localHtlcPubkey = localHtlcPrivkey.publicKey

      val (remoteCommitTx, timeout, success, remoteHtlcPubkey, remoteRevocationPubkey) =
        makeRemoteTxs(remoteCommit.index, commitments.localParams, commitments.remoteParams,
          commitments.commitInput, remoteCommit.remotePerCommitmentPoint, remoteCommit.spec)

      val finder = new PubKeyScriptIndexFinder(remoteCommitTx.tx)
      val feeRate = LNParams.broadcaster.perKwThreeSat

      val claimSuccessTxs = for {
        HtlcTimeoutTx(_, _, add) <- timeout
        info <- bag.getPaymentInfo(hash = add.paymentHash).toOption
        claimHtlcSuccessTx <- Scripts.makeClaimHtlcSuccessTx(finder, localHtlcPubkey, remoteHtlcPubkey,
          remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey, add, feeRate,
          commitments.localParams.dustLimit).toOption

        sig = Scripts.sign(localHtlcPrivkey)(claimHtlcSuccessTx)
        signed = Scripts.addSigs(claimHtlcSuccessTx, sig, info.preimage)
        success <- Scripts.checkValid(signed).toOption
      } yield success

      val claimTimeoutTxs = for {
        HtlcSuccessTx(_, _, add) <- success
        claimHtlcTimeoutTx <- Scripts.makeClaimHtlcTimeoutTx(finder, localHtlcPubkey, remoteHtlcPubkey,
          remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey, add, feeRate,
          commitments.localParams.dustLimit).toOption

        sig = Scripts.sign(localHtlcPrivkey)(claimHtlcTimeoutTx)
        signed = Scripts.addSigs(claimHtlcTimeoutTx, sig)
        timeout <- Scripts.checkValid(signed).toOption
      } yield timeout

      val main = claimRemoteMainOutput(commitments, remoteCommit.remotePerCommitmentPoint, remoteCommitTx.tx)
      main.copy(claimHtlcSuccess = claimSuccessTxs, claimHtlcTimeout = claimTimeoutTxs)
    }

    def claimRemoteMainOutput(commitments: NormalCommits, remotePerCommitmentPoint: Point, commitTx: Transaction) = {
      val localPaymentPrivateKey = derivePrivKey(secret = commitments.localParams.paymentKey, remotePerCommitmentPoint)
      val ri = RevocationInfo(Nil, None, None, LNParams.broadcaster.perKwSixSat, commitments.localParams.dustLimit.amount,
        commitments.localParams.defaultFinalScriptPubKey, 0, localPaymentPrivateKey.publicKey, null, null)

      val claimMain = for {
        claimP2WPKH <- ri.makeClaimP2WPKHOutput(commitTx)
        sig = Scripts.sign(localPaymentPrivateKey)(claimP2WPKH)
        signed = Scripts.addSigs(claimP2WPKH, sig, ri.localPubKey)
        main <- Scripts.checkValid(signed)
      } yield main

      // We only claim a main output here in case when it's a refunding
      RemoteCommitPublished(claimMain.toOption.toSeq, Nil, Nil, commitTx)
    }

    // CONTRACT BREACH HANDLING

    def makeRevocationInfo(commitments: NormalCommits, tx: Transaction,
                           perCommitSecret: Scalar, feeRate: Long) = {

      val remotePerCommitmentPoint = perCommitSecret.toPoint
      val localPrivkey = derivePrivKey(commitments.localParams.paymentKey, remotePerCommitmentPoint)
      val remoteRevocationPrivkey = revocationPrivKey(commitments.localParams.revocationSecret, perCommitSecret)
      val remoteDelayedPaymentKey = derivePubKey(commitments.remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)

      val ri = RevocationInfo(redeemScriptsToSigs = Nil, claimMainTxSig = None, claimPenaltyTxSig = None,
        feeRate, commitments.localParams.dustLimit.amount, commitments.localParams.defaultFinalScriptPubKey,
        commitments.localParams.toSelfDelay, localPrivkey.publicKey, remoteRevocationPrivkey.publicKey,
        remoteDelayedPaymentKey)

      val finder = new PubKeyScriptIndexFinder(tx)
      val htlcs = commitments.remoteCommit.spec.htlcs
      val localHtlcKey = derivePubKey(commitments.localParams.htlcBasepoint, remotePerCommitmentPoint)
      val remoteHtlcKey = derivePubKey(commitments.remoteParams.htlcBasepoint, remotePerCommitmentPoint)
      val remoteRevKey = revocationPubKey(commitments.localParams.revocationBasepoint, remotePerCommitmentPoint)
      val offered = for (Htlc(false, add) <- htlcs) yield Scripts.htlcOffered(remoteHtlcKey, localHtlcKey, remoteRevKey, add.hash160)
      val received = for (Htlc(true, add) <- htlcs) yield Scripts.htlcReceived(remoteHtlcKey, localHtlcKey, remoteRevKey, add.hash160, add.expiry)
      val redeemScripts = for (redeem <- offered ++ received) yield Tuple2(Script.write(Script pay2wsh redeem), Script write redeem)
      val redeemScriptsMap = redeemScripts.toMap

      val htlcPenaltySigs = for {
        TxOut(_, publicKeyScript) <- tx.txOut
        // Small HTLCs won't make it into tx outputs
        redeemScript <- redeemScriptsMap get publicKeyScript
        penaltyTx <- ri.makeHtlcPenalty(finder)(redeemScript).toOption
        htlcSig = Scripts.sign(remoteRevocationPrivkey)(penaltyTx)
      } yield (redeemScript, htlcSig)

      ri.copy(redeemScriptsToSigs = htlcPenaltySigs.toList,
        claimPenaltyTxSig = ri.makeMainPenalty(tx).map(Scripts sign remoteRevocationPrivkey).toOption,
        claimMainTxSig = ri.makeClaimP2WPKHOutput(tx).map(Scripts sign localPrivkey).toOption)
    }

    // Here we have an existing RevocationInfo with updated fee rate
    def reMakeRevocationInfo(ri: RevocationInfo, commitments: NormalCommits,
                             tx: Transaction, perCommitSecret: Scalar) = {

      val finder = new PubKeyScriptIndexFinder(tx)
      val remotePerCommitmentPoint = perCommitSecret.toPoint
      val localPrivkey = derivePrivKey(commitments.localParams.paymentKey, remotePerCommitmentPoint)
      val remoteRevocationPrivkey = revocationPrivKey(commitments.localParams.revocationSecret, perCommitSecret)

      val htlcPenaltySigs = for {
        redeemScript \ _ <- ri.redeemScriptsToSigs
        // Small HTLCs won't make it into tx outputs
        penaltyTx <- ri.makeHtlcPenalty(finder)(redeemScript).toOption
        htlcSig = Scripts.sign(remoteRevocationPrivkey)(penaltyTx)
      } yield (redeemScript, htlcSig)

      ri.copy(redeemScriptsToSigs = htlcPenaltySigs,
        claimPenaltyTxSig = ri.makeMainPenalty(tx).map(Scripts sign remoteRevocationPrivkey).toOption,
        claimMainTxSig = ri.makeClaimP2WPKHOutput(tx).map(Scripts sign localPrivkey).toOption)
    }

    def claimRevokedRemoteCommitTxOutputs(ri: RevocationInfo, tx: Transaction) = {
      // We only save scripts and signatures in RevocationInfo to save storage space
      // once required we regenerate full transactions here on demand
      val finder = new PubKeyScriptIndexFinder(tx)

      val claimMainTx = for {
        sig <- ri.claimMainTxSig
        makeClaimP2WPKH <- ri.makeClaimP2WPKHOutput(tx).toOption
        signed = Scripts.addSigs(makeClaimP2WPKH, sig, ri.localPubKey)
        main <- Scripts.checkValid(signed).toOption
      } yield main

      val claimPenaltyTx = for {
        sig <- ri.claimPenaltyTxSig
        theirMainPenalty <- ri.makeMainPenalty(tx).toOption
        signed = Scripts.addSigs(theirMainPenalty, sig)
        their <- Scripts.checkValid(signed).toOption
      } yield their

      val htlcPenaltyTxs = for {
        redeemScript \ sig <- ri.redeemScriptsToSigs
        htlcPenaltyTx <- ri.makeHtlcPenalty(finder)(redeemScript).toOption
        signed = Scripts.addSigs(htlcPenaltyTx, sig, ri.remoteRevocationPubkey)
        penalty <- Scripts.checkValid(signed).toOption
      } yield penalty

      RevokedCommitPublished(claimMainTx.toSeq,
        claimPenaltyTx.toSeq, htlcPenaltyTxs, tx)
    }

    def extractCommitmentSecret(commitments: NormalCommits, commitTx: Transaction) = {
      val txNumber = Scripts.obscuredCommitTxNumber(Scripts.decodeTxNumber(commitTx.txIn.head.sequence, commitTx.lockTime),
        !commitments.localParams.isFunder, commitments.remoteParams.paymentBasepoint, commitments.localParams.paymentBasepoint)

      val index = moves(largestTxIndex - txNumber)
      val hashes = commitments.remotePerCommitmentSecrets.hashes
      getHash(hashes)(index).map(ByteVector.view).map(Scalar.apply)
    }

    def claimRevokedHtlcTxOutputs(commitments: NormalCommits, htlcTx: Transaction,
                                  perCommitSecret: Scalar): Option[Transaction] = {

      val remotePerCommitmentPoint = perCommitSecret.toPoint
      val remoteRevocationPrivkey = revocationPrivKey(commitments.localParams.revocationSecret, perCommitSecret)
      val remoteDelayedPaymentPubkey = derivePubKey(commitments.remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)

      for {
        penaltyHtlcTx <- Scripts.makeClaimDelayedOutputPenaltyTx(delayedOutputTx = htlcTx, remoteRevocationPrivkey.publicKey,
          commitments.localParams.toSelfDelay, remoteDelayedPaymentPubkey, commitments.localParams.defaultFinalScriptPubKey,
          LNParams.broadcaster.perKwThreeSat, commitments.localParams.dustLimit).toOption

        sig = Scripts.sign(remoteRevocationPrivkey)(penaltyHtlcTx)
        signed = Scripts.addSigs(penaltyHtlcTx, revocationSig = sig)
        penalty <- Scripts.checkValid(signed).toOption
      } yield penalty.tx
    }
  }

  object Funding {
    def makeFundingInputInfo(fundingTxid: ByteVector, fundingTxOutputIndex: Int,
                             fundingSatoshis: Satoshi, fundingPubkey1: PublicKey,
                             fundingPubkey2: PublicKey): InputInfo = {

      val multisig = Scripts.multiSig2of2(fundingPubkey1, fundingPubkey2)
      val fundingTxOut = TxOut(fundingSatoshis, Script pay2wsh multisig)
      val outPoint = OutPoint(fundingTxid, fundingTxOutputIndex)
      InputInfo(outPoint, fundingTxOut, Script write multisig)
    }

    def makeFirstCommitTxs(localParams: LocalParams, fundingSat: Long, pushMsat: Long, initialFeeratePerKw: Long,
                           remoteParams: AcceptChannel, fundingTxid: ByteVector, fundingTxOutputIndex: Int,
                           remoteFirstPoint: Point) = {

      val toLocalMsat = if (localParams.isFunder) fundingSat * 1000L - pushMsat else pushMsat
      val toRemoteMsat = if (localParams.isFunder) pushMsat else fundingSat * 1000L - pushMsat

      val localSpec = CommitmentSpec(initialFeeratePerKw, toLocalMsat, toRemoteMsat)
      val remoteSpec = CommitmentSpec(initialFeeratePerKw, toRemoteMsat, toLocalMsat)

      if (!localParams.isFunder) {
        val fees = Scripts.commitTxFee(remoteParams.dustLimitSat, remoteSpec).amount
        val missing = remoteSpec.toLocalMsat / 1000L - localParams.channelReserveSat - fees
        if (missing < 0) throw new LightningException(s"Remote funder misses $missing SAT for fees")
      }

      val localPerCommitmentPoint = perCommitPoint(localParams.shaSeed, 0L)
      val commitmentInput = makeFundingInputInfo(fundingTxid, fundingTxOutputIndex,
        Satoshi(fundingSat), localParams.fundingPrivKey.publicKey, remoteParams.fundingPubkey)

      val (localCommitTx, _, _) = makeLocalTxs(0L, localParams, remoteParams, commitmentInput, localPerCommitmentPoint, localSpec)
      val (remoteCommitTx, _, _, _, _) = makeRemoteTxs(0L, localParams, remoteParams, commitmentInput, remoteFirstPoint, remoteSpec)
      (localSpec, localCommitTx, remoteSpec, remoteCommitTx)
    }
  }
}