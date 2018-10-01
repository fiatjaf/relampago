package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Scripts._
import com.lightning.walletapp.ln.crypto.ShaChain._
import com.lightning.walletapp.ln.crypto.Generators._
import fr.acinq.bitcoin.Crypto.{Point, PublicKey, Scalar}
import scala.util.{Success, Try}


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

  object Closing {
    type SuccessAndClaim = (HtlcSuccessTx, ClaimDelayedOutputTx)
    type TimeoutAndClaim = (HtlcTimeoutTx, ClaimDelayedOutputTx)

    def isValidFinalScriptPubkey(raw: BinaryData) = Try(Script parse raw) match {
      case Success(OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pkh, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil) => pkh.data.size == 20
      case Success(OP_HASH160 :: OP_PUSHDATA(scriptHash, _) :: OP_EQUAL :: Nil) => scriptHash.data.size == 20
      case Success(OP_0 :: OP_PUSHDATA(pubkeyHash, _) :: Nil) if pubkeyHash.length == 20 => true
      case Success(OP_0 :: OP_PUSHDATA(scriptHash, _) :: Nil) if scriptHash.length == 32 => true
      case _ => false
    }

    def makeFirstClosing(commitments: Commitments, localScriptPubkey: BinaryData, remoteScriptPubkey: BinaryData) = {
      val closingTx = Scripts.addSigs(makeClosingTx(commitments.commitInput, localScriptPubkey, remoteScriptPubkey, Satoshi(0),
        Satoshi(0), commitments.localCommit.spec, commitments.localParams.isFunder), commitments.localParams.fundingPrivKey.publicKey,
        commitments.remoteParams.fundingPubkey, "aa" * 71, "bb" * 71)

      // There is no need for a high fee in a mutual closing tx AND mutual fee can't be bigger than last commit tx fee
      val computedClosingFee = Scripts.weight2fee(LNParams.broadcaster.perKwSixSat / 2, Transaction weight closingTx.tx)
      val lastCommitFee = commitments.commitInput.txOut.amount - commitments.localCommit.commitTx.tx.allOutputsAmount
      val closingFee = if (computedClosingFee > lastCommitFee) lastCommitFee else computedClosingFee
      makeClosing(commitments, closingFee, localScriptPubkey, remoteScriptPubkey)
    }

    def makeClosing(commitments: Commitments, closingFee: Satoshi, local: BinaryData, remote: BinaryData) = {
      val theirDustIsHigherThanOurs: Boolean = commitments.localParams.dustLimit < commitments.remoteParams.dustLimitSat
      val dustLimit = if (theirDustIsHigherThanOurs) commitments.remoteParams.dustLimitSat else commitments.localParams.dustLimit

      val closing: ClosingTx =
        makeClosingTx(commitments.commitInput, local, remote, dustLimit,
          closingFee, commitments.localCommit.spec, commitments.localParams.isFunder)

      val localClosingSig = Scripts.sign(closing, commitments.localParams.fundingPrivKey)
      val closingSigned = ClosingSigned(commitments.channelId, closingFee.amount, localClosingSig)
      require(isValidFinalScriptPubkey(remote), "Invalid remoteScriptPubkey")
      require(isValidFinalScriptPubkey(local), "Invalid localScriptPubkey")
      ClosingTxProposed(closing, closingSigned)
    }

    def makeClosingTx(commitTxInput: InputInfo, localScriptPubKey: BinaryData, remoteScriptPubKey: BinaryData,
                      dustLimit: Satoshi, closingFee: Satoshi, spec: CommitmentSpec, localIsFunder: Boolean) = {

      require(spec.htlcs.isEmpty, "No pending HTLCs allowed")
      val toRemoteAmount: Satoshi = if (localIsFunder) MilliSatoshi(spec.toRemoteMsat) else MilliSatoshi(spec.toRemoteMsat) - closingFee
      val toLocalAmount: Satoshi = if (localIsFunder) MilliSatoshi(spec.toLocalMsat) - closingFee else MilliSatoshi(spec.toLocalMsat)
      val toRemoteOutput = if (toRemoteAmount < dustLimit) Nil else TxOut(toRemoteAmount, remoteScriptPubKey) :: Nil
      val toLocalOutput = if (toLocalAmount < dustLimit) Nil else TxOut(toLocalAmount, localScriptPubKey) :: Nil
      val input = TxIn(commitTxInput.outPoint, BinaryData.empty, sequence = 0xffffffffL) :: Nil
      val tx = Transaction(2, input, toLocalOutput ++ toRemoteOutput, lockTime = 0)
      ClosingTx(commitTxInput, LexicographicalOrdering sort tx)
    }

    def claimCurrentLocalCommitTxOutputs(commitments: Commitments, bag: PaymentInfoBag) = {
      val localPerCommitmentPoint = perCommitPoint(commitments.localParams.shaSeed, commitments.localCommit.index.toInt)
      val localRevocationPubkey = revocationPubKey(commitments.remoteParams.revocationBasepoint, localPerCommitmentPoint)
      val localDelayedPrivkey = derivePrivKey(commitments.localParams.delayedPaymentKey, localPerCommitmentPoint)
      // No need to use a high fee rate for delayed transactions here
      val feeRate = LNParams.broadcaster.perKwSixSat

      def makeClaimDelayedOutput(tx: Transaction) = for {
        claimDelayed <- Scripts.makeClaimDelayedOutputTx(tx, localRevocationPubkey,
          commitments.remoteParams.toSelfDelay, localDelayedPrivkey.publicKey,
          commitments.localParams.defaultFinalScriptPubKey, feeRate,
          commitments.localParams.dustLimit)

        sig = Scripts.sign(claimDelayed, localDelayedPrivkey)
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
    def claimRemoteCommitTxOutputs(commitments: Commitments, remoteCommit: RemoteCommit, bag: PaymentInfoBag) = {
      val localHtlcPrivkey = derivePrivKey(commitments.localParams.htlcKey, remoteCommit.remotePerCommitmentPoint)
      // We need to use a rather high fee for htlc-claim because we compete with the counterparty
      val feeRate = LNParams.broadcaster.perKwThreeSat

      val (remoteCommitTx, timeout, success, remoteHtlcPubkey, remoteRevocationPubkey) =
        makeRemoteTxs(remoteCommit.index, commitments.localParams, commitments.remoteParams,
          commitments.commitInput, remoteCommit.remotePerCommitmentPoint, remoteCommit.spec)

      val finder = new PubKeyScriptIndexFinder(remoteCommitTx.tx)

      val claimSuccessTxs = for {
        HtlcTimeoutTx(_, _, add) <- timeout
        info <- bag.getPaymentInfo(add.paymentHash).toOption
        claimHtlcSuccessTx <- Scripts.makeClaimHtlcSuccessTx(finder, localHtlcPrivkey.publicKey,
          remoteHtlcPubkey, remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey,
          add, feeRate, commitments.localParams.dustLimit).toOption

        sig = Scripts.sign(claimHtlcSuccessTx, localHtlcPrivkey)
        signed = Scripts.addSigs(claimHtlcSuccessTx, sig, info.preimage)
        success <- Scripts.checkValid(signed).toOption
      } yield success

      val claimTimeoutTxs = for {
        HtlcSuccessTx(_, _, add) <- success
        claimHtlcTimeoutTx <- Scripts.makeClaimHtlcTimeoutTx(finder, localHtlcPrivkey.publicKey,
          remoteHtlcPubkey, remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey,
          add, feeRate, commitments.localParams.dustLimit).toOption

        sig = Scripts.sign(claimHtlcTimeoutTx, localHtlcPrivkey)
        signed = Scripts.addSigs(claimHtlcTimeoutTx, sig)
        timeout <- Scripts.checkValid(signed).toOption
      } yield timeout

      val main = claimRemoteMainOutput(commitments, remoteCommit.remotePerCommitmentPoint, remoteCommitTx.tx)
      main.copy(claimHtlcSuccess = claimSuccessTxs, claimHtlcTimeout = claimTimeoutTxs)
    }

    def claimRemoteMainOutput(commitments: Commitments, remotePerCommitmentPoint: Point, commitTx: Transaction) = {
      // May be a special case where we have lost our data and explicitly ask them to spend their local current commit
      val localPaymentPrivkey = derivePrivKey(commitments.localParams.paymentKey, remotePerCommitmentPoint)
      val finalScriptPubKey = commitments.localParams.defaultFinalScriptPubKey
      // No need to use a high fee rate for main transaction here
      val feeRate = LNParams.broadcaster.perKwSixSat

      val claimMain = for {
        claimP2WPKH <- Scripts.makeClaimP2WPKHOutputTx(commitTx,
          localPaymentPrivkey.publicKey, finalScriptPubKey, feeRate,
          commitments.localParams.dustLimit)

        sig = Scripts.sign(txinfo = claimP2WPKH, localPaymentPrivkey)
        signed = Scripts.addSigs(claimP2WPKH, sig, localPaymentPrivkey.publicKey)
        main <- Scripts.checkValid(signed)
      } yield main

      // We only claim a main output here in case when it's a refunding
      RemoteCommitPublished(claimMain.toOption.toSeq, Nil, Nil, commitTx)
    }

    def claimRevokedRemoteCommitTxOutputs(commitments: Commitments, tx: Transaction, bag: PaymentInfoBag) = {
      val txNumber = Scripts.obscuredCommitTxNumber(number = Scripts.decodeTxNumber(tx.txIn.head.sequence, tx.lockTime),
        !commitments.localParams.isFunder, commitments.remoteParams.paymentBasepoint, commitments.localParams.paymentBasepoint)

      val index = moves(largestTxIndex - txNumber)
      getHash(commitments.remotePerCommitmentSecrets.hashes)(index) map { perCommitSecret =>
        // At the very least we should take both balances + HTLCs if could be found in database

        val remotePerCommitmentSecretScalar = Scalar(perCommitSecret)
        val remotePerCommitmentPoint = remotePerCommitmentSecretScalar.toPoint

        val finalScriptPubKey = commitments.localParams.defaultFinalScriptPubKey
        val localPrivkey = derivePrivKey(commitments.localParams.paymentKey, remotePerCommitmentPoint)
        val remoteDelayedPaymentKey = derivePubKey(commitments.remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
        val remoteRevocationPrivkey = revocationPrivKey(commitments.localParams.revocationSecret, remotePerCommitmentSecretScalar)
        val remoteRevocationPubkey = remoteRevocationPrivkey.publicKey
        // No need to use a high fee rate for main transactions here
        val feeRate = LNParams.broadcaster.perKwThreeSat

        val claimMainTx = for {
          makeClaimP2WPKH <- Scripts.makeClaimP2WPKHOutputTx(tx, localPrivkey.publicKey,
            finalScriptPubKey, feeRate, commitments.localParams.dustLimit)

          sig = Scripts.sign(txinfo = makeClaimP2WPKH, localPrivkey)
          signed = Scripts.addSigs(makeClaimP2WPKH, sig, localPrivkey.publicKey)
          main <- Scripts.checkValid(signed)
        } yield main

        val claimPenaltyTx = for {
          theirMainPenalty <- Scripts.makeMainPenaltyTx(tx, remoteRevocationPubkey,
            finalScriptPubKey, commitments.localParams.toSelfDelay, remoteDelayedPaymentKey,
            feeRate, commitments.localParams.dustLimit)

          sig = Scripts.sign(theirMainPenalty, remoteRevocationPrivkey)
          signed = Scripts.addSigs(theirMainPenalty, sig)
          their <- Scripts.checkValid(signed)
        } yield their

        val localHtlc = derivePubKey(commitments.localParams.htlcBasepoint, remotePerCommitmentPoint)
        val remoteHtlc = derivePubKey(commitments.remoteParams.htlcBasepoint, remotePerCommitmentPoint)
        val remoteRev = revocationPubKey(commitments.localParams.revocationBasepoint, remotePerCommitmentPoint)

        val matching = bag getAllRevoked txNumber
        val finder = new PubKeyScriptIndexFinder(tx)
        val offered = for (h160 \ _ <- matching) yield Scripts.htlcOffered(remoteHtlc, localHtlc, remoteRev, h160)
        val received = for (h160 \ expiry <- matching) yield Scripts.htlcReceived(remoteHtlc, localHtlc, remoteRev, h160, expiry)
        val redeemScripts = for (redeem <- offered ++ received) yield Script.write(Script pay2wsh redeem) -> Script.write(redeem)
        val redeemMap = redeemScripts.toMap

        val htlcPenaltyTxs = for {
          TxOut(_, publicKeyScript) <- tx.txOut
          redeemScript <- redeemMap get publicKeyScript
          htlcPenaltyTx <- Scripts.makeHtlcPenaltyTx(finder, redeemScript,
            finalScriptPubKey, feeRate, commitments.localParams.dustLimit).toOption

          sig = Scripts.sign(htlcPenaltyTx, remoteRevocationPrivkey)
          signed = Scripts.addSigs(htlcPenaltyTx, sig, remoteRevocationPubkey)
          penalty <- Scripts.checkValid(signed).toOption
        } yield penalty

        RevokedCommitPublished(claimMainTx.toOption.toSeq,
          claimPenaltyTx.toOption.toSeq, htlcPenaltyTxs, tx)
      }
    }
  }

  object Funding {
    def makeFundingInputInfo(fundingTxHash: BinaryData, fundingTxOutputIndex: Int,
                             fundingSatoshis: Satoshi, fundingPubkey1: PublicKey,
                             fundingPubkey2: PublicKey): InputInfo = {

      val multisig = Scripts.multiSig2of2(fundingPubkey1, fundingPubkey2)
      val fundingTxOut = TxOut(fundingSatoshis, Script pay2wsh multisig)
      val outPoint = OutPoint(fundingTxHash, fundingTxOutputIndex)
      InputInfo(outPoint, fundingTxOut, Script write multisig)
    }

    def makeFirstCommitTxs(localParams: LocalParams, fundingSat: Long, pushMsat: Long, initialFeeratePerKw: Long,
                           remoteParams: AcceptChannel, fundingTxHash: BinaryData, fundingTxOutputIndex: Int,
                           remoteFirstPoint: Point) = {

      val toLocalMsat = if (localParams.isFunder) fundingSat * 1000L - pushMsat else pushMsat
      val toRemoteMsat = if (localParams.isFunder) pushMsat else fundingSat * 1000L - pushMsat

      val localSpec = CommitmentSpec(initialFeeratePerKw, toLocalMsat, toRemoteMsat)
      val remoteSpec = CommitmentSpec(initialFeeratePerKw, toRemoteMsat, toLocalMsat)

      if (!localParams.isFunder) {
        val fees = Scripts.commitTxFee(remoteParams.dustLimitSat, remoteSpec).amount
        val missing = remoteSpec.toLocalMsat / 1000L - localParams.channelReserveSat - fees
        if (missing < 0) throw new LightningException("They are funder and can not afford fees")
      }

      val localPerCommitmentPoint = perCommitPoint(localParams.shaSeed, 0L)
      val commitmentInput = makeFundingInputInfo(fundingTxHash, fundingTxOutputIndex,
        Satoshi(fundingSat), localParams.fundingPrivKey.publicKey, remoteParams.fundingPubkey)

      val (localCommitTx, _, _) = makeLocalTxs(0L, localParams, remoteParams, commitmentInput, localPerCommitmentPoint, localSpec)
      val (remoteCommitTx, _, _, _, _) = makeRemoteTxs(0L, localParams, remoteParams, commitmentInput, remoteFirstPoint, remoteSpec)
      (localSpec, localCommitTx, remoteSpec, remoteCommitTx)
    }
  }
}