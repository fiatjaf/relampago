package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Scripts._
import com.lightning.wallet.ln.crypto.ShaChain._
import com.lightning.wallet.ln.crypto.Generators._
import fr.acinq.bitcoin.Crypto.{Point, PublicKey, Scalar}
import scala.util.{Success, Try}


object Helpers { me =>
  def makeLocalTxs(commitTxNumber: Long, localParams: LocalParams,
                   remoteParams: AcceptChannel, commitmentInput: InputInfo,
                   localPerCommitmentPoint: Point, spec: CommitmentSpec) = {

    val localHtlcPubkey = derivePubKey(localParams.htlcBasepoint, localPerCommitmentPoint)
    val localDelayedPaymentPubkey = derivePubKey(localParams.delayedPaymentBasepoint, localPerCommitmentPoint)
    val localRevocationPubkey = revocationPubKey(remoteParams.revocationBasepoint, localPerCommitmentPoint)
    val remotePaymentPubkey = derivePubKey(remoteParams.paymentBasepoint, localPerCommitmentPoint)
    val remoteHtlcPubkey = derivePubKey(remoteParams.htlcBasepoint, localPerCommitmentPoint)

    val commitTx = Scripts.makeCommitTx(commitmentInput, commitTxNumber, localParams.paymentBasepoint,
      remoteParams.paymentBasepoint, localParams.isFunder, LNParams.dustLimit, localRevocationPubkey,
      remoteParams.toSelfDelay, localDelayedPaymentPubkey, remotePaymentPubkey, localHtlcPubkey,
      remoteHtlcPubkey, spec)

    val htlcTimeoutTxs \ htlcSuccessTxs = Scripts.makeHtlcTxs(commitTx.tx, LNParams.dustLimit,
      localRevocationPubkey, remoteParams.toSelfDelay, localDelayedPaymentPubkey, localHtlcPubkey,
      remoteHtlcPubkey, spec)

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

    val commitTx = Scripts.makeCommitTx(commitmentInput, commitTxNumber, remoteParams.paymentBasepoint,
      localParams.paymentBasepoint, !localParams.isFunder, remoteParams.dustLimitSat, remoteRevocationPubkey,
      localParams.toSelfDelay, remoteDelayedPaymentPubkey, localPaymentPubkey, remoteHtlcPubkey,
      localHtlcPubkey, spec)

    val htlcTimeoutTxs \ htlcSuccessTxs = Scripts.makeHtlcTxs(commitTx.tx, remoteParams.dustLimitSat,
      remoteRevocationPubkey, localParams.toSelfDelay, remoteDelayedPaymentPubkey, remoteHtlcPubkey,
      localHtlcPubkey, spec)

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
      val estimatedWeight: Int = Transaction.weight(Scripts.addSigs(makeFunderClosingTx(commitments.commitInput, localScriptPubkey,
        remoteScriptPubkey, Satoshi(0), Satoshi(0), commitments.localCommit.spec), commitments.localParams.fundingPrivKey.publicKey,
        commitments.remoteParams.fundingPubkey, "aa" * 71, "bb" * 71).tx)

      // We don't need an extra high fee for a closing transaction so we offer an economical one
      val closingFee = Scripts.weight2fee(commitments.localCommit.spec.feeratePerKw / 2, estimatedWeight)
      makeClosing(commitments, closingFee, localScriptPubkey, remoteScriptPubkey)
    }

    def makeClosing(commitments: Commitments, closingFee: Satoshi,
                    local: BinaryData, remote: BinaryData) = {

      require(isValidFinalScriptPubkey(local), "Invalid localScriptPubkey")
      require(isValidFinalScriptPubkey(remote), "Invalid remoteScriptPubkey")

      val theirDustIsHigherThanOurs = LNParams.dustLimit < commitments.remoteParams.dustLimitSat
      val dustLimit = if (theirDustIsHigherThanOurs) commitments.remoteParams.dustLimitSat else LNParams.dustLimit
      val closing = makeFunderClosingTx(commitments.commitInput, local, remote, dustLimit, closingFee, commitments.localCommit.spec)

      val localClosingSig = Scripts.sign(closing, commitments.localParams.fundingPrivKey)
      val closingSigned = ClosingSigned(commitments.channelId, closingFee.amount, localClosingSig)
      ClosingTxProposed(closing, closingSigned)
    }

    def makeFunderClosingTx(commitTxInput: InputInfo, localScriptPubKey: BinaryData, remoteScriptPubKey: BinaryData,
                            dustLimit: Satoshi, closingFee: Satoshi, spec: CommitmentSpec): ClosingTx = {

      require(spec.htlcs.isEmpty, "No HTLCs allowed")
      val toRemoteAmount: Satoshi = MilliSatoshi(spec.toRemoteMsat)
      val toLocalAmount: Satoshi = MilliSatoshi(spec.toLocalMsat) - closingFee
      val toLocalOutput = if (toLocalAmount < dustLimit) Nil else TxOut(toLocalAmount, localScriptPubKey) :: Nil
      val toRemoteOutput = if (toRemoteAmount < dustLimit) Nil else TxOut(toRemoteAmount, remoteScriptPubKey) :: Nil
      val input = TxIn(commitTxInput.outPoint, Array.emptyByteArray, sequence = 0xffffffffL) :: Nil
      val tx = Transaction(version = 2, input, toLocalOutput ++ toRemoteOutput, lockTime = 0)
      ClosingTx(commitTxInput, LexicographicalOrdering sort tx)
    }

    def claimCurrentLocalCommitTxOutputs(commitments: Commitments, bag: PaymentInfoBag) = {
      val localPerCommitmentPoint = perCommitPoint(commitments.localParams.shaSeed, commitments.localCommit.index.toInt)
      val localRevocationPubkey = revocationPubKey(commitments.remoteParams.revocationBasepoint, localPerCommitmentPoint)
      val localDelayedPrivkey = derivePrivKey(commitments.localParams.delayedPaymentKey, localPerCommitmentPoint)

      def makeClaimDelayedOutput(tx: Transaction): ClaimDelayedOutputTx = {
        val claimDelayed = Scripts.makeClaimDelayedOutputTx(tx, localRevocationPubkey, commitments.remoteParams.toSelfDelay,
          localDelayedPrivkey.publicKey, commitments.localParams.defaultFinalScriptPubKey, commitments.localCommit.spec.feeratePerKw)

        val sig = Scripts.sign(claimDelayed, localDelayedPrivkey)
        Scripts.addSigs(claimDelayed, sig)
      }

      val allSuccessTxs = for {
        HtlcTxAndSigs(info: HtlcSuccessTx, localSig, remoteSig) <- commitments.localCommit.htlcTxsAndSigs
        paymentInfo <- bag.getPaymentInfo(info.add.paymentHash).toOption

        // First transaction can be spent immediately, subsequent one has CSV timeout
        success = Scripts.addSigs(info, localSig, remoteSig, paymentInfo.preimage)
        delayedClaim <- Scripts checkSpendable makeClaimDelayedOutput(success.tx)
      } yield success -> delayedClaim

      val allTimeoutTxs = for {
        HtlcTxAndSigs(info: HtlcTimeoutTx, localSig, remoteSig) <- commitments.localCommit.htlcTxsAndSigs
        timeout = Scripts.addSigs(info, localSig, remoteSig)

        // First transaction has CLTV timeout, subsequent one has CSV timeout
        delayedClaim <- Scripts checkSpendable makeClaimDelayedOutput(timeout.tx)
      } yield timeout -> delayedClaim

      val claimMainDelayedTx = Scripts.checkSpendable {
        // When local commit is spent our main output is also delayed
        makeClaimDelayedOutput(commitments.localCommit.commitTx.tx)
      }

      LocalCommitPublished(claimMainDelayedTx.toList, allSuccessTxs,
        allTimeoutTxs, commitTx = commitments.localCommit.commitTx.tx)
    }

    // remoteCommit may refer to their current or next RemoteCommit, hence it is a separate parameter
    def claimRemoteCommitTxOutputs(commitments: Commitments, remoteCommit: RemoteCommit, bag: PaymentInfoBag) = {
      val localHtlcPrivkey = derivePrivKey(commitments.localParams.htlcKey, remoteCommit.remotePerCommitmentPoint)

      val (remoteCommitTx, timeoutTxs, successTxs, remoteHtlcPubkey, remoteRevocationPubkey) =
        makeRemoteTxs(remoteCommit.index, commitments.localParams, commitments.remoteParams,
          commitments.commitInput, remoteCommit.remotePerCommitmentPoint, remoteCommit.spec)

      val claimSuccessTxs = for {
        HtlcTimeoutTx(_, _, add) <- timeoutTxs
        paymentInfo <- bag.getPaymentInfo(add.paymentHash).toOption
        claimHtlcSuccessTx = Scripts.makeClaimHtlcSuccessTx(remoteCommitTx.tx, localHtlcPrivkey.publicKey,
          remoteHtlcPubkey, remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey,
          add, remoteCommit.spec.feeratePerKw)

        signature = Scripts.sign(claimHtlcSuccessTx, localHtlcPrivkey)
        signed = Scripts.addSigs(claimHtlcSuccessTx, signature, paymentInfo.preimage)
        claimSuccess <- Scripts checkSpendable signed
      } yield claimSuccess

      val claimTimeoutTxs = for {
        HtlcSuccessTx(_, _, add) <- successTxs
        claimHtlcTimeoutTx = Scripts.makeClaimHtlcTimeoutTx(remoteCommitTx.tx, localHtlcPrivkey.publicKey,
          remoteHtlcPubkey, remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey,
          add, remoteCommit.spec.feeratePerKw)

        sig = Scripts.sign(claimHtlcTimeoutTx, localHtlcPrivkey)
        signed = Scripts.addSigs(claimHtlcTimeoutTx, sig)
        claimTimeout <- Scripts checkSpendable signed
      } yield claimTimeout

      val main = claimRemoteMainOutput(commitments, remoteCommit.remotePerCommitmentPoint, remoteCommitTx.tx)
      main.copy(claimHtlcSuccess = claimSuccessTxs.toList, claimHtlcTimeout = claimTimeoutTxs.toList)
    }

    def claimRemoteMainOutput(commitments: Commitments, remotePerCommitmentPoint: Point, commitTx: Transaction) = {
      // May be a special case where we have lost our data and explicitly ask them to spend their local current commit
      val localPaymentPrivkey = derivePrivKey(commitments.localParams.paymentKey, remotePerCommitmentPoint)

      RemoteCommitPublished(Scripts.checkSpendable {
        val txWithInputInfo = Scripts.makeClaimP2WPKHOutputTx(commitTx, localPaymentPrivkey.publicKey,
          commitments.localParams.defaultFinalScriptPubKey, LNParams.broadcaster.ratePerKwSat)

        val sig = Scripts.sign(txWithInputInfo, localPaymentPrivkey)
        Scripts.addSigs(txWithInputInfo, localPaymentPrivkey.publicKey, sig)
      }.toList, claimHtlcSuccess = Nil, claimHtlcTimeout = Nil, commitTx)
    }

    def claimRevokedRemoteCommitTxOutputs(commitments: Commitments, tx: Transaction) = {
      val txNumber = Scripts.obscuredCommitTxNumber(number = Scripts.decodeTxNumber(tx.txIn.head.sequence, tx.lockTime),
        !commitments.localParams.isFunder, commitments.remoteParams.paymentBasepoint, commitments.localParams.paymentBasepoint)

      val index = moves(largestTxIndex - txNumber)
      val hashes = commitments.remotePerCommitmentSecrets.hashes

      getHash(hashes)(index) map { remotePerCommitmentSecret =>
        val remotePerCommitmentSecretScalar = Scalar(remotePerCommitmentSecret)
        val remotePerCommitmentPoint = remotePerCommitmentSecretScalar.toPoint

        val remoteDelayedPaymentPubkey = derivePubKey(commitments.remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
        val remoteRevocationPrivkey = revocationPrivKey(commitments.localParams.revocationSecret, remotePerCommitmentSecretScalar)
        val localPrivkey = derivePrivKey(commitments.localParams.paymentKey, remotePerCommitmentPoint)

        val claimMainTx = Scripts checkSpendable {
          val claimMain = Scripts.makeClaimP2WPKHOutputTx(tx, localPrivkey.publicKey,
            commitments.localParams.defaultFinalScriptPubKey, LNParams.broadcaster.ratePerKwSat)

          val sig = Scripts.sign(claimMain, localPrivkey)
          Scripts.addSigs(claimMain, localPrivkey.publicKey, sig)
        }

        val claimPenaltyTx = Scripts checkSpendable {
          val txinfo = Scripts.makeMainPenaltyTx(tx, remoteRevocationPrivkey.publicKey,
            commitments.localParams.defaultFinalScriptPubKey, commitments.localParams.toSelfDelay,
            remoteDelayedPaymentPubkey, LNParams.broadcaster.ratePerKwSat)

          val sig = Scripts.sign(txinfo, remoteRevocationPrivkey)
          Scripts.addSigs(txinfo, sig)
        }

        RevokedCommitPublished(claimMainTx.toList,
          claimPenaltyTx.toList, commitTx = tx)
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

    // Assuming we are always a funder
    def makeFirstFunderCommitTxs(cmd: CMDOpenChannel, remoteParams: AcceptChannel,
                                 fundingTxHash: BinaryData, fundingTxOutputIndex: Int,
                                 remoteFirstPoint: Point) = {

      val toLocalMsat = cmd.realFundingAmountSat * 1000L - cmd.pushMsat
      val commitmentInput = makeFundingInputInfo(fundingTxHash, fundingTxOutputIndex,
        Satoshi(cmd.realFundingAmountSat), cmd.localParams.fundingPrivKey.publicKey,
        remoteParams.fundingPubkey)

      val localPerCommitmentPoint = perCommitPoint(cmd.localParams.shaSeed, 0L)
      val localSpec = CommitmentSpec(cmd.initialFeeratePerKw, toLocalMsat, cmd.pushMsat)
      val remoteSpec = CommitmentSpec(cmd.initialFeeratePerKw, cmd.pushMsat, toLocalMsat)
      val (localCommitTx, _, _) = makeLocalTxs(0L, cmd.localParams, remoteParams, commitmentInput, localPerCommitmentPoint, localSpec)
      val (remoteCommitTx, _, _, _, _) = makeRemoteTxs(0L, cmd.localParams, remoteParams, commitmentInput, remoteFirstPoint, remoteSpec)
      (localSpec, localCommitTx, remoteSpec, remoteCommitTx)
    }
  }
}