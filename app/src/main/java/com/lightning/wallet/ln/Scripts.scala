package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire.UpdateAddHtlc
import scala.language.postfixOps
import java.nio.ByteOrder
import scala.util.Try

import fr.acinq.bitcoin.Crypto.{Point, PrivateKey, PublicKey}
import fr.acinq.bitcoin.SigVersion.SIGVERSION_WITNESS_V0
import fr.acinq.bitcoin.SigVersion.SIGVERSION_BASE
import ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS


object Scripts { me =>
  type ScriptEltSeq = Seq[ScriptElt]

  def multiSig2of2(pubkey1: PublicKey, pubkey2: PublicKey): ScriptEltSeq =
    LexicographicalOrdering.isLessThan(pubkey1.toBin, pubkey2.toBin) match {
      case false => Script.createMultiSigMofN(m = 2, pubkey2 :: pubkey1 :: Nil)
      case true => Script.createMultiSigMofN(m = 2, pubkey1 :: pubkey2 :: Nil)
    }

  def witness2of2(sig1: BinaryData, sig2: BinaryData, pubkey1: PublicKey, pubkey2: PublicKey) =
    LexicographicalOrdering.isLessThan(pubkey1.toBin, pubkey2.toBin) -> multiSig2of2(pubkey1, pubkey2) match {
      case (false, multisig) => ScriptWitness(BinaryData.empty :: sig2 :: sig1 :: Script.write(multisig) :: Nil)
      case (true, multisig) => ScriptWitness(BinaryData.empty :: sig1 :: sig2 :: Script.write(multisig) :: Nil)
    }

  def cltvBlocks(tx: Transaction): Long =
    if (tx.lockTime <= LockTimeThreshold) tx.lockTime else 0L

  def csvTimeout(tx: Transaction): Long =
    if (tx.version < 2) 0L else tx.txIn map { in =>
      val isCsvDisabled = (in.sequence & TxIn.SEQUENCE_LOCKTIME_DISABLE_FLAG) != 0L
      if (isCsvDisabled) 0L else in.sequence & TxIn.SEQUENCE_LOCKTIME_MASK
    } max

  def encodeNumber(number: Long): ScriptElt = number match {
    case n if n < -1 | n > 16 => OP_PUSHDATA(Script encodeNumber n)

    case x if x >= 1 && x <= 16 =>
      val code = ScriptElt elt2code OP_1
      val result = (code + x - 1).toInt
      ScriptElt code2elt result

    case -1 => OP_1NEGATE
    case 0 => OP_0
  }

  // LN SCRIPTS
  // @formatter:off

  def toLocalDelayed(revocationPubkey: PublicKey, toSelfDelay: Int,
                     localDelayedPaymentPubkey: PublicKey): ScriptEltSeq =

    OP_IF ::
      OP_PUSHDATA(revocationPubkey) ::
    OP_ELSE ::
      encodeNumber(toSelfDelay) ::
      OP_CHECKSEQUENCEVERIFY :: OP_DROP ::
      OP_PUSHDATA(localDelayedPaymentPubkey) ::
    OP_ENDIF ::
    OP_CHECKSIG :: Nil

  def htlcOffered(localHtlcPubkey: PublicKey, remoteHtlcPubkey: PublicKey,
                  revocationPubKey: PublicKey, paymentHash: BinaryData) =

    OP_DUP :: OP_HASH160 ::
    OP_PUSHDATA(revocationPubKey.hash160) ::
    OP_EQUAL ::
    OP_IF ::
      // To you with revocation key
      OP_CHECKSIG ::
    OP_ELSE ::
      OP_PUSHDATA(remoteHtlcPubkey) :: OP_SWAP ::
      OP_SIZE :: encodeNumber(32) :: OP_EQUAL ::
      OP_NOTIF ::
        // To me via timelocked HTLC-timeout transaction
        OP_DROP :: OP_2 :: OP_SWAP ::
        OP_PUSHDATA(localHtlcPubkey) ::
        OP_2 :: OP_CHECKMULTISIG ::
      OP_ELSE ::
        OP_HASH160 :: OP_PUSHDATA(paymentHash) ::
        OP_EQUALVERIFY :: OP_CHECKSIG ::
      OP_ENDIF ::
    OP_ENDIF :: Nil

  def htlcReceived(localHtlcPubkey: PublicKey, remoteHtlcPubkey: PublicKey,
                   revocationPubKey: PublicKey, paymentHash: BinaryData,
                   lockTime: Long): ScriptEltSeq =

    OP_DUP :: OP_HASH160 ::
    OP_PUSHDATA(revocationPubKey.hash160) ::
    OP_EQUAL ::
    OP_IF ::
      // To you with revocation key
      OP_CHECKSIG ::
    OP_ELSE ::
      OP_PUSHDATA(remoteHtlcPubkey) :: OP_SWAP ::
      OP_SIZE :: encodeNumber(32) :: OP_EQUAL ::
      OP_IF ::
        // To me via HTLC-success transaction
        OP_HASH160 :: OP_PUSHDATA(paymentHash) :: OP_EQUALVERIFY ::
        OP_2 :: OP_SWAP :: OP_PUSHDATA(localHtlcPubkey) ::
        OP_2 :: OP_CHECKMULTISIG ::
      OP_ELSE ::
        // To you after timeout
        OP_DROP :: encodeNumber(lockTime) ::
        OP_CHECKLOCKTIMEVERIFY :: OP_DROP ::
        OP_CHECKSIG ::
      OP_ENDIF ::
    OP_ENDIF :: Nil

  // @formatter:on

  // TRANSACTION TEMPLATES

  trait TransactionWithInputInfo {
    // Input of current tx - output of next tx output reveals a final fee
    def --(that: TransactionWithInputInfo) = input.txOut.amount - that.amount
    def amount: Satoshi = tx.txOut.reduce(_.amount + _.amount)
    def input: InputInfo
    def tx: Transaction
  }

  /**
    * When *local* *current* [[CommitTx]] is published:
    *   - [[ClaimDelayedOutputTx]] spends to-local output of [[CommitTx]] after a delay
    *   - [[HtlcSuccessTx]] spends htlc-received outputs of [[CommitTx]] for which we have the preimage
    *     - [[ClaimDelayedOutputTx]] spends [[HtlcSuccessTx]] after a delay
    *   - [[HtlcTimeoutTx]] spends htlc-sent outputs of [[CommitTx]] after a timeout
    *     - [[ClaimDelayedOutputTx]] spends [[HtlcTimeoutTx]] after a delay
    *
    * When *remote* *current* [[CommitTx]] is published:
    *   - [[ClaimP2WPKHOutputTx]] spends to-local output of [[CommitTx]]
    *   - [[ClaimHtlcSuccessTx]] spends htlc-received outputs of [[CommitTx]] for which we have the preimage
    *   - [[ClaimHtlcTimeoutTx]] spends htlc-sent outputs of [[CommitTx]] after a timeout
    *
    * When *remote* *revoked* [[CommitTx]] is published:
    *   - [[ClaimP2WPKHOutputTx]] spends to-local output of [[CommitTx]]
    *   - [[MainPenaltyTx]] spends remote main output using the per-commitment secret
    *   - [[HtlcSuccessTx]] spends htlc-sent outputs of [[CommitTx]] for which they have the preimage (published by remote)
    *     - [[HtlcPenaltyTx]] spends [[HtlcSuccessTx]] using the per-commitment secret
    *   - [[ClaimHtlcTimeoutTx]] spends htlc-sent outputs of [[CommitTx]] after a timeout
    *   - [[HtlcTimeoutTx]] spends htlc-received outputs of [[CommitTx]] after a timeout (published by local or remote)
    *     - [[HtlcPenaltyTx]] spends [[HtlcTimeoutTx]] using the per-commitment secret
    */

  case class InputInfo(outPoint: OutPoint, txOut: TxOut, redeemScript: BinaryData)
  case class CommitTx(input: InputInfo, tx: Transaction) extends TransactionWithInputInfo

  case class HtlcSuccessTx(input: InputInfo, tx: Transaction, add: UpdateAddHtlc) extends TransactionWithInputInfo
  case class HtlcTimeoutTx(input: InputInfo, tx: Transaction, add: UpdateAddHtlc) extends TransactionWithInputInfo

  case class ClaimHtlcSuccessTx(input: InputInfo, tx: Transaction) extends TransactionWithInputInfo
  case class ClaimHtlcTimeoutTx(input: InputInfo, tx: Transaction) extends TransactionWithInputInfo
  case class ClaimP2WPKHOutputTx(input: InputInfo, tx: Transaction) extends TransactionWithInputInfo
  case class ClaimDelayedOutputTx(input: InputInfo, tx: Transaction) extends TransactionWithInputInfo
  case class MainPenaltyTx(input: InputInfo, tx: Transaction) extends TransactionWithInputInfo
  case class HtlcPenaltyTx(input: InputInfo, tx: Transaction) extends TransactionWithInputInfo
  case class ClosingTx(input: InputInfo, tx: Transaction) extends TransactionWithInputInfo

  val commitWeight = 724
  val htlcTimeoutWeight = 663
  val htlcSuccessWeight = 703
  val claimP2WPKHOutputWeight = 437
  val claimHtlcDelayedWeight = 482
  val claimHtlcSuccessWeight = 570
  val claimHtlcTimeoutWeight = 544
  val mainPenaltyWeight = 483

  // Fee calculation
  def weight2fee(feeratePerKw: Long, weight: Int) =
    Satoshi(feeratePerKw * weight / 1000L)

  private def trimOfferedHtlcs(dustLimit: Satoshi, spec: CommitmentSpec) = {
    val htlcTimeoutFee: MilliSatoshi = weight2fee(spec.feeratePerKw, htlcTimeoutWeight) + dustLimit
    spec.htlcs.collect { case Htlc(false, add) if add.amountMsat >= htlcTimeoutFee.amount => add }.toSeq
  }

  private def trimReceivedHtlcs(dustLimit: Satoshi, spec: CommitmentSpec) = {
    val htlcSuccessFee: MilliSatoshi = weight2fee(spec.feeratePerKw, htlcSuccessWeight) + dustLimit
    spec.htlcs.collect { case Htlc(true, add) if add.amountMsat >= htlcSuccessFee.amount => add }.toSeq
  }

  def commitTxFee(dustLimit: Satoshi, spec: CommitmentSpec): Satoshi = {
    val trimmedOfferedHtlcs = 172 * trimOfferedHtlcs(dustLimit, spec).size
    val trimmedReceivedHtlcs = 172 * trimReceivedHtlcs(dustLimit, spec).size
    val weight = commitWeight + trimmedOfferedHtlcs + trimmedReceivedHtlcs
    weight2fee(spec.feeratePerKw, weight)
  }

  // Commit tx with obscured tx number
  // SHA256(payment-basepoint from open_channel || payment-basepoint from accept_channel)
  def obscuredCommitTxNumber(number: Long, isFunder: Boolean, local: Point, remote: Point) = {
    val (paymentBasepoint1, paymentBasepoint2) = if (isFunder) (local, remote) else (remote, local)
    val combined = paymentBasepoint1.toBin(compressed = true) ++ paymentBasepoint2.toBin(compressed = true)
    val blind = Crypto.sha256(combined).takeRight(6).reverse ++ BinaryData("0x0000")
    number ^ Protocol.uint64(blind, ByteOrder.LITTLE_ENDIAN)
  }

  def getCommitTxNumber(commitTx: Transaction, isFunder: Boolean, local: Point, remote: Point): Long =
    decodeTxNumber(commitTx.txIn.head.sequence, commitTx.lockTime) ^ obscuredCommitTxNumber(0, isFunder, local, remote)

  // This is a trick to split and encode a 48-bit txnumber into the sequence and locktime fields of a tx
  def encodeTxNumber(txnumber: Long) = (0x80000000L | (txnumber >> 24), (txnumber & 0xffffffL) | 0x20000000)
  def decodeTxNumber(sequence: Long, locktime: Long) = (sequence & 0xffffffL).<<(24) + (locktime & 0xffffffL)

  // Tx siging and checking
  def addSigs(commit: CommitTx, localKey: PublicKey, remoteKey: PublicKey, localSig: BinaryData, remoteSig: BinaryData): CommitTx =
    commit.modify(_.tx).using(_ updateWitnesses witness2of2(localSig, remoteSig, localKey, remoteKey) :: Nil)

  def addSigs(closing: ClosingTx, localFunding: PublicKey, remoteFunding: PublicKey, localSig: BinaryData, remoteSig: BinaryData): ClosingTx =
    closing.modify(_.tx).using(_ updateWitnesses witness2of2(localSig, remoteSig, localFunding, remoteFunding) :: Nil)

  def addSigs(claimMainDelayedRevokedTx: MainPenaltyTx, revocationSig: BinaryData): MainPenaltyTx =
    claimMainDelayedRevokedTx.modify(_.tx).using(_ updateWitnesses ScriptWitness(revocationSig :: BinaryData("01") ::
      claimMainDelayedRevokedTx.input.redeemScript :: Nil) :: Nil)

  def addSigs(htlcSuccessTx: HtlcSuccessTx, localSig: BinaryData, remoteSig: BinaryData, paymentPreimage: BinaryData): HtlcSuccessTx =
    htlcSuccessTx.modify(_.tx).using(_ updateWitnesses ScriptWitness(BinaryData.empty :: remoteSig :: localSig :: paymentPreimage ::
      htlcSuccessTx.input.redeemScript :: Nil) :: Nil)

  def addSigs(htlcTimeoutTx: HtlcTimeoutTx, localSig: BinaryData, remoteSig: BinaryData): HtlcTimeoutTx =
    htlcTimeoutTx.modify(_.tx).using(_ updateWitnesses ScriptWitness(BinaryData.empty :: remoteSig ::
      localSig :: BinaryData.empty :: htlcTimeoutTx.input.redeemScript :: Nil) :: Nil)

  def addSigs(claimHtlcSuccessTx: ClaimHtlcSuccessTx, localSig: BinaryData, paymentPreimage: BinaryData): ClaimHtlcSuccessTx =
    claimHtlcSuccessTx.modify(_.tx).using(_ updateWitnesses ScriptWitness(localSig :: paymentPreimage ::
      claimHtlcSuccessTx.input.redeemScript :: Nil) :: Nil)

  def addSigs(claimHtlcTimeoutTx: ClaimHtlcTimeoutTx, localSig: BinaryData): ClaimHtlcTimeoutTx =
    claimHtlcTimeoutTx.modify(_.tx).using(_ updateWitnesses ScriptWitness(localSig :: BinaryData.empty ::
      claimHtlcTimeoutTx.input.redeemScript :: Nil) :: Nil)

  def addSigs(claimP2WPKHOutputTx: ClaimP2WPKHOutputTx, localPaymentPubkey: BinaryData, localSig: BinaryData): ClaimP2WPKHOutputTx =
    claimP2WPKHOutputTx.modify(_.tx).using(_ updateWitnesses ScriptWitness(localSig :: localPaymentPubkey :: Nil) :: Nil)

  def addSigs(claimHtlcDelayed: ClaimDelayedOutputTx, localSig: BinaryData): ClaimDelayedOutputTx =
    claimHtlcDelayed.modify(_.tx).using(_ updateWitnesses ScriptWitness(localSig :: BinaryData.empty ::
      claimHtlcDelayed.input.redeemScript :: Nil) :: Nil)

  def sign(tx: Transaction, inputIndex: Int, redeemScript: BinaryData, amount: Satoshi, key: PrivateKey): BinaryData =
    Transaction.signInput(tx, inputIndex, redeemScript, SIGHASH_ALL, amount, SIGVERSION_WITNESS_V0, key)

  def sign(tx: Transaction, inputIndex: Int, redeemScript: BinaryData, key: PrivateKey): BinaryData =
    Transaction.signInput(tx, inputIndex, redeemScript, SIGHASH_ALL, Satoshi(0), SIGVERSION_BASE, key)

  def sign(txinfo: TransactionWithInputInfo, key: PrivateKey): BinaryData =
    sign(txinfo.tx, inputIndex = 0, txinfo.input.redeemScript, txinfo.input.txOut.amount, key)

  def checkSpendable[T <: TransactionWithInputInfo](txWithInputInfo: => T) = Try {
    if (txWithInputInfo.tx.txOut.isEmpty) throw new Exception("Empty transaction found")
    val check = Map(txWithInputInfo.tx.txIn.head.outPoint -> txWithInputInfo.input.txOut)
    Transaction.correctlySpends(txWithInputInfo.tx, check, STANDARD_SCRIPT_VERIFY_FLAGS)
    txWithInputInfo
  } toOption

  def checkSig(txinfo: TransactionWithInputInfo, sig: BinaryData, pubKey: PublicKey): Boolean =
    Crypto.verifySignature(Transaction.hashForSigning(txinfo.tx, 0, txinfo.input.redeemScript,
      SIGHASH_ALL, txinfo.input.txOut.amount, SIGVERSION_WITNESS_V0), sig, pubKey)

  def makeCommitTx(commitTxInput: InputInfo, commitTxNumber: Long, localPaymentBasePoint: Point, remotePaymentBasePoint: Point,
                   localIsFunder: Boolean, localDustLimit: Satoshi, localRevocationPubkey: PublicKey, toLocalDelay: Int,
                   localDelayedPaymentPubkey: PublicKey, remotePaymentPubkey: PublicKey, localHtlcPubkey: PublicKey,
                   remoteHtlcPubkey: PublicKey, spec: CommitmentSpec): CommitTx = {

    val commitFee = commitTxFee(localDustLimit, spec)
    val toRemote: Satoshi = MilliSatoshi(spec.toRemoteMsat)
    val toLocal: Satoshi = MilliSatoshi(spec.toLocalMsat)

    val toLocalAmount \ toRemoteAmount =
      if (localIsFunder) Tuple2(toLocal - commitFee, toRemote)
      else Tuple2(toLocal, toRemote - commitFee)

    val toLocalDelayedOutput = if (toLocalAmount < localDustLimit) Nil else
      TxOut(publicKeyScript = Script pay2wsh toLocalDelayed(localRevocationPubkey,
        toLocalDelay, localDelayedPaymentPubkey), amount = toLocalAmount) :: Nil

    val toRemoteOutput = if (toRemoteAmount < localDustLimit) Nil else
      TxOut(toRemoteAmount, Script pay2wpkh remotePaymentPubkey) :: Nil

    val htlcOfferedOutputs = trimOfferedHtlcs(localDustLimit, spec) map { add =>
      TxOut(publicKeyScript = Script pay2wsh htlcOffered(localHtlcPubkey, remoteHtlcPubkey,
        localRevocationPubkey, Crypto ripemd160 add.paymentHash), amount = add.amount)
    }

    val htlcReceivedOutputs = trimReceivedHtlcs(localDustLimit, spec) map { add =>
      TxOut(publicKeyScript = Script pay2wsh htlcReceived(localHtlcPubkey, remoteHtlcPubkey,
        localRevocationPubkey, Crypto ripemd160 add.paymentHash, add.expiry), amount = add.amount)
    }

    // Make an obscured tx number as defined in BOLT #3 (a 48 bits integer) which we can use in case of contract breach
    val txNumber = obscuredCommitTxNumber(commitTxNumber, localIsFunder, localPaymentBasePoint, remotePaymentBasePoint)
    val (sequence, locktime) = encodeTxNumber(txNumber)

    val outs = toLocalDelayedOutput ++ toRemoteOutput ++ htlcOfferedOutputs ++ htlcReceivedOutputs
    val in = TxIn(commitTxInput.outPoint, Array.emptyByteArray, sequence = sequence) :: Nil
    val tx = Transaction(version = 2, in, outs, lockTime = locktime)
    CommitTx(commitTxInput, LexicographicalOrdering sort tx)
  }

  // General templates

  def tier2Amount(amt: Satoshi, fee: Satoshi): Satoshi = if (amt - fee < LNParams.dustLimit) amt / 2 else amt - fee
  def findPubKeyScriptIndex(tx: Transaction, script: BinaryData): Int = tx.txOut.indexWhere(_.publicKeyScript == script)

  def makeHtlcTx[T](fun: (InputInfo, Transaction) => T, parent: Transaction,
                    redeemScript: ScriptEltSeq, pubKeyScript: ScriptEltSeq,
                    amount: MilliSatoshi, fee: Satoshi, expiry: Long,
                    sequence: Long) = {

    val index: Int = findPubKeyScriptIndex(script = Script.write(Script pay2wsh redeemScript), tx = parent)
    val inputInfo = InputInfo(OutPoint(parent, index), parent.txOut(index), Script write redeemScript)
    val txIn = TxIn(inputInfo.outPoint, Array.emptyByteArray, sequence) :: Nil
    val txOut = TxOut(amount - fee, pubKeyScript) :: Nil
    val tx = Transaction(2, txIn, txOut, expiry)
    fun(inputInfo, tx)
  }

  def makeClaimHtlcTx[T](fun: (InputInfo, Transaction) => T, parent: Transaction,
                         redeemScript: ScriptEltSeq, pubKeyScript: ScriptEltSeq,
                         localFinalScriptPubKey: BinaryData, fee: Satoshi,
                         expiry: Long, sequence: Long) = {

    val index: Int = findPubKeyScriptIndex(script = Script.write(pubKeyScript), tx = parent)
    val inputInfo = InputInfo(OutPoint(parent, index), parent.txOut(index), Script write redeemScript)
    val txOut = TxOut(tier2Amount(inputInfo.txOut.amount, fee), localFinalScriptPubKey) :: Nil
    val txIn = TxIn(inputInfo.outPoint, Array.emptyByteArray, sequence) :: Nil
    val tx = Transaction(2, txIn, txOut, expiry)
    fun(inputInfo, tx)
  }

  // Concrete templates

  def makeHtlcTxs(commitTx: Transaction, dustLimit: Satoshi, localRevPubkey: PublicKey,
                  toLocalDelay: Int, localDelayedPaymentPubkey: PublicKey, localHtlcPubkey: PublicKey,
                  remoteHtlcPubkey: PublicKey, spec: CommitmentSpec) = {

    def makeHtlcTimeoutTx(add: UpdateAddHtlc) = {
      val paymentHash160 = Crypto ripemd160 add.paymentHash
      val offered = htlcOffered(localHtlcPubkey, remoteHtlcPubkey, localRevPubkey, paymentHash160)
      val pubKeyScript = Script pay2wsh toLocalDelayed(localRevPubkey, toLocalDelay, localDelayedPaymentPubkey)
      makeHtlcTx(HtlcTimeoutTx(_, _, add), commitTx, offered, pubKeyScript, add.amount,
        weight2fee(spec.feeratePerKw, htlcTimeoutWeight), add.expiry, 0x00000000L)
    }

    def makeHtlcSuccessTx(add: UpdateAddHtlc) = {
      val paymentHash160 = Crypto ripemd160 add.paymentHash
      val pubKeyScript = Script pay2wsh toLocalDelayed(localRevPubkey, toLocalDelay, localDelayedPaymentPubkey)
      val received = htlcReceived(localHtlcPubkey, remoteHtlcPubkey, localRevPubkey, paymentHash160, add.expiry)
      makeHtlcTx(HtlcSuccessTx(_, _, add), commitTx, received, pubKeyScript, add.amount,
        weight2fee(spec.feeratePerKw, htlcSuccessWeight), 0L, 0x00000000L)
    }

    // Dusty HTLCs are filtered out and thus go to fees
    val htlcTimeoutTxs = trimOfferedHtlcs(dustLimit, spec) map makeHtlcTimeoutTx
    val htlcSuccessTxs = trimReceivedHtlcs(dustLimit, spec) map makeHtlcSuccessTx
    htlcTimeoutTxs -> htlcSuccessTxs
  }

  def makeClaimHtlcTimeoutTx(commitTx: Transaction, localHtlcPubkey: PublicKey, remoteHtlcPubkey: PublicKey,
                             remoteRevocationPubkey: PublicKey, localFinalScriptPubKey: BinaryData,
                             add: UpdateAddHtlc, feeratePerKw: Long): ClaimHtlcTimeoutTx = {

    val paymentHash = Crypto ripemd160 add.paymentHash
    val redeem = htlcReceived(remoteHtlcPubkey, localHtlcPubkey, remoteRevocationPubkey, paymentHash, add.expiry)
    makeClaimHtlcTx(ClaimHtlcTimeoutTx, commitTx, redeem, Script pay2wsh redeem, localFinalScriptPubKey,
      weight2fee(feeratePerKw, claimHtlcTimeoutWeight), add.expiry, sequence = 0x00000000L)
  }

  def makeClaimHtlcSuccessTx(commitTx: Transaction, localHtlcPubkey: PublicKey, remoteHtlcPubkey: PublicKey,
                             remoteRevocationPubkey: PublicKey, localFinalScriptPubKey: BinaryData,
                             add: UpdateAddHtlc, feeratePerKw: Long): ClaimHtlcSuccessTx = {

    val paymentHash = Crypto ripemd160 add.paymentHash
    val redeem = htlcOffered(remoteHtlcPubkey, localHtlcPubkey, remoteRevocationPubkey, paymentHash)
    makeClaimHtlcTx(ClaimHtlcSuccessTx, commitTx, redeem, Script pay2wsh redeem, localFinalScriptPubKey,
      weight2fee(feeratePerKw, claimHtlcSuccessWeight), expiry = 0L, sequence = 0xffffffffL)
  }

  def makeClaimP2WPKHOutputTx(delayedOutputTx: Transaction, localPaymentPubkey: PublicKey,
                              localFinalScriptPubKey: BinaryData, feeratePerKw: Long) =

    makeClaimHtlcTx(ClaimP2WPKHOutputTx, delayedOutputTx, Script pay2pkh localPaymentPubkey, Script pay2wpkh localPaymentPubkey,
      localFinalScriptPubKey, weight2fee(feeratePerKw, claimP2WPKHOutputWeight), expiry = 0L, sequence = 0x00000000L)

  def makeClaimDelayedOutputTx(delayedOutputTx: Transaction, localRevocationPubkey: PublicKey, toLocalDelay: Int,
                               remoteDelayedPaymentPubkey: PublicKey, localFinalScriptPubKey: BinaryData,
                               feeratePerKw: Long): ClaimDelayedOutputTx = {

    val redeem = toLocalDelayed(localRevocationPubkey, toLocalDelay, remoteDelayedPaymentPubkey)
    makeClaimHtlcTx(ClaimDelayedOutputTx, delayedOutputTx, redeem, Script pay2wsh redeem,
      localFinalScriptPubKey, weight2fee(feeratePerKw, claimHtlcDelayedWeight),
      expiry = 0L, sequence = toLocalDelay)
  }

  def makeMainPenaltyTx(commitTx: Transaction, remoteRevocationPubkey: PublicKey, localFinalScriptPubKey: BinaryData,
                        toRemoteDelay: Int, remoteDelayedPaymentPubkey: PublicKey, feeratePerKw: Long): MainPenaltyTx = {

    val redeem = toLocalDelayed(remoteRevocationPubkey, toRemoteDelay, remoteDelayedPaymentPubkey)
    makeClaimHtlcTx(MainPenaltyTx, commitTx, redeem, Script pay2wsh redeem, localFinalScriptPubKey,
      weight2fee(feeratePerKw, mainPenaltyWeight), expiry = 0L, sequence = 0xffffffffL)
  }
}