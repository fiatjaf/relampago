package org.bitcoinj.core

import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import scala.collection.JavaConverters._
import org.bitcoinj.wallet.WalletTransaction.Pool._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import org.bitcoinj.wallet.SendRequest
import scodec.bits.ByteVector
import scala.util.Try


// Holds an unsigned channel funding tx with dummy pubkeyScript
case class Batch(unsigned: SendRequest, dummyScript: ByteVector, pr: PaymentRequest) {
  val fundOutIdx = new PubKeyScriptIndexFinder(unsigned.tx).findPubKeyScriptIndex(dummyScript)
  val fundingAmountSat = unsigned.tx.getOutput(fundOutIdx).getValue.value

  def replaceDummy(realScript: ByteVector) = {
    val realOut = new TransactionOutput(app.params, null, Coin valueOf fundingAmountSat, realScript.toArray)
    val withReplacedDummy = unsigned.tx.getOutputs.asScala.patch(fundOutIdx, List(realOut), replaced = 1)

    unsigned.tx.clearOutputs
    // First remove all existing outs, then fill in updated
    for (out <- withReplacedDummy) unsigned.tx addOutput out
    unsigned
  }
}

class TxWrap(val tx: Transaction) {
  private val nativeSentFromMe = tx.getInputs.asScala.flatMap(inOuts).foldLeft(Coin.ZERO) {
    case accumulator \ output if output isMine app.kit.wallet => accumulator add output.getValue
    case accumulator \ _ => accumulator
  }

  private val nativeSentToMe = tx.getOutputs.asScala.foldLeft(Coin.ZERO) {
    case accumulator \ out if out isMine app.kit.wallet => accumulator add out.getValue
    case accumulator \ _ => accumulator
  }

  val fee = Option(tx.getFee)
  val valueDelta = nativeSentToMe subtract nativeSentFromMe
  val valueWithoutFee = fee map valueDelta.add getOrElse valueDelta

  val visibleValue =
    if (valueDelta.isPositive) valueDelta // This is an incoming tx, we don't care about fee
    else if (valueWithoutFee.isZero) nativeSentToMe // This is a to-itself transaction, hide the fee
    else valueWithoutFee // This is an outgoing tx, subtract the fee

  def directedScriptPubKeysWithValueTry(incoming: Boolean) = tx.getOutputs.asScala collect {
    case out if out.isMine(app.kit.wallet) == incoming => Try(out.getScriptPubKey -> out.getValue)
  }

  private def inOuts(input: TransactionInput): Option[TransactionOutput] =
    Stream(UNSPENT, SPENT, PENDING).map(app.kit.wallet.getTransactionPool)
      .map(input.getConnectedOutput).find(_ != null)

  def isVisible = tx.getMemo != HIDE && !valueDelta.isZero
  def makeHidden = tx setMemo HIDE
  final val HIDE = "HIDE"
}