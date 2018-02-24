package org.bitcoinj.core

import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import scala.collection.JavaConverters._
import com.lightning.wallet.{AddrData, P2WSHData}
import org.bitcoinj.wallet.WalletTransaction.Pool.{UNSPENT, SPENT, PENDING}
import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import scala.util.Try


object TxWrap {
  val HIDE = "HIDE"
  def watchedWrap(wrap: TxWrap) = wrap.valueDelta.isZero
  def hiddenWrap(wrap: TxWrap) = wrap.tx.getMemo == HIDE
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
    else valueWithoutFee // This is an outgoing tx, remove the fee part

  // Depending on whether this is an incoming or outgoing transaction
  // we collect either outputs which belong to us or the foreign ones

  def payDatas(incoming: Boolean) =
    tx.getOutputs.asScala filter { out =>
      out.isMine(app.kit.wallet) == incoming
    } map outputToPayData

  private def outputToPayData(out: TransactionOutput) = Try(out.getScriptPubKey) map {
    case publicKeyScript if publicKeyScript.isSentToP2WSH => P2WSHData(out.getValue, publicKeyScript)
    case publicKeyScript => AddrData(out.getValue, publicKeyScript getToAddress app.params)
  }

  private def inOuts(input: TransactionInput): Option[TransactionOutput] =
    Stream(UNSPENT, SPENT, PENDING).map(app.kit.wallet.getTransactionPool)
      .map(input.getConnectedOutput).find(_ != null)

  def depth = tx.getConfidence.getDepthInBlocks
  def isDead = tx.getConfidence.getConfidenceType == DEAD
}