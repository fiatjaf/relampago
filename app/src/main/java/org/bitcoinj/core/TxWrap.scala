package org.bitcoinj.core

import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import scala.collection.JavaConverters._
import org.bitcoinj.wallet.WalletTransaction.Pool._
import com.lightning.wallet.{AddrData, P2WSHData}
import scala.util.Try


object TxWrap {
  val HIDE = "HIDE"
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

  // valueDelta being zero means this is a watched transaction
  // valueWithoutFee being zero means this is a to-itself transaction

  val fee = Option(tx.getFee)
  val valueDelta = nativeSentToMe subtract nativeSentFromMe
  val valueWithoutFee = fee map valueDelta.add getOrElse valueDelta
  val visibleValue = if (valueWithoutFee.isZero) nativeSentToMe else valueWithoutFee

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
}