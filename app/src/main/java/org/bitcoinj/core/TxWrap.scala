package org.bitcoinj.core

import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import org.bitcoinj.wallet.SendRequest._
import scala.collection.JavaConverters._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Scripts._
import com.lightning.walletapp.Denomination._
import org.bitcoinj.wallet.WalletTransaction.Pool._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import scala.util.{Success, Try}
import fr.acinq.bitcoin.{BinaryData, Satoshi}
import com.lightning.walletapp.Denomination.mSat2Coin
import com.lightning.walletapp.lnutils.RatesSaver
import org.bitcoinj.script.ScriptBuilder
import org.bitcoinj.wallet.SendRequest


// Holds an unsigned channel funding tx with dummy pubkeyScript
case class Batch(unsigned: SendRequest, dummyScript: BinaryData, pr: PaymentRequest) {
  val fundOutIdx = new PubKeyScriptIndexFinder(unsigned.tx).findPubKeyScriptIndex(dummyScript)
  val fundingAmountSat = unsigned.tx.getOutput(fundOutIdx).getValue.value

  def replaceDummy(realScript: BinaryData) = {
    val realOut = new TransactionOutput(app.params, null, Coin valueOf fundingAmountSat, realScript)
    val withReplacedDummy = unsigned.tx.getOutputs.asScala.patch(fundOutIdx, List(realOut), replaced = 1)

    unsigned.tx.clearOutputs
    // First remove all existing outs, then fill in updated
    for (out <- withReplacedDummy) unsigned.tx addOutput out
    unsigned
  }

  def asString(source: Int) = {
    val base = app getString source
    val info = getDescription(pr.description)
    val onchainSum = coloredOut apply pr.amount.get
    val onchainFee = coloredOut apply unsigned.tx.getFee
    val channelSum = coloredChan apply Satoshi(fundingAmountSat)
    base.format(info, onchainSum, channelSum, onchainFee).html
  }
}

object TxWrap {
  def findBestBatch(pr: PaymentRequest) = Try {
    // Any of these two might throw and thus work as guards
    val where = Address.fromString(app.params, pr.fallbackAddress.get)
    val sum = mSat2Coin(pr.amount.get)

    val dummyScript = pubKeyScript(randomPrivKey.publicKey, randomPrivKey.publicKey)
    val addrScript = ScriptBuilder.createOutputScript(where).getProgram
    val emptyThreshold = Coin.valueOf(LNParams.minCapacitySat * 2)
    val suggestedChanSum = Coin.valueOf(5000000L)
    val totalBalance = app.kit.conf1Balance

    val candidates = for (idx <- 0 to 10) yield Try {
      // Try out a number of amounts to determine the largest change
      val increase = sum add Coin.valueOf(LNParams.minCapacitySat * idx)
      val shouldEmpty = totalBalance minus increase isLessThan emptyThreshold
      val req = if (shouldEmpty) emptyWallet(where) else to(where, increase)

      req.feePerKb = RatesSaver.rates.feeSix
      app.kit.wallet addLocalInputsToTx req
      req
    }

    val corrected = candidates collect {
      case Success(req) if req.tx.getOutputs.size == 1 =>
        // Tx has only one output, this means it empties a wallet
        // channel amount is total sum subtracted from requested sum
        val channelSat = req.tx.getOutput(0).getValue.minus(sum)

        req.tx.clearOutputs
        req.tx.addOutput(sum, where)
        req.tx.addOutput(channelSat, dummyScript)
        channelSat -> req

      case Success(req) if req.tx.getOutputs.size == 2 =>
        // Tx has two outputs so there is some change which will be used for channel
        // Depending on whether change is below max chan size we return it as is or adjusted down
        val payee \ change = req.tx.getOutputs.asScala.partition(_.getScriptBytes sameElements addrScript)
        // Payee sum may have an excessive amount which should be added to a change sum
        val realChangeSat = change.head.getValue.plus(payee.head.getValue minus sum)

        if (realChangeSat.value > LNParams.maxCapacity.amount) {
          // Change amount exceeds max chan capacity so lower it down
          val reducedChangeSum = realChangeSat.minus(suggestedChanSum)

          req.tx.clearOutputs
          req.tx.addOutput(sum, where)
          req.tx.addOutput(suggestedChanSum, dummyScript)
          // Add a real change output with subtracted channel capacity
          req.tx.addOutput(reducedChangeSum, change.head.getScriptPubKey)
          suggestedChanSum -> req

        } else {
          req.tx.clearOutputs
          req.tx.addOutput(sum, where)
          // Change becomes a channel capacity here
          req.tx.addOutput(realChangeSat, dummyScript)
          realChangeSat -> req
        }
    }

    // It may fail here because after filtering we may have no items at all
    val filtered = corrected filter { case amount \ _ => amount.value > LNParams.minCapacitySat }
    val _ \ finalRequest = filtered maxBy { case bestAmount \ _ => bestAmount.value }
    Batch(finalRequest, dummyScript, pr)
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