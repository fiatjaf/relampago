package com.lightning.walletapp.ln

import com.lightning.walletapp.ln.Tools.runAnd
import fr.acinq.bitcoin.Crypto.PrivateKey
import language.implicitConversions
import fr.acinq.bitcoin.BinaryData
import crypto.RandomGenerator
import java.util


object \ {
  // Matching Tuple2 via arrows with much less noise
  def unapply[A, B](t2: (A, B) /* Got a tuple */) = Some(t2)
}

object Tools {
  type UserId = String
  type Bytes = Array[Byte]
  val random = new RandomGenerator
  def runAnd[T](result: T)(action: Any): T = result
  def bin2readable(bin: Bytes) = new String(bin, "UTF-8")
  def errlog(error: Throwable): Unit = error.printStackTrace
  def log(message: String): Unit = android.util.Log.d("LN", message)
  def randomPrivKey = PrivateKey(random getBytes 32, compressed = true)
  def wrap(run: => Unit)(go: => Unit) = try go catch none finally run
  def none: PartialFunction[Any, Unit] = { case _ => }

  def fromShortId(id: Long) = {
    val blockHeight = id.>>(40).&(0xFFFFFF).toInt
    val txIndex = id.>>(16).&(0xFFFFFF).toInt
    val outputIndex = id.&(0xFFFF).toInt
    (blockHeight, txIndex, outputIndex)
  }

  def toShortIdOpt(blockHeight: Long, txIndex: Long, outputIndex: Long): Option[Long] = {
    val result = blockHeight.&(0xFFFFFFL).<<(40) | txIndex.&(0xFFFFFFL).<<(16) | outputIndex.&(0xFFFFL)
    if (txIndex < 0) None else Some(result)
  }

  def toLongId(fundingHash: BinaryData, fundingOutputIndex: Int) =
    if (fundingOutputIndex >= 65536 | fundingHash.size != 32) throw new LightningException("Funding index > 65535 or funding hash != 32")
    else fundingHash.take(30) :+ fundingHash.data(30).^(fundingOutputIndex >> 8).toByte :+ fundingHash.data(31).^(fundingOutputIndex).toByte
}

object Features {
  val OPTION_DATA_LOSS_PROTECT_MANDATORY = 0
  val OPTION_DATA_LOSS_PROTECT_OPTIONAL = 1

  implicit def binData2BitSet(data: BinaryData): util.BitSet = util.BitSet valueOf data.reverse.toArray
  def areSupported(bitset: util.BitSet) = !(0 until bitset.length by 2 exists bitset.get)

  def dataLossProtect(bitset: util.BitSet) =
    bitset.get(OPTION_DATA_LOSS_PROTECT_OPTIONAL) ||
      bitset.get(OPTION_DATA_LOSS_PROTECT_MANDATORY)
}

class LightningException(reason: String = "Failure") extends RuntimeException(reason)
case class HTLCExpiryException(norm: NormalData, htlc: Htlc) extends LightningException
case class CMDAddImpossible(rd: RoutingData, code: Int) extends LightningException

// STATE MACHINE

abstract class StateMachine[T] {
  def become(freshData: T, freshState: String) =
    runAnd { data = freshData } { state = freshState }

  def doProcess(change: Any)
  var state: String = _
  var data: T = _
}