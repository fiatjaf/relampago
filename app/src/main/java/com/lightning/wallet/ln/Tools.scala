package com.lightning.wallet.ln

import com.lightning.wallet.ln.Tools.wrap
import language.implicitConversions
import fr.acinq.bitcoin.BinaryData
import crypto.RandomGenerator
import java.util


object SET {
  // Matching sets as algebraic data structures
  def unapplySeq[T](s: Set[T] /* got a set */) = Some(s.toSeq)
}

object \ {
  // Matching Tuple2 via arrows with much less noise
  def unapply[A, B](t2: (A, B) /* got a tuple */) = Some(t2)
}

object Tools {
  type Bytes = Array[Byte]
  val random = new RandomGenerator
  def runAnd[T](result: T)(action: Any): T = result
  def errlog(error: Throwable): Unit = error.printStackTrace
  def log(message: String): Unit = android.util.Log.d("LN", message)
  def wrap(run: => Unit)(go: => Unit) = try go catch none finally run
  def none: PartialFunction[Any, Unit] = { case _ => }

  def fromShortId(id: Long): (Int, Int, Int) = {
    val blockNumber = id.>>(40).&(0xFFFFFF).toInt
    val txOrd = id.>>(16).&(0xFFFFFF).toInt
    val outOrd = id.&(0xFFFF).toInt
    (blockNumber, txOrd, outOrd)
  }

  def toLongId(fundingHash: BinaryData, fundingOutputIndex: Int): BinaryData =
    if (fundingOutputIndex >= 65536 | fundingHash.size != 32) throw new LightningException
    else fundingHash.take(30) :+ fundingHash.data(30).^(fundingOutputIndex >> 8).toByte :+
      fundingHash.data(31).^(fundingOutputIndex).toByte
}

object Features {
  val OPTION_DATA_LOSS_PROTECT_OPTIONAL = 1
  val INITIAL_ROUTING_SYNC_BIT_OPTIONAL = 3

  implicit def binData2BitSet(data: BinaryData): util.BitSet = util.BitSet valueOf data.reverse.toArray
  def initialRoutingSync(bitset: util.BitSet): Boolean = bitset get INITIAL_ROUTING_SYNC_BIT_OPTIONAL
  def dataLossProtect(bitset: util.BitSet): Boolean = bitset get OPTION_DATA_LOSS_PROTECT_OPTIONAL
  def areSupported(bitset: util.BitSet): Boolean = !(0 until bitset.length by 2 exists bitset.get)
}

case class CMDAddExcept(rpi: RuntimePaymentInfo, code: Int) extends CMDException
case class CMDReserveExcept(rpi: RuntimePaymentInfo, missingSat: Long, reserveSat: Long) extends CMDException
class LightningException(reason: String = "Generic Lightning exception") extends RuntimeException(reason)
trait CMDException extends LightningException { val rpi: RuntimePaymentInfo }

// STATE MACHINE

abstract class StateMachine[T] {
  def become(freshData: T, freshState: String) =
    wrap { data = freshData } { state = freshState }

  def doProcess(change: Any)
  var state: String = _
  var data: T = _
}