package com.lightning.walletapp.lnutils

import spray.json._
import scala.concurrent.duration._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import rx.lang.scala.{Observable => Obs}

import com.lightning.walletapp.lnutils.olympus.OlympusWrap.Fiat2Btc
import org.bitcoinj.core.Transaction.DEFAULT_TX_FEE
import com.lightning.walletapp.ChannelManager
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.walletapp.AbstractKit
import com.lightning.walletapp.Utils.app
import org.bitcoinj.core.Coin
import spray.json.JsonFormat
import scala.util.Try


object JsonHttpUtils {
  val minAllowedFee = Coin valueOf 5000L
  def initDelay[T](next: Obs[T], startMillis: Long, timeoutMillis: Long) = {
    val adjustedTimeout = startMillis + timeoutMillis - System.currentTimeMillis
    val delayLeft = if (adjustedTimeout < 0L) 0L else adjustedTimeout
    Obs.just(null).delay(delayLeft.millis).flatMap(_ => next)
  }

  def obsOnIO = Obs just null subscribeOn IOScheduler.apply
  def repeat[T](obs: Obs[T], pick: (Unit, Int) => Duration, times: Range) =
    obs.repeatWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)

  def retry[T](obs: Obs[T], pick: (Throwable, Int) => Duration, times: Range) =
    obs.retryWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)

  def to[T : JsonFormat](raw: String): T = raw.parseJson.convertTo[T]
  def pickInc(errorOrUnit: Any, next: Int) = next.seconds
}

object RatesSaver {
  private[this] val raw = app.prefs.getString(AbstractKit.RATES_DATA, new String)
  var rates = Try(raw) map to[Rates] getOrElse Rates(Nil, Nil, Map.empty, 0)
  var onUpdated = Set.empty[Runnable]

  private[this] def safe = retry(app.olympus.getRates, pickInc, 3 to 4)
  def initialize = initDelay(safe, rates.stamp, 1800000) foreach { case newFee \ newFiat =>
    val sensibleThree = for (notZero <- newFee("3") +: rates.feesThree if notZero > 0) yield notZero
    val sensibleSix = for (notZero <- newFee("6") +: rates.feesSix if notZero > 0) yield notZero

    // Channels may become open sooner then we get updated fees so inform channels once we get updated fees
    for (c <- ChannelManager.notClosingOrRefunding) c process CMDFeerate(LNParams.broadcaster.perKwThreeSat)
    rates = Rates(sensibleSix take 2, sensibleThree take 2, newFiat, System.currentTimeMillis)
    app.prefs.edit.putString(AbstractKit.RATES_DATA, rates.toJson.toString).commit
    for (runner <- onUpdated) runner.run
  }
}

// 2 items of memory to help eliminate possible fee spikes
// feesSix is used for manual txs sending, feeThree is used to maintain a channel feerate
case class Rates(feesSix: Seq[Double], feesThree: Seq[Double], exchange: Fiat2Btc, stamp: Long) {
  private[this] val avgThree: Coin = if (feesThree.isEmpty) DEFAULT_TX_FEE else btcBigDecimal2MSat(feesThree.sum / 2)
  private[this] val avgSix: Coin = if (feesSix.isEmpty) DEFAULT_TX_FEE else btcBigDecimal2MSat(feesSix.sum / 2)
  val feeThree = if (avgThree isLessThan minAllowedFee) minAllowedFee else avgThree
  val feeSix = if (avgSix isLessThan minAllowedFee) minAllowedFee else avgSix
}