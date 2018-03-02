package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._

import rx.lang.scala.{Scheduler, Observable => Obs}
import org.bitcoinj.core.Transaction.DEFAULT_TX_FEE
import com.lightning.wallet.AbstractKit
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Coin
import spray.json.JsonFormat
import scala.util.Try


object JsonHttpUtils {
  def initDelay[T](next: Obs[T], startMillis: Long, timeoutMillis: Long) = {
    val adjustedTimeout = startMillis + timeoutMillis - System.currentTimeMillis
    val delayLeft = if (adjustedTimeout < 0) 0 else adjustedTimeout
    Obs.just(null).delay(delayLeft.millis).flatMap(_ => next)
  }

  def obsOn[T](provider: => T, scheduler: Scheduler) =
    Obs.just(null).subscribeOn(scheduler).map(_ => provider)

  def retry[T](obs: Obs[T], pick: (Throwable, Int) => Duration, times: Range) =
    obs.retryWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)

  def to[T : JsonFormat](raw: String): T = raw.parseJson.convertTo[T]
  def pickInc(error: Throwable, next: Int) = next.seconds
}

object RatesSaver {
  type Fiat2Btc = Map[String, Double]
  type BlockNum2Fee = Map[String, Double]
  type Result = (BlockNum2Fee, Fiat2Btc)

  var rates = {
    val raw = app.prefs.getString(AbstractKit.RATES_DATA, new String)
    Try(raw) map to[Rates] getOrElse Rates(Nil, Map.empty, 0)
  }

  def safe = retry(LNParams.cloud.connector.ask[Result]("rates/get"), pickInc, 3 to 4)
  def initialize = initDelay(safe, rates.stamp, 1800000) foreach { case newFee \ newFiat =>
    val sensibleFees = for (notZero <- newFee("6") +: rates.feeHistory if notZero > 0) yield notZero
    rates = Rates(feeHistory = sensibleFees take 3, exchange = newFiat, stamp = System.currentTimeMillis)
    app.prefs.edit.putString(AbstractKit.RATES_DATA, rates.toJson.toString).commit
  }
}

import com.lightning.wallet.lnutils.RatesSaver.Fiat2Btc
case class Rates(feeHistory: Seq[Double], exchange: Fiat2Btc, stamp: Long) {
  lazy val feeMean: Coin = btcBigDecimal2MSat(feeHistory.sum / feeHistory.size)
  lazy val feeLive = if (feeHistory.isEmpty) DEFAULT_TX_FEE else feeMean
}