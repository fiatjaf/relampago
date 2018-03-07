package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import rx.lang.scala.{Scheduler, Observable => Obs}

import com.lightning.wallet.lnutils.olympus.OlympusWrap.Fiat2Btc
import com.lightning.wallet.lnutils.olympus.OlympusWrap
import com.lightning.wallet.AbstractKit
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Coin
import spray.json.JsonFormat
import scala.util.Try


object JsonHttpUtils {
  def initDelay[T](next: Obs[T], startMillis: Long, timeoutMillis: Long) = {
    val adjustedTimeout = startMillis + timeoutMillis - System.currentTimeMillis
    val delayLeft = if (adjustedTimeout < 0L) 0L else adjustedTimeout
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
  def safe = retry(OlympusWrap.getRates, pickInc, 3 to 4)
  def initialize = initDelay(safe, rates.stamp, 1800000) foreach { case newFee \ newFiat =>
    val sensibleFees = for (notZero <- newFee("6") +: rates.feeHistory if notZero > 0) yield notZero
    rates = Rates(feeHistory = sensibleFees take 3, exchange = newFiat, stamp = System.currentTimeMillis)
    app.prefs.edit.putString(AbstractKit.RATES_DATA, rates.toJson.toString).commit
  }

  var rates = {
    val raw = app.prefs.getString(AbstractKit.RATES_DATA, new String)
    Try(raw) map to[Rates] getOrElse Rates(Nil, Map.empty, 0)
  }
}

case class Rates(feeHistory: Seq[Double], exchange: Fiat2Btc, stamp: Long) {
  // Bitcoin Core provides unreliable fees in testnet so just use default here
  // TODO: remove for mainnet

  private val DEFAULT_TX_FEE = Coin valueOf 50000L
  lazy val feeLive = if (feeHistory.isEmpty) DEFAULT_TX_FEE else {
    val mu: Coin = btcBigDecimal2MSat(feeHistory.sum / feeHistory.size)
    if (mu isLessThan DEFAULT_TX_FEE) DEFAULT_TX_FEE else mu
  }
}