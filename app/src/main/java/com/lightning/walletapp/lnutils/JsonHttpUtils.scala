package com.lightning.walletapp.lnutils

import spray.json._
import scala.concurrent.duration._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import rx.lang.scala.{Observable => Obs}

import com.lightning.walletapp.lnutils.olympus.OlympusWrap.Fiat2Btc
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import org.bitcoinj.core.Transaction.DEFAULT_TX_FEE
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.walletapp.AbstractKit
import com.lightning.walletapp.Utils.app
import org.bitcoinj.core.Coin
import spray.json.JsonFormat
import scala.util.Try


object JsonHttpUtils {
  val minAllowedFee = Coin valueOf 100000L // TODO: 5000L
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
  def safe = retry(OlympusWrap.getRates, pickInc, 3 to 4)
  def initialize = initDelay(safe, rates.stamp, 1800000) foreach { case newFee \ newFiat =>
    val sensibleSix = for (notZero <- newFee("6") +: rates.feesSix if notZero > 0) yield notZero
    val sensibleTwo = for (notZero <- newFee("2") +: rates.feesTwo if notZero > 0) yield notZero

    // Channels may become open sooner then we get updated fees so inform channels once we get updated fees
    rates = Rates(sensibleSix take 2, sensibleTwo take 2, exchange = newFiat, stamp = System.currentTimeMillis)
    for (c <- app.ChannelManager.notClosingOrRefunding) c process CMDFeerate(LNParams.broadcaster.perKwTwoSat)
    app.prefs.edit.putString(AbstractKit.RATES_DATA, rates.toJson.toString).commit
  }

  var rates = {
    val raw = app.prefs.getString(AbstractKit.RATES_DATA, new String)
    Try(raw) map to[Rates] getOrElse Rates(Nil, Nil, Map.empty, 0)
  }
}

// 2 items of memory to help eliminate possible fee spikes
// feesSix is used for manual txs sending, feesTwo is used to maintain a channel feerate
case class Rates(feesSix: Seq[Double], feesTwo: Seq[Double], exchange: Fiat2Btc, stamp: Long) {
  val avgSix: Coin = if (feesSix.isEmpty) DEFAULT_TX_FEE else btcBigDecimal2MSat(feesSix.sum / 2)
  val avgTwo: Coin = if (feesTwo.isEmpty) DEFAULT_TX_FEE else btcBigDecimal2MSat(feesTwo.sum / 2)
  val feeSix = if (avgSix isLessThan minAllowedFee) minAllowedFee else avgSix
  val feeTwo = if (avgTwo isLessThan minAllowedFee) minAllowedFee else avgTwo
}