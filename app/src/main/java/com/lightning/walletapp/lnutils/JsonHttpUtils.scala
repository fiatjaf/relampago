package com.lightning.walletapp.lnutils

import spray.json._
import scala.concurrent.duration._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Denomination._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._

import rx.lang.scala.{Observable => Obs}
import org.bitcoinj.core.{Coin, PeerAddress}
import org.bitcoinj.core.Transaction.DEFAULT_TX_FEE
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.walletapp.AbstractKit
import com.lightning.walletapp.Utils.app
import spray.json.JsonFormat
import java.net.InetAddress
import scala.util.Try


object JsonHttpUtils {
  val minAllowedFee = Coin valueOf 5000L
  def initDelay[T](next: Obs[T], startMillis: Long, timeoutMillis: Long) = {
    val adjustedTimeout = startMillis + timeoutMillis - System.currentTimeMillis
    val delayLeft = if (adjustedTimeout < 0L) 0L else adjustedTimeout
    Obs.just(null).delay(delayLeft.millis).flatMap(_ => next)
  }

  def repeat[T](obs: Obs[T], pick: (Unit, Int) => Duration, times: Range) =
    obs.repeatWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)

  def retry[T](obs: Obs[T], pick: (Throwable, Int) => Duration, times: Range) =
    obs.retryWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)

  def obsOnIO = Obs just null subscribeOn IOScheduler.apply
  def to[T : JsonFormat](raw: String): T = raw.parseJson.convertTo[T]
  def pickInc(errorOrUnit: Any, next: Int) = next.seconds
}

object RatesSaver {
  private[this] val raw = app.prefs.getString(AbstractKit.RATES_DATA, new String)
  var rates = Try(raw) map to[Rates] getOrElse Rates(Nil, Nil, Map.empty, 0L)

  private[this] val process: PartialFunction[Result, Unit] = { case newFee \ newFiat =>
    val sensibleThree = for (notZero <- newFee("3") +: rates.feesThree if notZero > 0) yield notZero
    val sensibleSix = for (notZero <- newFee("6") +: rates.feesSix if notZero > 0) yield notZero
    rates = Rates(sensibleSix take 2, sensibleThree take 2, newFiat, System.currentTimeMillis)
    app.prefs.edit.putString(AbstractKit.RATES_DATA, rates.toJson.toString).commit
    // Channels may become open sooner then we get updated fees
    // so inform channels again once we get updated fees
    LNParams.updateFeerate
  }

  lazy val subscription =
    // Can later be cancelled if don't want updates
    initDelay(retry(app.olympus.getRates, pickInc, 3 to 4),
      rates.stamp, 60 * 30 * 1000).subscribe(process, Tools.none)
}

// 2 items of memory to help eliminate possible fee spikes
// feesSix is used for manual txs sending, feeThree is used to maintain a channel feerate
case class Rates(feesSix: Seq[Double], feesThree: Seq[Double], exchange: Fiat2Btc, stamp: Long) {
  private[this] val avgThree: Coin = if (feesThree.isEmpty) DEFAULT_TX_FEE else btcBigDecimal2MSat(feesThree.sum / feesThree.size)
  private[this] val avgSix: Coin = if (feesSix.isEmpty) DEFAULT_TX_FEE else btcBigDecimal2MSat(feesSix.sum / feesSix.size)
  val feeThree = if (avgThree isLessThan minAllowedFee) minAllowedFee else avgThree
  val feeSix = if (avgSix isLessThan minAllowedFee) minAllowedFee else avgSix
}

object TopNodes {
  private[this] val api = "https://bitnodes.earn.com/api/v1/nodes/leaderboard?limit=50"
  private[this] val raw = app.prefs.getString(AbstractKit.TOP_NODES, new String)
  var top = Try(raw) map to[TopNodes] getOrElse TopNodes(Vector.empty, 0L)

  private[this] val process: String => Unit = res => {
    val nodes = res.parseJson.asJsObject.fields("results").asInstanceOf[JsArray].elements.map(_.asJsObject fields "node")
    top = TopNodes(nodes = nodes.map(json2String).filterNot(_ contains "::"), stamp = System.currentTimeMillis)
    app.prefs.edit.putString(AbstractKit.TOP_NODES, top.toJson.toString).commit
  }

  def randomPeerAddress = {
    // May throw if no nodes are present
    val randNode = top.nodes(Tools.random nextInt top.nodes.size)
    val host \ port = randNode.splitAt(randNode lastIndexOf ':')
    val h1 \ p1 = (InetAddress getByName host, port.tail.toInt)
    new PeerAddress(app.params, h1, p1)
  }

  lazy val subscription =
    // Can later be cancelled if don't want updates
    initDelay(obsOnIO.map(_ => get(api, true).trustAllCerts.trustAllHosts.body),
      top.stamp, 60 * 60 * 24 * 7 * 1000L).foreach(process, Tools.none)
}

// Top nodes are updated once per week, have ipv4 only
case class TopNodes(nodes: StringVec, stamp: Long)