package com.lightning.wallet.lnutils.olympus

import spray.json._
import com.lightning.wallet.ln.LNParams._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.olympus.OlympusWrap._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.RoutingInfoTag.PaymentRouteVec
import scala.collection.JavaConverters.mapAsJavaMapConverter
import com.lightning.wallet.lnutils.OlympusTable
import com.lightning.wallet.ln.PaymentRequest
import com.lightning.wallet.helper.RichCursor
import java.net.ProtocolException
import java.math.BigInteger

import com.lightning.wallet.ln.wire.{NodeAnnouncement, OutRequest}
import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}


case class CloudData(info: Option[RequestAndMemo], tokens: Vector[ClearToken], acts: CloudActVec)
case class CloudAct(data: BinaryData, plus: Seq[HttpParam], path: String)

object OlympusWrap extends OlympusProvider {
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  type AnnounceChansNum = (NodeAnnouncement, Int)
  type ClearToken = (String, String, String)
  type BlockHeightAndTxs = (Long, StringVec)
  type TokensInfo = (String, String, Int)
  type HttpParam = (String, String)

  // Shortcuts for Olympus RPC return data types
  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  type BigIntegerVec = Vector[BigInteger]
  type CloudActVec = Vector[CloudAct]
  type StringVec = Vector[String]
  type CloudVec = Vector[Cloud]
  type TxSeq = Seq[Transaction]

  // BTC and fiat rates
  type Fiat2Btc = Map[String, Double]
  type BlockNum2Fee = Map[String, Double]
  type Result = (BlockNum2Fee, Fiat2Btc)
  val CMDStart = "CMDStart"
  val BODY = "body"

  // All available clouds for RPC queries and backups
  // backup upload requests are also sent to all the clounds
  // and final filtering is done inside of each available cloud
  var clouds = RichCursor(db select OlympusTable.selectAllSql) vec toCloud
  def tellClouds(data: Any) = for (cloud <- clouds) cloud doProcess data

  // SQL interface

  def remove(identifier: String) = db.change(OlympusTable.killSql, identifier)
  def updData(data: String, identifier: String) = db.change(OlympusTable.updDataSql, data, identifier)
  def updMeta(cd: Cloud, order: Int) = db.change(OlympusTable.updMetaSql, cd.connector.url, cd.auth, order, cd.identifier)
  def addServer(cloud: Cloud, order: Int) = db.change(OlympusTable.newSql, cloud.identifier, cloud.connector.url,
    cloud.data.toJson.toString, cloud.auth, order, cloud.removable)

  def toCloud(rc: RichCursor) = {
    val auth = rc int OlympusTable.auth
    val id = rc string OlympusTable.identifier
    val removable = rc int OlympusTable.removable
    val saved = to[CloudData](rc string OlympusTable.data)
    val connector = new Connector(rc string OlympusTable.url)
    new Cloud(id, connector, auth, removable) { data = saved }
  }

  // Olympus RPC interface

  def failOver[T](run: Cloud => Obs[T], cs: CloudVec): Obs[T] = {
    def tryAgainWithNextCloud(failure: Throwable) = failOver(run, cs.tail)
    if (cs.isEmpty) Obs error new ProtocolException("Run out of clouds")
    else run(cs.head) onErrorResumeNext tryAgainWithNextCloud
  }

  def getBackup(key: BinaryData) = {
    def empty(failure: Throwable) = Vector.empty[String]
    // Special case: we need to query all the available clouds at once
    Obs.from(clouds).flatMap(_.connector getBackup key onErrorReturn empty)
  }

  def getRates = failOver(_.connector.getRates, clouds)
  def getBlock(hash: String) = failOver(_.connector getBlock hash, clouds)
  def getChildTxs(txs: TxSeq) = failOver(_.connector getChildTxs txs, clouds)
  def findNodes(query: String) = failOver(_.connector findNodes query, clouds)
  def findRoutes(out: OutRequest) = failOver(_.connector findRoutes out, clouds)
}

trait OlympusProvider {
  def findRoutes(out: OutRequest): Obs[PaymentRouteVec]
  def findNodes(query: String): Obs[AnnounceChansNumVec]
  def getBlock(hash: String): Obs[BlockHeightAndTxs]
  def getBackup(key: BinaryData): Obs[StringVec]
  def getChildTxs(txs: TxSeq): Obs[TxSeq]
  def getRates: Obs[Result]
}

class Connector(val url: String) extends OlympusProvider {
  def http(way: String) = post(s"$url/$way", true) connectTimeout 15000
  def ask[T: JsonFormat](commandPath: String, parameters: HttpParam*): Obs[T] =
    obsOnIO.map(_ => http(commandPath).form(parameters.toMap.asJava).body.parseJson) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: response +: _) => response.convertTo[T]
      case _ => throw new ProtocolException
    }

  def getRates = ask[Result]("rates/get")
  def getBlock(hash: String) = ask[BlockHeightAndTxs]("block/get", "hash" -> hash)
  def getBackup(key: BinaryData) = ask[StringVec]("data/get", "key" -> key.toString)
  def findNodes(query: String) = ask[AnnounceChansNumVec]("router/nodes", "query" -> query)
  def getChildTxs(txs: TxSeq) = ask[TxSeq]("txs/get", "txids" -> txs.map(_.txid).toJson.toString.hex)
  def findRoutes(out: OutRequest) = ask[PaymentRouteVec]("router/routes", "params" -> out.toJson.toString.hex)
}