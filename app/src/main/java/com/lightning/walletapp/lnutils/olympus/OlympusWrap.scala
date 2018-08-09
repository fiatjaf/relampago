package com.lightning.walletapp.lnutils.olympus

import spray.json._
import com.lightning.walletapp.ln.LNParams._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import java.math.BigInteger
import java.net.ProtocolException
import com.lightning.walletapp.ln.PaymentRequest
import com.lightning.walletapp.helper.RichCursor
import com.lightning.walletapp.lnutils.OlympusTable
import scala.collection.JavaConverters.mapAsJavaMapConverter
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRouteVec
import com.lightning.walletapp.ln.wire.{NodeAnnouncement, OutRequest}
import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}


case class CloudData(info: Option[RequestAndMemo], tokens: Vector[ClearToken], acts: CloudActVec)
case class CloudAct(data: BinaryData, plus: Seq[HttpParam], path: String)

object OlympusWrap extends OlympusProvider {
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  type AnnounceChansNum = (NodeAnnouncement, Int)
  type ClearToken = (String, String, String)
  type TokensInfo = (String, String, Int)
  type BlockHeightAndTxIdx = (Long, Int)
  type HttpParam = (String, String)

  // Shortcuts for Olympus RPC return data types
  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  type BigIntegerVec = Vector[BigInteger]
  type CloudActVec = Vector[CloudAct]
  type StringVec = Vector[String]
  type CloudVec = Vector[Cloud]

  // Tx request/response
  type BinaryDataSeq = Seq[BinaryData]
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
  def tellClouds(candidateData: Any) = for (cloud <- clouds) cloud doProcess candidateData
  def backupExhausted = clouds.exists(cloud => cloud.isAuthEnabled && cloud.data.tokens.size <= 5)

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
    val stored = to[CloudData](rc string OlympusTable.data)
    val connector = new Connector(rc string OlympusTable.url)
    new Cloud(id, connector, auth, removable) { data = stored }
  }

  // Olympus RPC interface

  def failOver[T](run: Cloud => Obs[T], onRunOut: Obs[T], cs: CloudVec): Obs[T] = {
    def tryAgainWithNextCloud(failure: Throwable) = failOver(run, onRunOut, cs.tail)
    if (cs.isEmpty) onRunOut else run(cs.head) onErrorResumeNext tryAgainWithNextCloud
  }

  def getBackup(key: BinaryData) = {
    def empty(failure: Throwable) = Vector.empty[String]
    // Special case: we need to query all the available clouds at once
    Obs.from(clouds).flatMap(_.connector getBackup key onErrorReturn empty)
  }

  def getRates = failOver(_.connector.getRates, tryLater, clouds)
  def findNodes(query: String) = failOver(_.connector findNodes query, tryLater, clouds)
  def findRoutes(out: OutRequest) = failOver(_.connector findRoutes out, tryLater, clouds)
  def getShortId(txid: BinaryData) = failOver(_.connector getShortId txid, Obs.empty, clouds)
  def getChildTxs(ids: BinaryDataSeq) = failOver(_.connector getChildTxs ids, tryLater, clouds)
  private[this] val tryLater = Obs error new ProtocolException("Try again later")
}

trait OlympusProvider {
  def findRoutes(out: OutRequest): Obs[PaymentRouteVec]
  def findNodes(query: String): Obs[AnnounceChansNumVec]
  def getShortId(txid: BinaryData): Obs[BlockHeightAndTxIdx]
  def getChildTxs(txIds: BinaryDataSeq): Obs[TxSeq]
  def getBackup(key: BinaryData): Obs[StringVec]
  def getRates: Obs[Result]
}

class Connector(val url: String) extends OlympusProvider {
  def ask[T: JsonFormat](commandPath: String, parameters: HttpParam*): Obs[T] =
    obsOnIO.map(_ => http(commandPath).form(parameters.toMap.asJava).body.parseJson) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: response +: _) => response.convertTo[T]
      case _ => throw new ProtocolException
    }

  def getRates = ask[Result]("rates/get")
  def getBackup(key: BinaryData) = ask[StringVec]("data/get", "key" -> key.toString)
  def findNodes(query: String) = ask[AnnounceChansNumVec]("router/nodes", "query" -> query)
  def getShortId(txid: BinaryData) = ask[BlockHeightAndTxIdx]("shortid/get", "txid" -> txid.toString)
  def getChildTxs(txIds: BinaryDataSeq) = ask[TxSeq]("txs/get", "txids" -> txIds.toJson.toString.hex)
  def findRoutes(out: OutRequest) = ask[PaymentRouteVec]("router/routesplus", "params" -> out.toJson.toString.hex)
  def http(requestPath: String) = post(s"$url/$requestPath", true).trustAllCerts.trustAllHosts.connectTimeout(15000)
}