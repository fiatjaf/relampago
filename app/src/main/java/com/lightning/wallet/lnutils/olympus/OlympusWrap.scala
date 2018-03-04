package com.lightning.wallet.lnutils.olympus

import spray.json._
import com.lightning.wallet.ln.LNParams._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.olympus.OlympusWrap._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import com.lightning.wallet.ln.{PaymentRequest, RoutingData}
import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}

import com.lightning.wallet.ln.RoutingInfoTag.PaymentRouteVec
import scala.collection.JavaConverters.mapAsJavaMapConverter
import com.lightning.wallet.ln.wire.NodeAnnouncement
import com.lightning.wallet.lnutils.OlympusTable
import com.lightning.wallet.helper.RichCursor
import rx.lang.scala.schedulers.IOScheduler
import fr.acinq.bitcoin.Crypto.PublicKey
import java.net.ProtocolException
import java.math.BigInteger


case class OlympusServer(identifier: String, url: String, data: CloudData, auth: Int, removable: Int)
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
  type TxSeq = Seq[Transaction]
  type CloudVec = Vector[Cloud]

  // BTC and fiat rates
  type Fiat2Btc = Map[String, Double]
  type BlockNum2Fee = Map[String, Double]
  type Result = (BlockNum2Fee, Fiat2Btc)
  val CMDStart = "CMDStart"
  val BODY = "body"

  var clouds = {
    // All clouds for basic RPC queries
    val cursor = db select OlympusTable.selectAllSql
    RichCursor(cursor) vec toServer map toInstance
  }

  def doProcess(data: Any) = {
    val backups = clouds.filter(_.useAuth)
    for (cloud <- backups) cloud doProcess data
  }

  // SQL interface

  def remove(identifier: String) = db.change(OlympusTable.killSql, identifier)
  def updData(data: String, identifier: String) = db.change(OlympusTable.updDataSql, data, identifier)
  def updOrder(order: Int, identifier: String) = db.change(OlympusTable.updOrderSql, order, identifier)
  def updMeta(osr: OlympusServer) = db.change(OlympusTable.updMetaSql, osr.url, osr.auth, osr.identifier)
  def toServer(rc: RichCursor) = OlympusServer(rc string OlympusTable.identifier, rc string OlympusTable.url,
    to[CloudData](rc string OlympusTable.data), rc int OlympusTable.auth, rc int OlympusTable.removable)

  def addServer(osr: OlympusServer) =
    db.change(OlympusTable.newSql, osr.identifier, osr.url,
      osr.data.toJson.toString, osr.auth, osr.removable)

  def toInstance(osr: OlympusServer) =
    new Cloud(osr.auth == 1, new Connector(osr.url),
      osr.identifier) { this.data = osr.data }

  // Olympus RPC interface

  def failOver[T](run: Cloud => Obs[T], clouds: CloudVec): Obs[T] = {
    def tryAgainWithNextCloud(failure: Throwable) = failOver(run, clouds.tail)
    if (clouds.isEmpty) Obs error new ProtocolException("Run out of clouds")
    else run(clouds.head) onErrorResumeNext tryAgainWithNextCloud
  }

  def getBackup(key: BinaryData) = {
    def empty(failure: Throwable) = Vector.empty[String]
    // Special case: we need to query all the available clouds at once
    Obs.from(clouds).flatMap(_.connector getBackup key onErrorReturn empty)
  }

  def getRates = failOver(_.connector.getRates, clouds)
  def getBlock(hash: String) = failOver(_.connector getBlock hash, clouds)
  def findNodes(query: String) = failOver(_.connector findNodes query, clouds)
  def getChildTxs(txs: TxSeq) = failOver(_.connector getChildTxs txs, clouds)
  def findRoutes(rd: RoutingData, from: Set[PublicKey], to: PublicKey) =
    failOver(_.connector.findRoutes(rd, from, to), clouds)
}

trait OlympusProvider {
  def findRoutes(rd: RoutingData, from: Set[PublicKey],
                 to: PublicKey): Obs[PaymentRouteVec]

  def findNodes(query: String): Obs[AnnounceChansNumVec]
  def getBlock(hash: String): Obs[BlockHeightAndTxs]
  def getBackup(key: BinaryData): Obs[StringVec]
  def getChildTxs(txs: TxSeq): Obs[TxSeq]
  def getRates: Obs[Result]
}

class Connector(val url: String) extends OlympusProvider {
  def http(way: String) = post(s"$url/$way", true) connectTimeout 15000
  def ask[Type: JsonFormat](command: String, params: HttpParam*): Obs[Type] =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: response +: _) => response.convertTo[Type]
      case _ => throw new ProtocolException
    }

  def getRates = ask[Result]("rates/get")
  def getBlock(hash: String) = ask[BlockHeightAndTxs]("block/get", "hash" -> hash)
  def getBackup(key: BinaryData) = ask[StringVec]("data/get", "key" -> key.toString)
  def findNodes(query: String) = ask[AnnounceChansNumVec]("router/nodes", "query" -> query)
  def getChildTxs(txs: TxSeq) = ask[TxSeq]("txs/get", "txids" -> txs.map(_.txid).toJson.toString.hex)
  def findRoutes(rd: RoutingData, from: Set[PublicKey], to: PublicKey) = ask[PaymentRouteVec]("router/routes",
    "froms" -> from.map(_.toBin).toJson.toString.hex, "tos" -> Set(to).map(_.toBin).toJson.toString.hex,
    "xn" -> rd.badNodes.map(_.toBin).toJson.toString.hex, "xc" -> rd.badChans.toJson.toString.hex)
}