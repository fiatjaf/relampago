package com.lightning.wallet.lnutils

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.Connector._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import fr.acinq.bitcoin.{BinaryData, Crypto, Transaction}
import rx.lang.scala.{Observable => Obs}
import scala.util.{Failure, Success}

import com.lightning.wallet.ln.wire.LightningMessageCodecs.AnnounceChansNum
import com.lightning.wallet.ln.RoutingInfoTag.PaymentRouteVec
import collection.JavaConverters.mapAsJavaMapConverter
import com.github.kevinsawicki.http.HttpRequest.post
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.ln.Tools.none
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException
import org.bitcoinj.core.ECKey
import java.math.BigInteger


abstract class Cloud extends StateMachine[CloudData] {
  // Persisted data exchange with a maintenance server
  protected[this] var isFree = true
  val connector: Connector

  def getSided[T](obs: Obs[T] /* side effects */) = {
    val observable1 = obs doOnSubscribe { isFree = false }
    observable1 doOnTerminate { isFree = true }
  }

  def BECOME(d1: CloudData) = {
    // Save to db on every update
    CloudDataSaver saveObject d1
    become(d1, state)
  }
}

// Stores token request parameters, clear tokens left and tasks to be executed, also url for custom server
case class CloudData(info: Option[RequestAndMemo], tokens: Set[ClearToken], acts: Set[CloudAct], url: String)
// Represents a remote call to be executed in exchange for token or signature, can be locally persisted
case class CloudAct(data: BinaryData, plus: Seq[HttpParam], path: String)

class PublicCloud(bag: PaymentInfoBag) extends Cloud { me =>
  val connector = new Connector("http://10.0.2.2:9002")
  val maxPrice = 20000000L

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, clearTokens, acts, _) \ CMDStart
      // Info is None AND we are free AND few tokens left AND acts is empty AND have a non-depleted channel
      if isFree && clearTokens.size < 5 && acts.isEmpty && app.ChannelManager.canSend(maxPrice).nonEmpty =>
      // This will intercept the next case only if we have no cloud acts left which is desirable
      me getSided retry(getFreshData, pickInc, 4 to 5) foreach { case rpi \ prm =>
        // If requested sum is low enough and tokens quantity is high enough
        // and info has not already been set by another request
        app.ChannelManager.send(rpi, none)
        me updRequestData Some(prm)
      }

    // Execute if we are free and have available tokens and actions, don't care amout memo here
    case CloudData(_, SET(token @ (point, clear, signature), _*), SET(action, _*), _) \ CMDStart if isFree =>
      val params = Seq("point" -> point, "cleartoken" -> clear, "clearsig" -> signature, BODY -> action.data.toString)
      val send = me getSided connector.ask[String](action.path, params ++ action.plus:_*).doOnCompleted(me doProcess CMDStart)
      send.foreach(onResponse, onResponse)

      def onResponse(response: Any) = response match {
        case "done" => me BECOME data.copy(acts = data.acts - action, tokens = data.tokens - token)
        case err: Throwable if err.getMessage == "tokeninvalid" => me BECOME data.copy(tokens = data.tokens - token)
        case err: Throwable if err.getMessage == "tokenused" => me BECOME data.copy(tokens = data.tokens - token)
        case err: Throwable => Tools errlog err
        case _ =>
      }

    // We do not have any acts or tokens but have a memo
    // which means a payment is in progress so check it's status
    case CloudData(Some(pr \ memo), _, _, _) \ CMDStart if isFree =>
      val inFlight = app.ChannelManager.notClosingOrRefunding.flatMap(inFlightOutgoingHtlcs)
      val tokenRequestInFlight = inFlight.exists(_.add.paymentHash == pr.paymentHash)

      if (!tokenRequestInFlight) {
        // Payment is not in progress anymore so it's either failed or fulfilled
        val send = connector.ask[BigIntegerVec]("blindtokens/redeem", "seskey" -> memo.sesPubKeyHex)
        val send1 = send.map(memo.makeClearSigs).map(memo.packEverything).doOnCompleted(me doProcess CMDStart)
        send1.foreach(fresh => me BECOME data.copy(info = None, tokens = data.tokens ++ fresh), onError)
      }

      def onError(err: Throwable) = err.getMessage match {
        case "notfulfilled" if pr.isFresh && app.ChannelManager.canSend(maxPrice).nonEmpty =>
          // Retry an existing payment request instead of getting a new one until it expires
          val send = me getSided retry(withRoutesAndOnionRPIFromPR(pr), pickInc, 4 to 5)
          send.foreach(app.ChannelManager.sendOpt(_, none), none)

        // A special case where server can't find
        // our tokens at all so we just start over
        case "notfound" => me updRequestData None
        case other => Tools log other
      }

    case (_, act: CloudAct) =>
      // Record new action and try to send it
      me BECOME data.copy(acts = data.acts + act)
      me doProcess CMDStart

    case _ =>
  }

  // ADDING NEW TOKENS

  val updRequestData: Option[RequestAndMemo] => Unit = oram => me BECOME data.copy(info = oram)
  // Send CMDStart only in case if call was successful as we may enter an infinite loop otherwise
  // We only start over if server can't actually find our tokens, otherwise just wait for next try


  def getFreshData = for {
    prm @ (pr, memo) <- getPaymentRequestAndBlindMemo
    if pr.unsafeMsat < maxPrice && memo.clears.size > 20
    Some(rpi) <- withRoutesAndOnionRPIFromPR(pr)
    if data.info.isEmpty
  } yield rpi -> prm

  def withRoutesAndOnionRPIFromPR(pr: PaymentRequest) = {
    val emptyRPI = RuntimePaymentInfo(emptyRD, pr, pr.unsafeMsat)
    app.ChannelManager withRoutesAndOnionRPI emptyRPI
  }

  // TALKING TO SERVER

  def getPaymentRequestAndBlindMemo: Obs[RequestAndMemo] =
    connector.ask[TokensInfo]("blindtokens/info") flatMap {
      case (signerMasterPubKey, signerSessionPubKey, quantity) =>
        val pubKeyQ = ECKey.fromPublicOnly(HEX decode signerMasterPubKey)
        val pubKeyR = ECKey.fromPublicOnly(HEX decode signerSessionPubKey)

        // Prepare a list of BlindParam and a list of BigInteger clear tokens
        val blinder = new ECBlind(pubKeyQ.getPubKeyPoint, pubKeyR.getPubKeyPoint)
        val memo = BlindMemo(blinder params quantity, blinder tokens quantity, pubKeyR.getPublicKeyAsHex)
        connector.ask[String]("blindtokens/buy", "tokens" -> memo.makeBlindTokens.toJson.toString.hex,
          "seskey" -> memo.sesPubKeyHex).map(PaymentRequest.read).map(pr => pr -> memo)
      }
}

// Sig-based authentication
class PrivateCloud extends Cloud { me =>
  lazy val connector = new Connector(data.url)

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    // Execute if we are not busy and have available actions
    case CloudData(_, _, SET(action, _*), _) \ CMDStart if isFree =>
      val sig = Crypto encodeSignature Crypto.sign(Crypto sha256 action.data, LNParams.cloudPrivateKey)
      val params = Seq("sig" -> sig.toString, "pubkey" -> LNParams.cloudPublicKey.toString, BODY -> data.toString)
      val send = connector.ask[String](action.path, params ++ action.plus:_*).doOnCompleted(me doProcess CMDStart)
      send.foreach(ok => me BECOME data.copy(acts = data.acts - action), Tools.errlog)

    case (_, act: CloudAct) =>
      // Record new action and try to send it
      me BECOME data.copy(acts = data.acts + act)
      me doProcess CMDStart

    case _ =>
  }
}

class Connector(val url: String) {
  def http(way: String) = post(s"$url/$way", true)
  def ask[T : JsonFormat](command: String, params: HttpParam*): Obs[T] =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: response +: _) => response.convertTo[T]
      case _ => throw new ProtocolException
    }

  def findNodes(query: String) = ask[AnnounceChansNumVec]("router/nodes", "query" -> query)
  def getBackup(key: BinaryData) = ask[StringVec]("data/get", "key" -> key.toString).map(_ map HEX.decode)
  def getChildTxs(txs: TxSeq) = ask[TxSeq]("txs/get", "txids" -> txs.map(_.txid).toJson.toString.hex)

  def findRoutes(rd: RoutingData, froms: Set[PublicKey], to: PublicKey) =
    ask[PaymentRouteVec]("router/routes", "xn" -> rd.badNodes.map(_.toBin).toJson.toString.hex,
      "xc" -> rd.badChans.toJson.toString.hex, "froms" -> froms.map(_.toBin).toJson.toString.hex,
      "tos" -> Set(to).map(_.toBin).toJson.toString.hex)
}

object Connector {
  type TxSeq = Seq[Transaction]
  type StringVec = Vector[String]
  type BigIntegerVec = Vector[BigInteger]
  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  type ClearToken = (String, String, String)
  type TokensInfo = (String, String, Int)
  type HttpParam = (String, String)
  val CMDStart = "CMDStart"
  val BODY = "body"
}