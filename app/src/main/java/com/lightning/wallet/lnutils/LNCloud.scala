package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.Connector._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import fr.acinq.bitcoin.{BinaryData, Crypto, Transaction}
import rx.lang.scala.{Observable => Obs}

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

  def getSided[T](obs: Obs[T] /* manage isFree */) = {
    val observable1 = obs doOnSubscribe { isFree = false }
    observable1 doOnTerminate { isFree = true }
  }

  def BECOME(d1: CloudData) = {
    // Save to db on every update
    // TODO: add saving capability
    become(d1, state)
  }
}

// Stores token request parameters, clear tokens left and tasks to be executed, also url for custom server
case class CloudData(info: Option[RequestAndMemo], tokens: Set[ClearToken], acts: Set[CloudAct], url: String)
// Represents a remote call to be executed in exchange for token or signature, can be locally persisted
case class CloudAct(data: BinaryData, plus: Seq[HttpParam], path: String)

class PublicCloud(bag: PaymentInfoBag) extends Cloud { me =>
  def capableChanExists = app.ChannelManager.canSend(max).nonEmpty
  //val connector = new Connector("http://213.133.99.89:9002")
  val connector = new Connector("http://10.0.2.2:9002")
  val max = 20000000L

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, tokens, acts, _) \ CMDStart
      // We are free AND (no tokens left OR few tokens left AND no acts left) AND channel exists
      if isFree && (tokens.isEmpty || acts.isEmpty && tokens.size < 5) && capableChanExists =>
      // This will intercept the next case only if we have no acts left which is desirable
      me getSided retry(getFreshData, pickInc, 4 to 5) foreach { case rpi \ info =>
        // If requested sum is low enough and tokens quantity is high enough
        // and info has not already been set by another request
        app.ChannelManager.send(rpi, none)
        me BECOME data.copy(info = info)
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
    case CloudData(Some(pr \ memo), _, _, _) \ CMDStart if isFree =>
      // Our payment may still be in-flight or ir may be fulfilled or failed already
      val isInFlight = app.ChannelManager.activeInFlightHashes contains pr.paymentHash
      if (isInFlight) Tools log "LNCloud payment is still in-flight, doing nothing" else {
        val send = me getSided connector.ask[BigIntegerVec]("blindtokens/redeem", "seskey" -> memo.sesPubKeyHex)
        val send1 = send.map(memo.makeClearSigs).map(memo.packEverything).doOnCompleted(me doProcess CMDStart)
        send1.foreach(fresh => me BECOME data.copy(info = None, tokens = data.tokens ++ fresh), onError)
      }

      def onError(err: Throwable) = err.getMessage match {
        case "notfulfilled" if pr.isFresh && capableChanExists =>
          // Retry an existing request instead of getting a new one until it expires
          val send = me getSided retry(withRoutesAndOnionRPIFromPR(pr), pickInc, 4 to 5)
          send.foreach(foeRPI => app.ChannelManager.sendEither(foeRPI, none), none)

        // Server can't find our tokens or request has expired
        case "notfulfilled" => me BECOME data.copy(info = None)
        case "notfound" => me BECOME data.copy(info = None)
        case other => Tools log other
      }

    case (_, act: CloudAct) =>
      // Record new action and try to send it
      // recording happens irregardless of state
      me BECOME data.copy(acts = data.acts + act)
      me doProcess CMDStart

    case _ =>
  }

  // ADDING NEW TOKENS

  def getFreshData = for {
    prAndMemo @ (pr, memo) <- getPaymentRequestBlindMemo
    if pr.unsafeMsat < max && memo.clears.size > 20
    Right(rpi) <- withRoutesAndOnionRPIFromPR(pr)
    info = Some(prAndMemo)
    if data.info.isEmpty
  } yield rpi -> info

  def withRoutesAndOnionRPIFromPR(pr: PaymentRequest) = {
    val emptyRPI = RuntimePaymentInfo(emptyRD, pr, pr.unsafeMsat)
    // These payments will always be dust so frozen is not an issue
    app.ChannelManager withRoutesAndOnionRPIFrozenAllowed emptyRPI
  }

  // TALKING TO SERVER

  def getPaymentRequestBlindMemo: Obs[RequestAndMemo] =
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
      // recording happens irregardless of state
      me BECOME data.copy(acts = data.acts + act)
      me doProcess CMDStart

    case _ =>
  }
}

class Connector(val url: String) {
  def http(way: String) = post(s"$url/$way", true) connectTimeout 15000
  def ask[T : JsonFormat](command: String, params: HttpParam*): Obs[T] =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: response +: _) => response.convertTo[T]
      case _ => throw new ProtocolException
    }

  def getBlock(hash: String) = ask[BlockHeightAndTxs]("block/get", "hash" -> hash)
  def getBackup(key: BinaryData) = ask[StringVec]("data/get", "key" -> key.toString)
  def findNodes(query: String) = ask[AnnounceChansNumVec]("router/nodes", "query" -> query)
  def getChildTxs(txs: TxSeq) = ask[TxSeq]("txs/get", "txids" -> txs.map(_.txid).toJson.toString.hex)
  def findRoutes(rd: RoutingData, froms: Set[PublicKey], to: PublicKey) = ask[PaymentRouteVec]("router/routes",
    "froms" -> froms.map(_.toBin).toJson.toString.hex, "tos" -> Set(to).map(_.toBin).toJson.toString.hex,
    "xn" -> rd.badNodes.map(_.toBin).toJson.toString.hex, "xc" -> rd.badChans.toJson.toString.hex)
}

object Connector {
  type TxSeq = Seq[Transaction]
  type StringVec = Vector[String]
  type BigIntegerVec = Vector[BigInteger]
  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  type ClearToken = (String, String, String)
  type BlockHeightAndTxs = (Long, StringVec)
  type TokensInfo = (String, String, Int)
  type HttpParam = (String, String)
  val CMDStart = "CMDStart"
  val BODY = "body"
}