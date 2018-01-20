package com.lightning.wallet.lnutils

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.Connector._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import fr.acinq.bitcoin.{BinaryData, Crypto, Transaction}
import rx.lang.scala.{Observable => Obs}
import scala.util.{Failure, Success}

import com.lightning.wallet.ln.wire.LightningMessageCodecs.AnnounceChansNum
import com.lightning.wallet.ln.RoutingInfoTag.PaymentRoute
import collection.JavaConverters.mapAsJavaMapConverter
import com.github.kevinsawicki.http.HttpRequest.post
import com.lightning.wallet.ln.Tools.random
import rx.lang.scala.schedulers.IOScheduler
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException
import org.bitcoinj.core.ECKey
import java.math.BigInteger


abstract class Cloud extends StateMachine[CloudData] {
  // Persisted data exchange with a maintenance server
  // Checks if user's server accepts a signature
  def checkIfWorks: Obs[Any] = Obs just true
  protected[this] var isFree = true
  val connector: Connector

  def BECOME(d1: CloudData) = {
    // Save to db on every update
    CloudDataSaver saveObject d1
    become(d1, state)
  }
}

// Represents a remote call to be executed, can be persisted
case class CloudAct(data: BinaryData, plus: Seq[HttpParam], path: String)
// Stores token request parameters, clear tokens left and tasks to be executed, also url for custom server
case class CloudData(info: Option[RequestAndMemo], tokens: Set[ClearToken], acts: Set[CloudAct], url: String)
class PublicCloud(val connector: Connector, bag: PaymentInfoBag) extends Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, tokens, acts, _) \ CMDStart
      // Try to get new tokens either when we have no tokens left
      // or when we have no acts to execute and a few tokens left
      if tokens.isEmpty || acts.isEmpty && tokens.size < 5 =>

      for {
        operationalChannel <- app.ChannelManager.all.find(_.isOperational)
        paymentRequestAndMemo @ (pr, memo) <- retry(getRequestAndMemo, pickInc, 3 to 4)
        Some(rpi) <- retry(app.ChannelManager outPaymentObs emptyRPI(pr), pickInc, 3 to 4)
        // Server response may arrive in some time after our requesting, must account for that
      } if (data.info.isEmpty && pr.amount.forall(_.amount < 20000000L) && memo.clears.size > 20) {
        // Proceed if data is still empty, the price is low enough and number of sigs is high enough
        me BECOME data.copy(info = Some apply paymentRequestAndMemo)
        operationalChannel process CMDSilentAddHtlc(rpi)
      }

    // Execute if we are not busy and have available tokens and actions, don't care amout memo
    case CloudData(_, SET(token @ (point, clear, sig), _*), SET(action, _*), _) \ CMDStart if isFree =>
      val params = Seq("point" -> point, "cleartoken" -> clear, "clearsig" -> sig, BODY -> action.data.toString) ++ action.plus
      val go = connector.ask[String](action.path, params:_*) doOnTerminate { isFree = true } doOnCompleted { me doProcess CMDStart }
      go.foreach(_ => me BECOME data.copy(acts = data.acts - action, tokens = data.tokens - token), onError)
      isFree = false

      // data may change while request is on so we always copy it
      def onError(serverError: Throwable) = serverError.getMessage match {
        case "tokeninvalid" => me BECOME data.copy(tokens = data.tokens - token)
        case "tokenused" => me BECOME data.copy(tokens = data.tokens - token)
        case _ => Tools errlog serverError
      }

    // We don't have acts or tokens but have a memo
    case CloudData(Some(pr \ memo), _, _, _) \ CMDStart =>

      bag getPaymentInfo pr.paymentHash match {
        case Success(rpi) if rpi.actualStatus == SUCCESS => me resolveSuccess memo
        // These will have long expiration times, could be retried multiple times
        case Success(rpi) if rpi.actualStatus == FAILURE =>

          if (!pr.isFresh) eraseRequestData else for {
            operationalChannel <- app.ChannelManager.all.find(_.isOperational)
            // Repeatedly retry an old request instead of getting a new one until it expires
            Some(pay) <- retry(app.ChannelManager outPaymentObs emptyRPI(pr), pickInc, 3 to 4)
          } operationalChannel process CMDSilentAddHtlc(pay)

        // First attempt has been rejected
        case Failure(_) => eraseRequestData
        // Probably WAITING so do nothing
        case _ =>
      }

    case (_, action: CloudAct) =>
      val actions1 = data.acts + action
      me BECOME data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
  }

  // ADDING NEW TOKENS

  def eraseRequestData = me BECOME data.copy(info = None)
  // Send CMDStart only in case if call was successful as we may enter an infinite loop otherwise
  // We only start over if server can't actually find our tokens, otherwise just wait for next try
  def resolveSuccess(memo: BlindMemo) = getClearTokens(memo).doOnCompleted(me doProcess CMDStart)
    .foreach(plus => me BECOME data.copy(info = None, tokens = data.tokens ++ plus),
      error => if (error.getMessage == "notfound") eraseRequestData)

  // TALKING TO SERVER

  def getRequestAndMemo: Obs[RequestAndMemo] =
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

  def getClearTokens(m: BlindMemo) =
    connector.ask[BigIntegerVec]("blindtokens/redeem",
      "seskey" -> m.sesPubKeyHex).map(m.makeClearSigs).map(m.pack)
}

// Users may supply their own cloud with key based authentication
class PrivateCloud(val connector: Connector) extends Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    // Execute if we are not busy and have available actions
    case CloudData(_, _, SET(action, _*), _) \ CMDStart if isFree =>
      val go = connector.ask[String](action.path, signed(action.data) ++ action.plus:_*)
      val go1 = go doOnTerminate { isFree = true } doOnCompleted { me doProcess CMDStart }
      go1.foreach(_ => me BECOME data.copy(acts = data.acts - action), Tools.errlog)
      isFree = false

    case (_, action: CloudAct) =>
      val actions1 = data.acts + action
      me BECOME data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
  }

  def signed(data: BinaryData) = {
    val sig = Crypto encodeSignature Crypto.sign(Crypto sha256 data, LNParams.cloudPrivateKey)
    Seq("sig" -> sig.toString, "pubkey" -> LNParams.cloudPublicKey.toString, BODY -> data.toString)
  }

  override def checkIfWorks = {
    val params = signed(random getBytes 32)
    connector.ask[String]("check", params:_*)
  }
}

class Connector(val url: String) {
  def http(way: String) = post(s"$url/v1/$way", true)
  def ask[T : JsonFormat](command: String, params: HttpParam*): Obs[T] =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: response +: _) => response.convertTo[T]
      case _ => throw new ProtocolException
    }

  def findNodes(query: String) = ask[AnnounceChansNumVec]("router/nodes", "query" -> query)
  def getBackup(key: String) = ask[StringVec]("data/get", "key" -> key).map(_ map HEX.decode)
  def getChildTxs(txs: TxSeq) = ask[TxSeq]("txs/get", "txids" -> txs.map(_.txid).toJson.toString.hex)
  def findRoutes(rd: RoutingData, from: PublicKey, to: PublicKey) = ask[PaymentRouteVec]("router/routes",
    "nodes" -> rd.badNodes.map(_.toBin).toJson.toString.hex, "channels" -> rd.badChans.toJson.toString.hex,
    "from" -> from.toString, "to" -> to.toString)
}

class FailoverConnector(failover: Connector, url: String) extends Connector(url) {
  override def getChildTxs(txs: TxSeq) = super.getChildTxs(txs).onErrorResumeNext(_ => failover getChildTxs txs)
  override def getBackup(key: String) = super.getBackup(key).onErrorResumeNext(_ => failover getBackup key)
}

object Connector {
  type TxSeq = Seq[Transaction]
  type StringVec = Vector[String]
  type BigIntegerVec = Vector[BigInteger]
  type PaymentRouteVec = Vector[PaymentRoute]
  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  type ClearToken = (String, String, String)
  type TokensInfo = (String, String, Int)
  type HttpParam = (String, String)
  val CMDStart = "CMDStart"
  val BODY = "body"
}