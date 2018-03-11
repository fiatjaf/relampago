package com.lightning.wallet.lnutils.olympus

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.olympus.OlympusWrap._

import org.bitcoinj.core.ECKey
import org.bitcoinj.core.Utils.HEX
import com.lightning.wallet.Utils.app
import com.lightning.wallet.ln.Tools.none
import rx.lang.scala.{Observable => Obs}


// Uses special paid tokens to store data on server, is constructed directly from a database
class Cloud(val identifier: String, var connector: Connector, var auth: Int, val removable: Int,
            val maxPriceMsat: Long = 10000000L) extends StateMachine[CloudData] { me =>

  private var isFree = true
  def isAuthEnabled = auth == 1 // For easy database compatibility
  def capableChannelExists = app.ChannelManager.canSend(maxPriceMsat).nonEmpty
  def withFree[T](obs: Obs[T] /* Adds side effect to control isFree state */) =
    obs doOnSubscribe { isFree = false } doOnTerminate { isFree = true }

  // STATE MACHINE

  def BECOME(d1: CloudData) = {
    // Save fresh data to database on every update
    OlympusWrap.updData(d1.toJson.toString, identifier)
    become(d1, state)
  }

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, tokens, actions) \ CMDStart
      // We are free AND backup is on AND (no tokens left OR few tokens left AND no acts left) AND a channel exists
      if isFree && isAuthEnabled && (tokens.isEmpty || actions.isEmpty && tokens.size < 5) && capableChannelExists =>
      // This guard will intercept the next branch only if we are not capable of sending or have nothing to send
      me withFree retry(getFreshData, pickInc, 4 to 5) foreach { case rpi \ info1 =>
        // If requested sum is low enough and tokens quantity is high enough
        // and info has not already been set by another request
        app.ChannelManager.send(rpi, none)
        me BECOME data.copy(info = info1)
      }

    // Execute anyway if we are free and have available tokens and actions
    case CloudData(_, (point, clear, signature) +: tokens, action +: _) \ CMDStart if isFree =>
      val params = Seq("point" -> point, "cleartoken" -> clear, "clearsig" -> signature, BODY -> action.data.toString)
      // Be careful here: must make sure `doOnTerminate` changes `isFree` before `doOnCompleted` sends `CMDStart`
      val send = me withFree connector.ask[String](action.path, params ++ action.plus:_*)
      send.doOnCompleted(me doProcess CMDStart).foreach(onResponse, onResponse)

      def onResponse(response: Any) = response match {
        case "done" => me BECOME data.copy(acts = data.acts diff Vector(action), tokens = tokens)
        case err: Throwable if err.getMessage == "tokeninvalid" => me BECOME data.copy(tokens = tokens)
        case err: Throwable if err.getMessage == "tokenused" => me BECOME data.copy(tokens = tokens)
        case _ =>
      }

    // We do not have any acts or tokens but have a memo
    case CloudData(Some(pr \ memo), _, _) \ CMDStart if isFree =>
      // Our payment may still be in-flight or fulfilled or failed already
      val isInFlight = app.ChannelManager.activeInFlightHashes contains pr.paymentHash
      if (isInFlight) Tools log "LNCloud payment is still in-flight, doing nothing" else {
        val send = me withFree connector.ask[BigIntegerVec]("blindtokens/redeem", "seskey" -> memo.sesPubKeyHex)
        val send1 = send.map(memo.makeClearSigs).map(memo.packEverything).doOnCompleted(me doProcess CMDStart)
        send1.foreach(fresh => me BECOME data.copy(info = None, tokens = data.tokens ++ fresh), onError)
      }

      def onError(err: Throwable) = err.getMessage match {
        case "notfulfilled" if pr.isFresh && capableChannelExists =>
          // Retry an existing request instead of getting a new one until it expires
          val send = me withFree retry(withRoutesAndOnionRPIFromPR(pr), pickInc, 4 to 5)
          send.foreach(app.ChannelManager.sendEither(_, none), none)

        // Server can't find our tokens or request has expired
        case "notfulfilled" => me BECOME data.copy(info = None)
        case "notfound" => me BECOME data.copy(info = None)
        case other => Tools log other
      }

    case (_, act: CloudAct)
      if isAuthEnabled || data.tokens.nonEmpty =>
      // Backup is active or we have some tokens left
      // Keep processing until run out of tokens in any case
      me BECOME data.copy(acts = data.acts :+ act take 50)
      me doProcess CMDStart

    case _ =>
  }

  // TALKING TO SERVER

  def getPaymentRequestBlindMemo: Obs[RequestAndMemo] =
    connector.ask[TokensInfo]("blindtokens/info") flatMap {
      case (signerMasterPubKey, signerSessionPubKey, quantity) =>
        val pubKeyQ = ECKey.fromPublicOnly(HEX decode signerMasterPubKey)
        val pubKeyR = ECKey.fromPublicOnly(HEX decode signerSessionPubKey)
        val blinder = new ECBlind(pubKeyQ.getPubKeyPoint, pubKeyR.getPubKeyPoint)
        val memo = BlindMemo(blinder params quantity, blinder tokens quantity, pubKeyR.getPublicKeyAsHex)
        connector.ask[String]("blindtokens/buy", "tokens" -> memo.makeBlindTokens.toJson.toString.hex,
          "seskey" -> memo.sesPubKeyHex).map(PaymentRequest.read).map(pr => pr -> memo)
    }

  // ADDING NEW TOKENS

  def getFreshData = for {
    prAndMemo @ (pr, memo) <- getPaymentRequestBlindMemo
    if pr.unsafeMsat < maxPriceMsat && memo.clears.size > 20
    Right(rpi) <- withRoutesAndOnionRPIFromPR(pr)
    info1 = Some(prAndMemo)
    if data.info.isEmpty
  } yield rpi -> info1

  def withRoutesAndOnionRPIFromPR(pr: PaymentRequest) = {
    val emptyRPI = RuntimePaymentInfo(emptyRD, pr, pr.unsafeMsat)
    // These payments will always be dust so frozen is not an issue
    app.ChannelManager withRoutesAndOnionRPIFrozenAllowed emptyRPI
  }
}