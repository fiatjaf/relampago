package com.lightning.walletapp.lnutils.olympus

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import rx.lang.scala.{Observable => Obs}
import com.lightning.walletapp.Utils.app
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.ECKey


// Uses special paid tokens to store data on server, is constructed directly from a database
class Cloud(val identifier: String, var connector: Connector, var auth: Int, val removable: Int,
            val maxPriceMsat: Long = 5000000L) extends StateMachine[CloudData] { me =>

  private var isFree = true
  def isAuthEnabled = auth == 1

  // STATE MACHINE

  def BECOME(d1: CloudData) = {
    // Save fresh data to database on every update
    OlympusWrap.updData(d1.toJson.toString, identifier)
    become(d1, state)
  }

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, clearTokens, actions) \ CMDStart if isFree &&
      (clearTokens.isEmpty || actions.isEmpty && clearTokens.size < 5) &&
      app.ChannelManager.canSendNow(maxPriceMsat).nonEmpty &&
      isAuthEnabled =>

      // This guard will intercept the next branch only if we are not capable of sending or have nothing to send
      val send = retry(getFreshData, pickInc, 4 to 5) doOnSubscribe { isFree = false } doOnTerminate { isFree = true }

      for (Right(rd) \ newInfo <- send) {
        me BECOME data.copy(info = newInfo)
        PaymentInfoWrap addPendingPayment rd
      }

    // Execute anyway if we are free and have available tokens and actions
    case CloudData(_, (point, clear, signature) +: tokens, action +: _) \ CMDStart if isFree =>
      val params = Seq("point" -> point, "cleartoken" -> clear, "clearsig" -> signature, BODY -> action.data.toString)
      // Be careful here: must make sure `doOnTerminate` changes `isFree` before `doOnCompleted` sends `CMDStart`

      val send = connector.ask[String](action.path, params ++ action.plus:_*)
      val send1 = send doOnSubscribe { isFree = false } doOnTerminate { isFree = true }
      send1.doOnCompleted(me doProcess CMDStart).foreach(onGotResponse, onGotResponse)

      def onGotResponse(response: Any) = response match {
        case "done" => me BECOME data.copy(acts = data.acts diff Vector(action), tokens = tokens)
        case err: Throwable if err.getMessage == "tokeninvalid" => me BECOME data.copy(tokens = tokens)
        case err: Throwable if err.getMessage == "tokenused" => me BECOME data.copy(tokens = tokens)
        case _ =>
      }

    // We do not have any acts or tokens but have a memo
    case CloudData(Some(pr \ memo), _, _) \ CMDStart if isFree =>
      // Payment may still be unsent OR in-flight OR fulfilled OR failed
      val isInFlight = app.ChannelManager.activeInFlightHashes contains pr.paymentHash
      val send = connector.ask[BigIntegerVec]("blindtokens/redeem", "seskey" -> memo.sesPubKeyHex)

      if (!isInFlight) {
        // Assume that payment has been fulfilled and try to obtain storage tokens
        val send1 = send doOnSubscribe { isFree = false } doOnTerminate { isFree = true }
        val send2 = send1.map(memo.makeClearSigs).map(memo.packEverything).doOnCompleted(me doProcess CMDStart)
        send2.foreach(fresh => me BECOME data.copy(info = None, tokens = data.tokens ++ fresh), onError)
      }

      def onError(err: Throwable) = err.getMessage match {
        case "notfulfilled" if pr.isFresh => me retryFresh pr
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
        connector.ask[String]("blindtokens/buy", "lang" -> app.getString(com.lightning.walletapp.R.string.lang),
          "tokens" -> memo.makeBlindTokens.toJson.toString.hex, "seskey" -> memo.sesPubKeyHex)
            .map(PaymentRequest.read).map(pr => pr -> memo)
    }

  // ADDING NEW TOKENS

  private def getFreshData = for {
    prAndMemo @ (pr, memo) <- getPaymentRequestBlindMemo
    if pr.unsafeMsat < maxPriceMsat && memo.clears.size > 20
    rd = emptyRD(pr, firstMsat = pr.unsafeMsat, useCache = true)
    check = app.ChannelManager checkIfSendable rd
    info = Some(prAndMemo)
    if data.info.isEmpty
  } yield check -> info

  def retryFresh(pr: PaymentRequest): Unit = {
    val rd = emptyRD(pr, pr.unsafeMsat, useCache = true)
    val sendableResult = app.ChannelManager checkIfSendable rd
    sendableResult.right foreach PaymentInfoWrap.addPendingPayment
  }
}