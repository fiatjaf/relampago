package com.lightning.walletapp.lnutils.olympus

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.Utils.app
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.ECKey


// Uses special paid tokens to store data on server, is constructed directly from a database
class Cloud(val identifier: String, var connector: Connector, var auth: Int, val removable: Int,
            val maxPriceMsat: Long = 5000000L) extends StateMachine[CloudData] { me =>

  private var isFree = true
  def isAuthEnabled = auth == 1
  def BECOME(cloudData: CloudData) = {
    // Just save updated data to database on every change
    OlympusWrap.updData(cloudData.toJson.toString, identifier)
    become(cloudData, state)
  }

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, clearTokens, actions) \ CMDStart if isFree &&
      (clearTokens.isEmpty || actions.isEmpty && clearTokens.size < 5) &&
      isAuthEnabled =>

      // Also executes if we have no actions to upload and a few tokens left
      val send = retry(getPaymentRequestBlindMemo, pick = pickInc, times = 4 to 5)
      val send1 = send doOnSubscribe { isFree = false } doOnTerminate { isFree = true }

      for {
        pr \ memo <- send1
        if data.info.isEmpty && pr.unsafeMsat < maxPriceMsat && memo.clears.size > 20
        memoSaved = me BECOME CloudData(Some(pr, memo), clearTokens, actions)
      } retryFreshRequest(pr)

    // Execute anyway if we are free and have available tokens and actions
    case CloudData(_, (point, clear, signature) +: ts, action +: _) \ CMDStart if isFree =>
      val params = Seq("point" -> point, "cleartoken" -> clear, "clearsig" -> signature, BODY -> action.data.toString)
      // Be careful here: must make sure `doOnTerminate` changes `isFree` before `doOnCompleted` sends `CMDStart`

      val send = connector.ask[String](action.path, params ++ action.plus:_*)
      val send1 = send doOnSubscribe { isFree = false } doOnTerminate { isFree = true }
      send1.doOnCompleted(me doProcess CMDStart).foreach(onGotResponse, onGotResponse)

      def onGotResponse(response: Any) = response match {
        case "done" => me BECOME data.copy(acts = data.acts diff Vector(action), tokens = ts)
        case err: Throwable if err.getMessage == "tokeninvalid" => me BECOME data.copy(tokens = ts)
        case err: Throwable if err.getMessage == "tokenused" => me BECOME data.copy(tokens = ts)
        case _ =>
      }

    // We do not have any acts or tokens but have a memo
    case CloudData(Some(pr \ memo), _, _) \ CMDStart if isFree =>
      // Payment may still be unsent OR in-flight OR fulfilled OR failed already
      val isInFlight = app.ChannelManager.activeInFlightHashes contains pr.paymentHash

      if (!isInFlight) {
        // Assume that payment has been fulfilled and try to obtain storage tokens
        val send = connector.ask[BigIntegerVec]("blindtokens/redeem", "seskey" -> memo.key)
        val sendRelease = send doOnSubscribe { isFree = false } doOnTerminate { isFree = true }
        val sendConvert = sendRelease.map(memo.makeClearSigs).map(memo.packEverything).doOnCompleted(me doProcess CMDStart)
        sendConvert.foreach(freshTokens => me BECOME data.copy(info = None, tokens = data.tokens ++ freshTokens), onError)
      }

      def onError(err: Throwable) = err.getMessage match {
        case "notfulfilled" if pr.isFresh => retryFreshRequest(pr)
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

  private def getPaymentRequestBlindMemo =
    connector.ask[TokensInfo]("blindtokens/info") flatMap {
      case (signerMasterPubKey, signerSessionPubKey, quantity) =>
        val pubKeyQ = ECKey.fromPublicOnly(HEX decode signerMasterPubKey)
        val pubKeyR = ECKey.fromPublicOnly(HEX decode signerSessionPubKey)
        val ecBlind = new ECBlind(pubKeyQ.getPubKeyPoint, pubKeyR.getPubKeyPoint)

        val lang = app.getString(com.lightning.walletapp.R.string.lang)
        val memo = BlindMemo(ecBlind params quantity, ecBlind tokens quantity, pubKeyR.getPublicKeyAsHex)
        connector.ask[String]("blindtokens/buy", "tokens" -> memo.makeBlindTokens.toJson.toString.hex,
          "lang" -> lang, "seskey" -> memo.key).map(PaymentRequest.read).map(pr => pr -> memo)
    }

  def retryFreshRequest(pr: PaymentRequest): Unit = {
    val ok = app.ChannelManager.notClosing.exists(isOperational)
    val rd = emptyRD(pr, pr.unsafeMsat, Set.empty, useCache = true)
    if (ok) PaymentInfoWrap addPendingPayment rd
  }
}