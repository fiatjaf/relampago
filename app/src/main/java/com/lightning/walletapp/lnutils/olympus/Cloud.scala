package com.lightning.walletapp.lnutils.olympus

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.ChannelManager
import com.lightning.walletapp.ln.Tools.wrap
import com.lightning.walletapp.Utils.app
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.ECKey


// Uses special paid tokens to store data on server, is constructed directly from a database
class Cloud(val identifier: String, var connector: Connector, var auth: Int, val removable: Int,
            val maxMsat: Long = 5000000L) extends StateMachine[CloudData] { me =>

  private var isFree = true
  def BECOME(cloudData: CloudData) = {
    // Just save updated data to database on every change
    app.olympus.updData(cloudData.toJson.toString, identifier)
    become(cloudData, state)
  }

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, clearTokens, actions) \ CMDStart if isFree &&
      (clearTokens.isEmpty || actions.isEmpty && clearTokens.size < 5) &&
      ChannelManager.chanReports.exists(_.softReserveCanSend >= maxMsat) &&
      isAuthEnabled =>

      // Also executes if we have no actions to upload and a few tokens left
      val send = retry(getPaymentRequestBlindMemo, pick = pickInc, times = 4 to 5)
      val send1 = send doOnSubscribe { isFree = false } doOnTerminate { isFree = true }

      for {
        pr \ memo <- send1
        if data.info.isEmpty && pr.unsafeMsat < maxMsat && memo.clears.size > 20
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
        case err: Throwable if err.getMessage == "tokeninvalid" => me BECOME data.copy(tokens = ts)
        case err: Throwable if err.getMessage == "tokenused" => me BECOME data.copy(tokens = ts)

        case "done" =>
          val acts1 = data.acts diff Vector(action)
          val data1 = data.copy(acts = acts1, tokens = ts)
          wrap(me BECOME data1)(action.onDone)

        case _ =>
      }

    // We do not have any acts or tokens but have a memo
    case CloudData(Some(pr \ memo), _, _) \ CMDStart if isFree =>
      // Payment may still be unsent OR in-flight OR fulfilled OR failed already
      val isInFlight = ChannelManager.activeInFlightHashes contains pr.paymentHash

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

    case (_, act: CloudAct) if 0 == removable || isAuthEnabled || data.tokens.nonEmpty =>
      // Accept acts and just store them if this cloud is not removable OR auth is on OR tokens left
      // by doing this we can catch channel backups and upload them later if user re-enables tokens
      me BECOME data.copy(acts = data.acts :+ act take 25)
      me doProcess CMDStart

    case (_, cs: CloudSnapshot) if cs.url == connector.url =>
      // We may get new tokens off-band after restoring from GDrive
      me BECOME data.copy(tokens = (data.tokens ++ cs.tokens).distinct)

    case _ =>
  }

  // TALKING TO SERVER

  def getPaymentRequestBlindMemo =
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

  def isAuthEnabled = 1 == auth
  def snapshot = CloudSnapshot(data.tokens, connector.url)
  def retryFreshRequest(failedPayReq: PaymentRequest): Unit = {
    val rd = emptyRD(failedPayReq, firstMsat = failedPayReq.unsafeMsat, useCache = true)
    val isOk = ChannelManager.chanReports.exists(_.softReserveCanSend >= failedPayReq.unsafeMsat)
    if (isOk) PaymentInfoWrap addPendingPayment rd
  }
}