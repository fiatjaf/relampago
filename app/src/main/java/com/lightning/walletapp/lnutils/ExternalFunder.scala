package com.lightning.walletapp.lnutils

import spray.json._
import scala.concurrent.duration._
import com.neovisionaries.ws.client._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.wire.FundMsg._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import rx.lang.scala.{Observable => Obs}
import com.lightning.walletapp.ln.Tools.none
import java.util


object ExternalFunder {
  var worker = Option.empty[WSWrap]
  def setWSWrap(newWSWrap: WSWrap) = {
    // This is guaranteed to disconnect it
    for (old <- worker) disconnectWSWrap(old)
    newWSWrap.ws.connectAsynchronously
    worker = Some(newWSWrap)
  }

  def disconnectWSWrap(oldWorker: WSWrap, inform: Boolean = true) =
    for (currentWrap <- worker if currentWrap.params == oldWorker.params) {
      // Only disconnect if old websocket wrapper is also a current wrapper
      if (inform) for (lst <- currentWrap.listeners) lst.onDisconnect
      currentWrap.listeners = Set.empty
      currentWrap.ws.clearListeners
      currentWrap.ws.disconnect
      worker = None
    }
}

case class WSWrap(params: Started) { self =>
  val point = params.start.host + ":" + params.start.port
  val loaded = "ws://" + point + "/" + params.start.toJson.toString.hex
  val ws: WebSocket = (new WebSocketFactory).createSocket(loaded, 7500)
  var listeners: Set[ExternalFunderListener] = Set.empty
  var lastMessage: FundMsg = params.start
  var attemptsLeft: Int = 5

  type JavaList = util.List[String]
  type JavaListMap = util.Map[String, JavaList]
  def send(msg: FundMsg) = ws sendText msg.toJson.toString

  def shouldReconnect = lastMessage match {
    case err: Fail => err.code == FAIL_NOT_VERIFIED_YET
    case _: Fail | _: FundingTxBroadcasted => false
    case _ => attemptsLeft > 0
  }

  ws addListener new WebSocketAdapter {
    override def onConnected(websocket: WebSocket, headers: JavaListMap) = attemptsLeft = 5
    override def onTextMessage(ws: WebSocket, message: String) = for (lst <- listeners) lst onMessage to[FundMsg](message)
    override def onDisconnected(ws: WebSocket, scf: WebSocketFrame, ccf: WebSocketFrame, cbs: Boolean) = onConnectError(ws, null)

    override def onConnectError(ws: WebSocket, reason: WebSocketException) = if (shouldReconnect) {
      Obs.just(attemptsLeft -= 1).delay(5.seconds).foreach(in5Sec => for (lst <- listeners) lst.onAttempt)
      for (listener <- listeners) listener.onOffline
    } else ExternalFunder disconnectWSWrap self
  }

  listeners += new ExternalFunderListener {
    override def onMessage(msg: FundMsg) = lastMessage = msg
    override def onAttempt = ws.recreate.connectAsynchronously
  }
}

class ExternalFunderListener {
  def onMessage(msg: FundMsg): Unit = none
  def onDisconnect: Unit = none
  def onAttempt: Unit = none
  def onOffline: Unit = none
}