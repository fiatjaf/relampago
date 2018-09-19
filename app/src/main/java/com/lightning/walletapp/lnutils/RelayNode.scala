package com.lightning.walletapp.lnutils

import scala.concurrent.duration._
import com.neovisionaries.ws.client._
import com.lightning.walletapp.lnutils.RelayNode._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import rx.lang.scala.{Observable => Obs}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import com.lightning.walletapp.ln.wire.NodeAnnouncement
import com.lightning.walletapp.ln.Tools.runAnd
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.walletapp.Utils.app


object RelayNode {
  final val relaySockPort = 8089
  final val relayNodeKey = PublicKey("02330d13587b67a85c0a36ea001c4dba14bcd48dda8988f7303275b040bffb6abd")
  def relayPeerReports = app.ChannelManager.chanReports.filter(_.chan.data.announce.nodeId == relayNodeKey)
}

abstract class RelayNode(ann: NodeAnnouncement, payeeNodeId: PublicKey) { me =>
  val endPoint = s"ws://${ann.workingAddress.getHostString}:$relaySockPort/ws"
  val ws = (new WebSocketFactory).createSocket(endPoint, 7500)
  type RelayChannelInfos = Seq[RelayChannelInfo]
  var directSend = MilliSatoshi(0L)

  ws addListener new WebSocketAdapter {
    override def onDisconnected(ws: WebSocket, s: WebSocketFrame, e: WebSocketFrame, cbs: Boolean) =
      Obs.just(null).delay(2.seconds).foreach(in2Sec => ws.recreate.connectAsynchronously)

    override def onTextMessage(ws: WebSocket, raw: String) = {
      val balances = to[RelayChannelInfos](raw).filter(_.peerNodeId == payeeNodeId).map(_.balances)
      val fromMe2Relay = relayPeerReports.map(_.estimateFinalCanSend).reduceOption(_ max _) getOrElse 0L
      val fromRelay2Payee = balances.map(_.canSendMsat).reduceOption(_ max _) getOrElse 0L
      directSend = MilliSatoshi(fromMe2Relay min fromRelay2Payee)
      onGuaranteedAmountRefreshed
    }
  }

  // Need to remove listeners and only then disconnect
  def disconnect = runAnd(ws.clearListeners)(ws.disconnect)
  def onGuaranteedAmountRefreshed: Unit
  ws.connectAsynchronously
}

case class RelayChannelState(canSendMsat: Long, canReceiveMsat: Long)
case class RelayChannelInfo(balances: RelayChannelState, channelId: BinaryData, peerNodeId: PublicKey)