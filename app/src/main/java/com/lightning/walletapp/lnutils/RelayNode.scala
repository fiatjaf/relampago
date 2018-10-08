package com.lightning.walletapp.lnutils

import scala.concurrent.duration._
import com.neovisionaries.ws.client._
import com.lightning.walletapp.lnutils.RelayNode._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import rx.lang.scala.{Observable => Obs}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import com.lightning.walletapp.ln.Tools.{none, runAnd}
import com.lightning.walletapp.ln.wire.NodeAnnouncement
import com.lightning.walletapp.ChannelManager
import fr.acinq.bitcoin.Crypto.PublicKey


object RelayNode {
  final val relaySockPort = 8089
  final val relayNodeKey = PublicKey("02330d13587b67a85c0a36ea001c4dba14bcd48dda8988f7303275b040bffb6abd")
  def relayPeerReports = ChannelManager.chanReports.filter(_.chan.data.announce.nodeId == relayNodeKey)
  def hasRelayPeerOnly = ChannelManager.chanReports.forall(_.chan.data.announce.nodeId == relayNodeKey)
}

abstract class RelayNode(payeeNodeId: PublicKey) { me =>
  var canDeliver: Option[MilliSatoshi] = None
  var wsOpt: Option[WebSocket] = None

  def start(ann: NodeAnnouncement) = {
    type RelayChannelInfos = Seq[RelayChannelInfo]
    val endPoint = s"ws://${ann.workingAddress.getHostString}:$relaySockPort/ws"
    val ws = (new WebSocketFactory).createSocket(endPoint, 7500)
    ws.connectAsynchronously
    wsOpt = Some(ws)

    ws addListener new WebSocketAdapter {
      override def onDisconnected(ws: WebSocket, s: WebSocketFrame, e: WebSocketFrame, cbs: Boolean) =
        Obs.just(null).delay(2.seconds).doOnTerminate(ws.recreate.connectAsynchronously).foreach(none)

      override def onTextMessage(ws: WebSocket, raw: String) = {
        val balances = to[RelayChannelInfos](raw).filter(_.peerNodeId == payeeNodeId).map(_.balances)
        val fromMe2Relay = relayPeerReports.map(_.estimateFinalCanSend).reduceOption(_ max _) getOrElse 0L
        // 10 sat for routing + 4000000 sat max amount / 0.01% = 400 so in total we have to reserve 410 sat
        val fromRelay2Payee = balances.map(_.canSendMsat - 410000L).reduceOption(_ max _) getOrElse 0L
        val deliverableThroughRelay = MilliSatoshi(fromMe2Relay min fromRelay2Payee max 0L)
        canDeliver = if (balances.isEmpty) None else Some(deliverableThroughRelay)
        onDataUpdated
      }
    }
  }

  def onDataUpdated: Unit
  def disconnect = for (ws: WebSocket <- wsOpt)
    runAnd(ws.clearListeners)(ws.disconnect)
}

case class RelayChannelState(canSendMsat: Long, canReceiveMsat: Long)
case class RelayChannelInfo(balances: RelayChannelState, channelId: BinaryData, peerNodeId: PublicKey)