package com.lightning.walletapp.lnutils

import scala.concurrent.duration._
import com.neovisionaries.ws.client._
import com.lightning.walletapp.lnutils.RelayNode._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.wire.{Hop, NodeAnnouncement}
import rx.lang.scala.{Observable => Obs}

import com.lightning.walletapp.ChannelManager
import com.lightning.walletapp.ln.Tools.none
import com.lightning.walletapp.ln.LNParams
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.MilliSatoshi


object RelayNode {
  final val relayNodeKey = PublicKey("02330d13587b67a85c0a36ea001c4dba14bcd48dda8988f7303275b040bffb6abd")
  def relayPeerReports = ChannelManager.chanReports.filter(_.chan.data.announce.nodeId == relayNodeKey)
  def hasRelayPeerOnly = ChannelManager.chanReports.forall(_.chan.data.announce.nodeId == relayNodeKey)

  def makeWebSocket(ann: NodeAnnouncement)(fun: String => Unit) = {
    val endPoint = s"ws://${ann.workingAddress.getHostString}:8089/ws"
    val webSocket = (new WebSocketFactory).createSocket(endPoint, 7500)
    webSocket.connectAsynchronously

    webSocket addListener new WebSocketAdapter {
      override def onTextMessage(ws: WebSocket, raw: String) = fun(raw)
      override def onDisconnected(ws: WebSocket, s: WebSocketFrame, e: WebSocketFrame, cbs: Boolean) =
        Obs.just(null).delay(2.seconds).doOnTerminate(ws.recreate.connectAsynchronously).foreach(none)
    }

    new Runnable {
      override def run = {
        webSocket.clearListeners
        webSocket.disconnect
      }
    }
  }
}

abstract class RelayNode(payeeNodeId: PublicKey) {
  type ChannelBalanceInfos = Seq[ChannelBalanceInfo]
  type BestDeliverableAndHop = (MilliSatoshi, Hop)
  var best: Option[BestDeliverableAndHop] = None

  def onDataUpdated: Unit
  def start(ann: NodeAnnouncement) = makeWebSocket(ann) { raw =>
    val me2JointMaxSendable = relayPeerReports.map(_.estimateFinalCanSend).reduceOption(_ max _) getOrElse 0L
    val joint2PayeeMaxSendable = to[ChannelBalanceInfos](raw).filter(_.peerNodeId == payeeNodeId).sortBy(- _.withoutMaxFee).headOption
    val deliverableThroughJoint = MilliSatoshi(joint2PayeeMaxSendable.map(_.withoutMaxFee) getOrElse 0L min me2JointMaxSendable)
    best = if (deliverableThroughJoint.amount <= 10000L) None else joint2PayeeMaxSendable.map(deliverableThroughJoint -> _.hop)
    onDataUpdated
  }
}

case class ChannelBalance(canSendMsat: Long, canReceiveMsat: Long)
case class ChannelBalanceInfo(balance: ChannelBalance, peerNodeId: PublicKey, shortChannelId: Long, cltvExpiryDelta: Int,
                              htlcMinimumMsat: Long, feeBaseMsat: Long, feeProportionalMillionths: Long) {

  // We get balance info from our peer so its node key should be contained in Hop
  val hop = Hop(relayNodeKey, shortChannelId, cltvExpiryDelta, htlcMinimumMsat, feeBaseMsat, feeProportionalMillionths)
  val withoutMaxFee = balance.canSendMsat - feeBaseMsat - LNParams.maxHtlcValueMsat * feeProportionalMillionths / 1000000L
}