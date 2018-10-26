package com.lightning.walletapp.lnutils

import scala.concurrent.duration._
import com.neovisionaries.ws.client._
import com.lightning.walletapp.lnutils.RelayNode._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._

import rx.lang.scala.{Observable => Obs}
import com.lightning.walletapp.ln.Tools.{none, runAnd}
import com.lightning.walletapp.ln.wire.{Hop, NodeAnnouncement}
import com.lightning.walletapp.ChannelManager
import com.lightning.walletapp.ln.LNParams
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.MilliSatoshi


object RelayNode {
  final val relaySockPort = 8089
  final val relayNodeKey = PublicKey("02330d13587b67a85c0a36ea001c4dba14bcd48dda8988f7303275b040bffb6abd")
  def relayPeerReports = ChannelManager.chanReports.filter(_.chan.data.announce.nodeId == relayNodeKey)
  def hasRelayPeerOnly = ChannelManager.chanReports.forall(_.chan.data.announce.nodeId == relayNodeKey)
}

abstract class RelayNode(payeeNodeId: PublicKey) {
  var canDeliver: Option[MilliSatoshi] = None
  var wsOpt: Option[WebSocket] = None

  def start(ann: NodeAnnouncement) = {
    type ChannelBalanceInfos = Seq[ChannelBalanceInfo]
    val endPoint = s"ws://${ann.workingAddress.getHostString}:$relaySockPort/ws"
    val ws = (new WebSocketFactory).createSocket(endPoint, 7500)
    ws.connectAsynchronously
    wsOpt = Some(ws)

    ws addListener new WebSocketAdapter {
      override def onDisconnected(ws: WebSocket, s: WebSocketFrame, e: WebSocketFrame, cbs: Boolean) =
        Obs.just(null).delay(2.seconds).doOnTerminate(ws.recreate.connectAsynchronously).foreach(none)

      override def onTextMessage(ws: WebSocket, raw: String) = {
        val fromRelay2PayeeBalances = to[ChannelBalanceInfos](raw).filter(_.peerNodeId == payeeNodeId)
        val fromRelay2PayeeMaxSendable = fromRelay2PayeeBalances.map(_.withoutMaxFee).reduceOption(_ max _) getOrElse 0L
        val fromMe2RelayMaxSendable = relayPeerReports.map(_.estimateFinalCanSend).reduceOption(_ max _) getOrElse 0L
        val deliverableThroughRelay = MilliSatoshi(fromMe2RelayMaxSendable min fromRelay2PayeeMaxSendable max 0L)
        canDeliver = if (deliverableThroughRelay.amount <= 2000L) None else Some(deliverableThroughRelay)
        onDataUpdated
      }
    }
  }

  def onDataUpdated: Unit
  def disconnect = for (ws <- wsOpt)
    runAnd(ws.clearListeners)(ws.disconnect)
}

case class ChannelBalance(canSendMsat: Long, canReceiveMsat: Long)
case class ChannelBalanceInfo(balance: ChannelBalance, peerNodeId: PublicKey, shortChannelId: Long, cltvExpiryDelta: Int,
                              htlcMinimumMsat: Long, feeBaseMsat: Long, feeProportionalMillionths: Long) {

  val hop = Hop(peerNodeId, shortChannelId, cltvExpiryDelta, htlcMinimumMsat, feeBaseMsat, feeProportionalMillionths)
  val withoutMaxFee = balance.canSendMsat - feeBaseMsat - LNParams.maxHtlcValueMsat * feeProportionalMillionths / 1000000L
}