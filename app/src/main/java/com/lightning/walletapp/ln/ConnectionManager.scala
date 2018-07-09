package com.lightning.walletapp.ln

import scala.concurrent._
import scala.concurrent.duration._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.Features._

import rx.lang.scala.{Observable => Obs}
import com.lightning.walletapp.ln.Tools.{Bytes, none}
import com.lightning.walletapp.ln.crypto.Noise.KeyPair
import fr.acinq.bitcoin.Crypto.PublicKey
import java.util.concurrent.Executors
import fr.acinq.bitcoin.BinaryData
import java.net.Socket


object ConnectionManager {
  var listeners = Set.empty[ConnectionListener]
  var connections = Map.empty[PublicKey, Worker]

  protected[this] val events = new ConnectionListener {
    override def onMessage(nodeId: PublicKey, msg: LightningMessage) = for (lst <- listeners) lst.onMessage(nodeId, msg)
    override def onOperational(nodeId: PublicKey, their: Init) = for (lst <- listeners) lst.onOperational(nodeId, their)
    override def onTerminalError(nodeId: PublicKey) = for (lst <- listeners) lst.onTerminalError(nodeId)
    override def onIncompatible(nodeId: PublicKey) = for (lst <- listeners) lst.onIncompatible(nodeId)
    override def onDisconnect(nodeId: PublicKey) = for (lst <- listeners) lst.onDisconnect(nodeId)
  }

  def connectTo(ann: NodeAnnouncement, notify: Boolean) = connections get ann.nodeId match {
    case Some(goodWorker) => if (notify) events.onOperational(ann.nodeId, goodWorker.savedInit)
    case None => connections += ann.nodeId -> new Worker(ann)
  }

  class Worker(ann: NodeAnnouncement) {
    implicit val context = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
    private val keyPair = KeyPair(nodePublicKey.toBin, nodePrivateKey.toBin)
    private val buffer: Bytes = new Bytes(1024)
    private val socket: Socket = new Socket

    val handler: TransportHandler = new TransportHandler(keyPair, ann.nodeId) {
      def handleEnterOperationalState = handler process Init(LNParams.globalFeatures, LNParams.localFeatures)
      def handleDecryptedIncomingData(data: BinaryData) = intercept(LightningMessageCodecs deserialize data)
      def handleEncryptedOutgoingData(data: BinaryData) = try socket.getOutputStream write data catch none
      def handleError = { case _ => events onTerminalError ann.nodeId }
    }

    val work = Future {
      // First blocking connect, then send data
      socket.connect(ann.workingAddress, 7500)
      handler.init

      while (true) {
        val length = socket.getInputStream.read(buffer, 0, buffer.length)
        if (length < 0) throw new RuntimeException("Connection droppped")
        else handler process BinaryData(buffer take length)
      }
    }

    work onComplete { _ =>
      connections -= ann.nodeId
      events onDisconnect ann.nodeId
    }

    var savedInit: Init = _
    var lastMsg = System.currentTimeMillis
    def disconnect = try socket.close catch none
    def intercept(message: LightningMessage) = {
      // Update liveness on each incoming message
      lastMsg = System.currentTimeMillis

      message match {
        case their: Init =>
          // Save their Init for possible subsequent requests
          val isOk = areSupported(their.localFeatures) && dataLossProtect(their.localFeatures)
          if (isOk) events.onOperational(ann.nodeId, their) else events.onIncompatible(ann.nodeId)
          if (isOk) savedInit = their

        case pg: Ping if pg.pongLength > 0 => handler process Pong("00" * pg.pongLength)
        case internalMessage => events.onMessage(ann.nodeId, internalMessage)
      }
    }
  }

  Obs interval 60.seconds foreach { _ =>
    val outdated = System.currentTimeMillis - 1000 * 120
    for (work <- connections.values if work.lastMsg < outdated)
      work.disconnect
  }
}

class ConnectionListener {
  def onMessage(nodeId: PublicKey, msg: LightningMessage): Unit = none
  def onOperational(nodeId: PublicKey, their: Init): Unit = none
  def onTerminalError(nodeId: PublicKey): Unit = none
  def onIncompatible(nodeId: PublicKey): Unit = none
  def onDisconnect(nodeId: PublicKey): Unit = none
}