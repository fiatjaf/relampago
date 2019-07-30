package com.lightning.walletapp.ln

import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.Features._

import rx.lang.scala.{Observable => Obs}
import com.lightning.walletapp.ln.crypto.Noise.KeyPair
import java.util.concurrent.ConcurrentHashMap
import fr.acinq.bitcoin.Crypto.PublicKey
import java.util.concurrent.Executors
import scodec.bits.ByteVector
import java.net.Socket


object ConnectionManager {
  var listeners = Set.empty[ConnectionListener]
  val workers = new ConcurrentHashMap[PublicKey, Worker].asScala
  val keyPair = KeyPair(nodePublicKey.toBin, nodePrivateKey.toBin)

  protected[this] val events = new ConnectionListener {
    override def onMessage(nodeId: PublicKey, msg: LightningMessage) = for (lst <- listeners) lst.onMessage(nodeId, msg)
    override def onHostedMessage(ann: NodeAnnouncement, msg: HostedChannelMessage) = for (lst <- listeners) lst.onHostedMessage(ann, msg)
    override def onOperational(nodeId: PublicKey, isCompat: Boolean) = for (lst <- listeners) lst.onOperational(nodeId, isCompat)
    override def onDisconnect(nodeId: PublicKey) = for (lst <- listeners) lst.onDisconnect(nodeId)
  }

  def connectTo(ann: NodeAnnouncement, notify: Boolean) = synchronized {
    if (workers.get(ann.nodeId).isEmpty) workers(ann.nodeId) = new Worker(ann)
    else if (notify) events.onOperational(ann.nodeId, isCompat = true)
  }

  class Worker(val ann: NodeAnnouncement, buffer: Bytes = new Bytes(1024), val sock: Socket = new Socket) {
    implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
    var lastMsg = System.currentTimeMillis

    val handler: TransportHandler = new TransportHandler(keyPair, ann.nodeId) {
      def handleEnterOperationalState = handler process Init(LNParams.globalFeatures, LNParams.localFeatures)
      def handleEncryptedOutgoingData(data: ByteVector) = try sock.getOutputStream write data.toArray catch handleError
      def handleDecryptedIncomingData(data: ByteVector) = intercept(LightningMessageCodecs deserialize data)
      def handleError = { case _ => disconnect }
    }

    val thread = Future {
      // Node may have many addresses but we use the first valid for simplicity
      val theOne = ann.addresses.collectFirst(NodeAddress.toInetSocketAddress)
      sock.connect(theOne.get, 7500)
      handler.init

      while (true) {
        val length = sock.getInputStream.read(buffer, 0, buffer.length)
        if (length < 0) throw new RuntimeException("Connection droppped")
        else handler process ByteVector.view(buffer take length)
      }
    }

    thread onComplete { _ =>
      workers.remove(ann.nodeId)
      events onDisconnect ann.nodeId
    }

    def disconnect = try sock.close catch none
    def intercept(message: LightningMessage) = {
      // Update liveness on each incoming message
      lastMsg = System.currentTimeMillis

      message match {
        case their: Init => events.onOperational(isCompat = areSupported(their.localFeatures) && dataLossProtect(their.localFeatures), nodeId = ann.nodeId)
        case Ping(replyLength, _) if replyLength > 0 && replyLength <= 65532 => handler process Pong(ByteVector fromValidHex "00" * replyLength)
        case hostedChanMessage: HostedChannelMessage => events.onHostedMessage(ann, hostedChanMessage)
        case _ => events.onMessage(ann.nodeId, message)
      }
    }
  }

  for {
    _ <- Obs interval 30.seconds
    tooLongAgo = System.currentTimeMillis - 1000L * 60
    worker <- workers.values if worker.lastMsg < tooLongAgo
  } worker.disconnect
}

class ConnectionListener {
  def onOpenOffer(nodeId: PublicKey, msg: OpenChannel): Unit = none
  def onMessage(nodeId: PublicKey, msg: LightningMessage): Unit = none
  def onHostedMessage(ann: NodeAnnouncement, msg: HostedChannelMessage): Unit = none
  def onOperational(nodeId: PublicKey, isCompat: Boolean): Unit = none
  def onDisconnect(nodeId: PublicKey): Unit = none
}