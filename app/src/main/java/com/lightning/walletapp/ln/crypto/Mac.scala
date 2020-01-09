package com.lightning.walletapp.ln.crypto

import com.lightning.walletapp.ln.Tools.Bytes
import org.spongycastle.crypto.digests.SHA256Digest
import org.spongycastle.crypto.params.KeyParameter
import org.spongycastle.crypto.macs.HMac
import scodec.bits.ByteVector


trait Mac32 {
  def mac(message: ByteVector): ByteVector
  def verify(mac: ByteVector, message: ByteVector): Boolean
}

case class Hmac256(key: ByteVector) extends Mac32 {
  override def mac(message: ByteVector): ByteVector = Mac32.hmac256(key, message)
  override def verify(mac1: ByteVector, message: ByteVector): Boolean = mac(message) == mac1
}

object Mac32 {
  def hmac256(key: ByteVector, message: String): ByteVector = {
    val finalMessage = ByteVector(message getBytes "UTF-8")
    hmac256(key, finalMessage)
  }

  def hmac256(key: ByteVector, message: ByteVector): ByteVector = {
    val (hMac, output) = new HMac(new SHA256Digest) -> new Bytes(32)

    hMac init new KeyParameter(key.toArray)
    hMac.update(message.toArray, 0, message.length.toInt)
    hMac.doFinal(output, 0)
    ByteVector view output
  }
}