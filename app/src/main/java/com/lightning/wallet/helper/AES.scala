package com.lightning.wallet.helper

import com.lightning.wallet.ln.Tools.{Bytes, random}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import com.lightning.wallet.ln.crypto.MultiStreamUtils.aconcat
import fr.acinq.bitcoin.BinaryData
import javax.crypto.Cipher


object AES {
  def cipher(key: Bytes, initVector: Bytes, mode: Int) =
    Cipher getInstance "AES/CTR/NoPadding" match { case aesCipher =>
      val ivParameterSpec: IvParameterSpec = new IvParameterSpec(initVector)
      aesCipher.init(mode, new SecretKeySpec(key, "AES"), ivParameterSpec)
      aesCipher
    }

  def enc(data: Bytes, key: Bytes, initVector: Bytes) = cipher(key, initVector, Cipher.ENCRYPT_MODE) doFinal data
  def dec(data: Bytes, key: Bytes, initVector: Bytes) = cipher(key, initVector, Cipher.DECRYPT_MODE) doFinal data
  def unpack(raw: Bytes): (Bytes, Bytes) = (raw.slice(1, 1 + ivLength), raw drop 1 + ivLength)
  def pack(iv: Bytes, cipher: Bytes): Bytes = aconcat(Array(1.toByte), iv, cipher)
  private[this] val ivLength = 16

  def encode(plaintext: String, key: Bytes): BinaryData = {
    // Takes any input string and returns an encoded string

    val initVec = random getBytes ivLength
    val plainbytes = plaintext getBytes "UTF-8"
    val cipherbytes = enc(plainbytes, key, initVec)
    pack(initVec, cipherbytes)
  }

  def decode(key: Bytes)(packed: Bytes): String = {
    // Takes packed format with has version number and iv
    val (initVec, cipherbytes) = unpack(packed)
    val raw = dec(cipherbytes, key, initVec)
    new String(raw, "UTF-8")
  }
}