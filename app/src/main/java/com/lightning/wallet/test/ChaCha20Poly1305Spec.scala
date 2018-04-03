package com.lightning.wallet.test

import com.lightning.wallet.ln.crypto.{ChaCha20Legacy, ChaCha20Poly1305, Poly1305}
import fr.acinq.bitcoin.BinaryData


class ChaCha20Poly1305Spec {

  def legacy = {
    val plaintext: BinaryData = "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it.".getBytes
    val key: BinaryData = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    val nonce: BinaryData = "0000004a00000000"

    val ciphertext = ChaCha20Legacy.process(plaintext, key, nonce, encrypt = true, skipBlock = true)
    assert(BinaryData(ciphertext) == BinaryData("6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0bf91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d807ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab77937365af90bbf74a35be6b40b8eedf2785e42874d"))

    assert(BinaryData(ChaCha20Legacy.process(ciphertext, key, nonce, encrypt = false, skipBlock = true)) == BinaryData(plaintext))
  }

  def legacyKeyGeneration = {
    // see https://tools.ietf.org/html/rfc7539#section-2.6.2
    val key: BinaryData = "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"
    val nonce: BinaryData = "0001020304050607"
    assert(BinaryData(ChaCha20Legacy.process(new Array[Byte](32), key, nonce, encrypt = true, skipBlock = false)) ==
      BinaryData("8ad5a08b905f81cc815040274ab29471a833b637e3fd0da508dbb8e2fdd1a646"))
  }

  def poly1305 = {
    // from https://tools.ietf.org/html/rfc7539 ch 2.5.2
    val key: BinaryData = "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
    val data: BinaryData = "Cryptographic Forum Research Group".getBytes("UTF-8")
    val mac = Poly1305.mac(key, data)
    assert(BinaryData(mac) == BinaryData("a8061dc1305136c6c22b8baf0c0127a9"))
  }

  def poly1305two = {
    // from golang's poly1305 tests
    val key: BinaryData = "this is 32-byte key for Poly1305".getBytes()
    val data: BinaryData = "Hello world!".getBytes("UTF-8")
    val mac = Poly1305.mac(key, data)
    assert(BinaryData(mac) == BinaryData("a6f745008f81c916a20dcc74eef2b2f0"))
  }

  def ietf = {
    // see https://tools.ietf.org/html/rfc7539#section-2.8.1
    val plaintext = BinaryData("4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e")
    val key = BinaryData("808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f")
    val nonce = BinaryData("070000004041424344454647")
    val aad = BinaryData("50515253c0c1c2c3c4c5c6c7")

    val (ciphertext, mac) = ChaCha20Poly1305.encrypt(key, nonce, plaintext, aad)
    assert(BinaryData(ciphertext) == BinaryData("d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116"))
    assert(BinaryData(mac) == BinaryData("1ae10b594f09e26a7e902ecbd0600691"))

    val check = ChaCha20Poly1305.decrypt(key, nonce, ciphertext, aad, mac)
    assert(BinaryData(check) == plaintext)
  }

  def allTests = {
    legacyKeyGeneration
    legacy
    poly1305
    poly1305two
    ietf
  }
}
