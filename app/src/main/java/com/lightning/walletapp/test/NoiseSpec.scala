package com.lightning.walletapp.test

import com.lightning.walletapp.ln.crypto.Noise._
import fr.acinq.bitcoin.BinaryData
import NoiseSpec._
import com.lightning.walletapp.ln.crypto.ByteStream


class NoiseSpec {

  def hashTests = {
    // see https://tools.ietf.org/html/rfc4231
    assert(BinaryData(SHA256HashFunctions.hmacHash(BinaryData("0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"), BinaryData("4869205468657265"))) == BinaryData("b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"))
    val (out1, out2) = SHA256HashFunctions.hkdf(BinaryData("4e6f6973655f4e4e5f32353531395f436861436861506f6c795f534841323536"), BinaryData("37e0e7daacbd6bfbf669a846196fd44d1c8745d33f2be42e31d4674199ad005e"))
    println("hkdf")
    assert((BinaryData(out1), BinaryData(out2)) == (BinaryData("f6f78327c10316fdad06633fb965e03182e9a8b1f755d613f7980fbb85ebf46d"), BinaryData("4ee4220f31dbd3c9e2367e66a87f1e98a2433e4b9fbecfd986d156dcf027b937")))
  }

  def Noise_XK_secp256k1_ChaChaPoly_SHA256_test_vectors = {
    val dh = Secp256k1DHFunctions
    val prologue = "lightning".getBytes("UTF-8")

    object Initiator {
      val s = dh.generateKeyPair("1111111111111111111111111111111111111111111111111111111111111111")
      val e: BinaryData = "1212121212121212121212121212121212121212121212121212121212121212"
    }
    object Responder {
      val s = dh.generateKeyPair("2121212121212121212121212121212121212121212121212121212121212121")
      val e: BinaryData = "2222222222222222222222222222222222222222222222222222222222222222"
    }

    val initiator = HandshakeState.initializeWriter(
      handshakePattern = handshakePatternXK,
      prologue = prologue,
      s = Initiator.s, e = KeyPair(BinaryData.empty, BinaryData.empty), rs = Responder.s.pub, re = BinaryData.empty,
      dh = dh, cipher = Chacha20Poly1305CipherFunctions, hash = SHA256HashFunctions,
      byteStream = FixedStream(Initiator.e))

    val responder = HandshakeState.initializeReader(
      handshakePattern = handshakePatternXK,
      prologue = prologue,
      s = Responder.s, e = KeyPair(BinaryData.empty, BinaryData.empty), rs = BinaryData.empty, re = BinaryData.empty,
      dh = dh, cipher = Chacha20Poly1305CipherFunctions, hash = SHA256HashFunctions,
      byteStream = FixedStream(Responder.e))

    val (outputs, (enc, dec)) = handshake(initiator, responder, BinaryData.empty :: BinaryData.empty :: BinaryData.empty :: Nil)

    assert(enc.asInstanceOf[InitializedCipherState].k == BinaryData("0x969ab31b4d288cedf6218839b27a3e2140827047f2c0f01bf5c04435d43511a9"))
    assert(dec.asInstanceOf[InitializedCipherState].k == BinaryData("0xbb9020b8965f4df047e07f955f3c4b88418984aadc5cdb35096b9ea8fa5c3442"))
    val (enc1, ciphertext01) = enc.encryptWithAd(BinaryData.empty, BinaryData("79656c6c6f777375626d6172696e65"))
    val (dec1, ciphertext02) = dec.encryptWithAd(BinaryData.empty, BinaryData("7375626d6172696e6579656c6c6f77"))
    assert(outputs == List(
      BinaryData("0x036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a"),
      BinaryData("0x02466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae"),
      BinaryData("0xb9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
    ))
    assert(ciphertext01 == BinaryData("b64b348cbb37c88e5b76af12dce00a4a69cbe224a374aad16a4ab1b93741c4"))
    assert(ciphertext02 == BinaryData("289de201e633a43e01ea5b0ec1df9726bd04d0109f530f7172efa5808c3108"))
  }

  def allTests = {
    hashTests
    Noise_XK_secp256k1_ChaChaPoly_SHA256_test_vectors
  }
}

object NoiseSpec {

  /**
    * ByteStream implementation that always returns the same data.
    */
  case class FixedStream(data: BinaryData) extends ByteStream {
    override def getBytes(length: Int) = data
  }

  /**
    * Performs a Noise handshake. Initiator and responder must use the same handshake pattern.
    *
    * @param init    initiator
    * @param resp    responder
    * @param inputs  inputs messages (can all be empty, but the number of input messages must be equal to the number of
    *                remaining handshake patterns)
    * @param outputs accumulator, for internal use only
    * @return the list of output messages produced during the handshake, and the pair of cipherstates produced during the
    *         final stage of the handshake
    */
  def handshake(init: HandshakeStateWriter, resp: HandshakeStateReader, inputs: List[BinaryData], outputs: List[BinaryData] = Nil): (List[BinaryData], (CipherState, CipherState)) = {
    assert(init.messages == resp.messages)
    assert(init.messages.length == inputs.length)
    inputs match {
      case last :: Nil =>
        val (_, (ics0, ics1, _), message) = init.write(last)
        val (_, (rcs0, rcs1, _), _) = resp.read(message)
        assert(ics0 == rcs0)
        assert(ics1 == rcs1)
        ((BinaryData(message) :: outputs).reverse, (ics0, ics1))
      case head :: tail =>
        val (resp1, _, message) = init.write(head)
        val (init1, _, _) = resp.read(message)
        handshake(init1, resp1, tail, message :: outputs)

      case _ =>
        throw new Exception
    }
  }
}