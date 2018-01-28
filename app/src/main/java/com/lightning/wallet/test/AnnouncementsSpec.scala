package com.lightning.wallet.test

import java.net.InetSocketAddress

import com.lightning.wallet.ln.Announcements._
import fr.acinq.bitcoin.{BinaryData, Block}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random


class AnnouncementsSpec {
  def randomKey: PrivateKey = PrivateKey({
    val bin = Array.fill[Byte](32)(0)
    Random.nextBytes(bin)
    bin
  }, compressed = true)

  val alicePk = PrivateKey(BinaryData("cc" * 32), compressed = true)

  def allTests = Future {

    {
      println("check nodeId1/nodeId2 lexical ordering")
      val node1 = PublicKey("027710df7a1d7ad02e3572841a829d141d9f56b17de9ea124d2f83ea687b2e0461")
      val node2 = PublicKey("0306a730778d55deec162a74409e006034a24c46d541c67c6c45f89a2adde3d9b4")
      // NB: node1 < node2
      assert(isNode1(node1.toBin, node2.toBin))
      assert(!isNode1(node2.toBin, node1.toBin))
    }

    {
      println("create valid signed channel announcement")
      val (node_a, node_b, bitcoin_a, bitcoin_b) = (randomKey, randomKey, randomKey, randomKey)
      val (node_a_sig, bitcoin_a_sig) = signChannelAnnouncement(Block.RegtestGenesisBlock.hash, 42, node_a, node_b.publicKey, bitcoin_a, bitcoin_b.publicKey, "")
      val (node_b_sig, bitcoin_b_sig) = signChannelAnnouncement(Block.RegtestGenesisBlock.hash, 42, node_b, node_a.publicKey, bitcoin_b, bitcoin_a.publicKey, "")
      val ann = makeChannelAnnouncement(Block.RegtestGenesisBlock.hash, 42, node_a.publicKey, node_b.publicKey, bitcoin_a.publicKey, bitcoin_b.publicKey, node_a_sig, node_b_sig, bitcoin_a_sig, bitcoin_b_sig)
      assert(checkSigs(ann))
      assert(!checkSigs(ann.copy(nodeId1 = randomKey.publicKey)))
    }

    {
      println("create valid signed node announcement")
      val ann = makeNodeAnnouncement(alicePk, "alias", (1: Byte, 2: Byte, 3: Byte), new InetSocketAddress("localhost", 9731) :: Nil, System.currentTimeMillis / 1000)
      assert(checkSig(ann))
      assert(!checkSig(ann.copy(timestamp = 153)))
    }

    {
      println("create valid signed channel update announcement")
      val ann = makeChannelUpdate(Block.RegtestGenesisBlock.hash, alicePk, randomKey.publicKey, 45561, 10, 10000, 100, 1, true, System.currentTimeMillis / 1000)
      assert(checkSig(ann, alicePk.publicKey))
      assert(!checkSig(ann, randomKey.publicKey))
    }

    {
      println("check flags")
      val node1_priv = PrivateKey("5f447b05d86de82de6b245a65359d22f844ae764e2ae3824ac4ace7d8e1c749b01")
      val node2_priv = PrivateKey("eff467c5b601fdcc07315933767013002cd0705223d8e526cbb0c1bc75ccb62901")
      // NB: node1 < node2 (public keys)
      println(isNode1(node1_priv.publicKey.toBin, node2_priv.publicKey.toBin))
      println(!isNode1(node2_priv.publicKey.toBin, node1_priv.publicKey.toBin))
      val channelUpdate1 = makeChannelUpdate(Block.RegtestGenesisBlock.hash, node1_priv, node2_priv.publicKey, 0, 0, 0, 0, 0, isEnabled = true, System.currentTimeMillis / 1000)
      val channelUpdate1_disabled = makeChannelUpdate(Block.RegtestGenesisBlock.hash, node1_priv, node2_priv.publicKey, 0, 0, 0, 0, 0, isEnabled = false, System.currentTimeMillis / 1000)
      val channelUpdate2 = makeChannelUpdate(Block.RegtestGenesisBlock.hash, node2_priv, node1_priv.publicKey, 0, 0, 0, 0, 0, isEnabled = true, System.currentTimeMillis / 1000)
      val channelUpdate2_disabled = makeChannelUpdate(Block.RegtestGenesisBlock.hash, node2_priv, node1_priv.publicKey, 0, 0, 0, 0, 0, isEnabled = false, System.currentTimeMillis / 1000)
      println(channelUpdate1.flags == BinaryData("0000")) // ....00
      println(channelUpdate1_disabled.flags == BinaryData("0002")) // ....10
      println(channelUpdate2.flags == BinaryData("0001")) // ....01
      println(channelUpdate2_disabled.flags == BinaryData("0003")) // ....11
      println(isNode1(channelUpdate1.flags))
      println(isNode1(channelUpdate1_disabled.flags))
      println(!isNode1(channelUpdate2.flags))
      println(!isNode1(channelUpdate2_disabled.flags))
      println(!isDisabled(channelUpdate1.flags))
      println(isDisabled(channelUpdate1_disabled.flags))
      println(!isDisabled(channelUpdate2.flags))
      println(isDisabled(channelUpdate2_disabled.flags))
    }

  }
}
