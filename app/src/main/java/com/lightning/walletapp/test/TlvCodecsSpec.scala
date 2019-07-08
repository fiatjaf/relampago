package com.lightning.walletapp.test

import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.ln.wire.Tlv._
import com.lightning.walletapp.ln.wire.{GenericTlv, Tlv, TlvStream}
import scodec.bits.ByteVector
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.UInt64
import fr.acinq.eclair.UInt64.Conversions._
import scodec.codecs._
import scodec.Codec


class TlvCodecsSpec {
  import TlvCodecsSpec._

  def allTests = {

    {
      println("encode/decode tlv")

      val testCases = Seq(
        (ByteVector.fromValidHex("01 08 000000000000002a"), TestType1(42)),
        (ByteVector.fromValidHex("02 08 0000000000000226"), TestType2(550)),
        (ByteVector.fromValidHex("03 31 02eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619 0000000000000231 0000000000000451"), TestType3(PublicKey.fromValidHex("02eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"), 561, 1105))
      )

      for ((bin, expected) <- testCases) {
        val decoded = testTlvCodec.decode(bin.bits).require.value.asInstanceOf[Tlv]
        assert(decoded == expected)
        val encoded = testTlvCodec.encode(expected).require.bytes
        assert(encoded == bin)
      }
    }

    {
      println("decode invalid tlv")

      val testCases = Seq(
        ByteVector.fromValidHex("fd02"), // type truncated
        ByteVector.fromValidHex("fd022a"), // truncated after type
        ByteVector.fromValidHex("fd0100"), // not minimally encoded type
        ByteVector.fromValidHex("2a fd02"), // length truncated
        ByteVector.fromValidHex("2a fd0226"), // truncated after length
        ByteVector.fromValidHex("2a fe01010000"), // not minimally encoded length
        ByteVector.fromValidHex("2a fd2602 0231"), // value truncated
        ByteVector.fromValidHex("02 01 2a"), // short channel id too short
        ByteVector.fromValidHex("02 09 010101010101010101"), // short channel id length too big
        ByteVector.fromValidHex("2a ff0000000000000080") // invalid length (too big to fit inside a long)
      )

      for (testCase <- testCases) {
        assert(testTlvCodec.decode(testCase.toBitVector).isFailure)
      }
    }

    {
      println("decode invalid tlv stream")

      val testCases = Seq(
        ByteVector.fromValidHex("0108000000000000002a 02"), // valid tlv record followed by invalid tlv record (only type, length and value are missing)
        ByteVector.fromValidHex("02080000000000000226 0108000000000000002a"), // valid tlv records but invalid ordering
        ByteVector.fromValidHex("02080000000000000231 02080000000000000451"), // duplicate tlv type
        ByteVector.fromValidHex("0108000000000000002a 2a0101"), // unknown even type
        ByteVector.fromValidHex("0a080000000000000231 0b0400000451") // valid tlv records but from different namespace
      )

      for (testCase <- testCases) {
        assert(tlvStream(testTlvCodec).decode(testCase.toBitVector).isFailure, testCase)
      }
    }

    {
      println("create invalid tlv stream")

      val a = try { TlvStream(Seq(GenericTlv(42, ByteVector.fromValidHex("2a")))); 0 } catch { case _: Throwable => 1 }
      val b = try { TlvStream(Seq(TestType1(561), TestType2(1105), GenericTlv(42, ByteVector.fromValidHex("2a")))); 0 } catch { case _: Throwable => 1 } // unknown even type
      val c = try { TlvStream(Seq(TestType1(561), TestType1(1105))); 0 } catch { case _: Throwable => 1 } // duplicate type
      val d = try { TlvStream(Seq(TestType2(1105), TestType1(561))); 0 } catch { case _: Throwable => 1 } // invalid ordering

      assert(a + b + c + d == 4)
    }

    {
      println("encode/decode tlv stream")

      val bin = ByteVector.fromValidHex("01080000000000000231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451")
      val expected = Seq(
        TestType1(561),
        TestType2(1105),
        TestType3(PublicKey.fromValidHex("02eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"), 561, 1105)
      )

      val decoded = tlvStream(testTlvCodec).decode(bin.toBitVector).require.value
      assert(decoded == TlvStream(expected))

      val encoded = tlvStream(testTlvCodec).encode(TlvStream(expected)).require.toByteVector
      assert(encoded == bin)
    }

    {
      println("encode/decode tlv stream with unknown odd type")

      val bin = ByteVector.fromValidHex("01080000000000000231 0b0400000451 0d02002a")
      val expected = Seq(
        TestType1(561),
        GenericTlv(11, ByteVector.fromValidHex("00000451")),
        TestType13(42)
      )

      val decoded = tlvStream(testTlvCodec).decode(bin.toBitVector).require.value
      assert(decoded == TlvStream(expected))

      val encoded = tlvStream(testTlvCodec).encode(TlvStream(expected)).require.toByteVector
      assert(encoded == bin)
    }

    {
      println("encoded/decode empty tlv stream")

      assert(tlvStream(testTlvCodec).decode(ByteVector.empty.bits).require.value == TlvStream(Nil))
      assert(tlvStream(testTlvCodec).encode(TlvStream(Nil)).require.bytes == ByteVector.empty)
    }

    {
      println("encode/decode length-prefixed tlv stream")

      val codec = lengthPrefixedTlvStream(testTlvCodec)
      val testCases = Seq(
        ByteVector.fromValidHex("47 01080000000000000231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451"),
        ByteVector.fromValidHex("fd5301 01080000000000000231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451 ff6543210987654321 fd0001 10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010010101010101")
      )

      for (testCase <- testCases) {
        assert(codec.encode(codec.decode(testCase.bits).require.value).require.bytes == testCase)
      }
    }

    {
      println("decode invalid length-prefixed tlv stream")

      val testCases = Seq(
        ByteVector.fromValidHex("48 01080000000000000231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451"),
        ByteVector.fromValidHex("46 01080000000000000231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451"),
        ByteVector.fromValidHex("01080000000000000231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451")
      )

      for (testCase <- testCases) {
        assert(lengthPrefixedTlvStream(testTlvCodec).decode(testCase.bits).isFailure)
      }
    }

  }
}

object TlvCodecsSpec {
  sealed trait TestTlv extends Tlv
  case class TestType1(uintValue: UInt64) extends TestTlv { override val tag = UInt64(1) }
  case class TestType2(shortChannelId: Long) extends TestTlv { override val tag = UInt64(2) }
  case class TestType3(nodeId: PublicKey, value1: UInt64, value2: UInt64) extends TestTlv { override val tag = UInt64(3) }
  case class TestType13(intValue: Int) extends TestTlv { override val tag = UInt64(13) }

  val testCodec1: Codec[TestType1] = (("length" | constant(ByteVector.fromValidHex("08"))) :: ("value" | uint64)).as[TestType1]
  val testCodec2: Codec[TestType2] = (("length" | constant(ByteVector.fromValidHex("08"))) :: ("short_channel_id" | int64)).as[TestType2]
  val testCodec3: Codec[TestType3] = (("length" | constant(ByteVector.fromValidHex("31"))) :: ("node_id" | publicKey) :: ("value_1" | uint64) :: ("value_2" | uint64)).as[TestType3]
  val testCodec13: Codec[TestType13] = (("length" | constant(ByteVector.fromValidHex("02"))) :: ("value" | uint16)).as[TestType13]
  val testTlvCodec = discriminated[Tlv].by(varint).typecase(UInt64(1L), testCodec1).typecase(UInt64(2L), testCodec2).typecase(UInt64(3L), testCodec3).typecase(UInt64(13L), testCodec13)

  sealed trait OtherTlv extends Tlv
  case class OtherType1(uintValue: UInt64) extends OtherTlv { override val tag = UInt64(10) }
  case class OtherType2(smallValue: Long) extends OtherTlv { override val tag = UInt64(11) }

  val otherCodec1: Codec[OtherType1] = (("length" | constant(ByteVector.fromValidHex("08"))) :: ("value" | uint64)).as[OtherType1]
  val otherCodec2: Codec[OtherType2] = (("length" | constant(ByteVector.fromValidHex("04"))) :: ("value" | uint32)).as[OtherType2]
  val otherTlvCodec = discriminated[Tlv].by(varint).typecase(10, otherCodec1).typecase(11, otherCodec2)
}