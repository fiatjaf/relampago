package com.lightning.walletapp.test

import java.nio.ByteOrder
import fr.acinq.bitcoin.Protocol
import scodec.bits.ByteVector


class FeaturesSpec {
  import com.lightning.walletapp.ln.Features._
  
  def allTests = {

    {
      println("'data_loss_protect' feature")
      assert(dataLossProtect(ByteVector.fromValidHex("02"))) // optional
      assert(!dataLossProtect(ByteVector.fromValidHex("00")))
      assert(dataLossProtect(ByteVector.fromValidHex("81"))) // mandatory
    }

    {
      println("'variable_length_onion' feature")
      assert(variableLengthOnion(ByteVector.fromValidHex("0100")))
      assert(variableLengthOnion(ByteVector.fromValidHex("0200")))
    }

    {
      println("features compatibility")
      assert(areSupported(Protocol.writeUInt64(1L << OPTION_DATA_LOSS_PROTECT_OPTIONAL, ByteOrder.BIG_ENDIAN)))
      assert(!areSupported(ByteVector.fromValidHex("14")))
      assert(!areSupported(ByteVector.fromValidHex("0141")))
      assert(areSupported(Protocol.writeUInt64(1l << VARIABLE_LENGTH_ONION_OPTIONAL, ByteOrder.BIG_ENDIAN)))
      assert(!areSupported(ByteVector.fromValidHex("14")))
      assert(!areSupported(ByteVector.fromValidHex("0141")))
    }

  }
}
