package com.lightning.walletapp.test

import com.lightning.walletapp.hivemind.Hivemind
import com.lightning.walletapp.ln.PubKeyScriptIndexFinder
import fr.acinq.bitcoin.{Bech32, BinaryData, OP_0, OP_PUSHDATA, OutPoint, Satoshi, Script, Transaction, TxIn, TxOut}
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.Utils.app
import org.bitcoinj.script.ScriptBuilder


class HivemindSpec {

  def allTests = {
    {
      println("Deposit tx")
      val hivemindDepositHash = BinaryData("5cb3d8f395c4a43b8add814b1b1bcdd1a050586a")
      val hivemindAddress = Bech32.encodeWitnessAddress("hmtb", 0.toByte, hivemindDepositHash)
      assert(hivemindAddress == "hmtb1qtjea3uu4cjjrhzkas993kx7d6xs9qkr2k4j7ky")
      assert(Bech32.decodeWitnessAddressHivemind(hivemindAddress)._3 == hivemindDepositHash)

      val prevTx = Transaction(version = 0, txIn = TxIn(OutPoint(BinaryData("00" * 32), 0), BinaryData.empty, 0xffffffffL) :: Nil,
        txOut = TxOut(Satoshi(200000), Hivemind.scriptPubKey) :: TxOut(Satoshi(200000), BinaryData("00" * 32)) :: Nil, lockTime = 0)

      val depositTx = Hivemind.madeDepositTx(hivemindDepositHash, Satoshi(100000), prevTx).tx
      assert(depositTx.txOut(new PubKeyScriptIndexFinder(depositTx).findPubKeyScriptIndex(Hivemind.scriptPubKey)).amount == Satoshi(300000L))
      assert(depositTx.txOut.exists(_.amount.amount == app.kit.conf0Balance.value - 100000 - depositTx.getFee.value))

      val opReturnScript = Script.write(OP_0 :: OP_PUSHDATA(hivemindDepositHash) :: Nil)
      val program = ScriptBuilder.createOpReturnScript(opReturnScript).getProgram
      new PubKeyScriptIndexFinder(depositTx).findPubKeyScriptIndex(program)
    }
  }
}