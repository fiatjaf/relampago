package com.lightning.walletapp.hivemind

import com.lightning.walletapp.Utils._
import org.bitcoinj.wallet.SendRequest._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import fr.acinq.bitcoin.{Base58Check, BinaryData, OP_0, OP_PUSHDATA, SIGHASH_ALL, Satoshi, Script, Transaction}
import org.bitcoinj.core.{Coin, TransactionInput, TransactionOutPoint}
import com.lightning.walletapp.ln.PubKeyScriptIndexFinder
import fr.acinq.bitcoin.SigVersion.SIGVERSION_BASE
import fr.acinq.bitcoin.Crypto.PrivateKey
import org.bitcoinj.script.ScriptBuilder
import language.implicitConversions


object Hivemind {
  val encodedPrivKey = "L3UjtLhNXKZaDgFtf14EHkxV1p5CKUoyRUT5DcU7aUS1X2yX8hhg"
  val scriptPubKey = BinaryData("76a9147c33a3f6d9d5b873f96dba4b12d6aaf6be71fbd288ac")
  val (_, data) = Base58Check decode encodedPrivKey
  val privateKey = PrivateKey(data)

  implicit def sat2Coin(sat: Satoshi): Coin = Coin valueOf sat.amount
  def madeDepositTx(hiveMindHash: BinaryData, depositAmount: Satoshi, prevTx: Transaction) = {
    val prevDepositIndex = new PubKeyScriptIndexFinder(prevTx).findPubKeyScriptIndex(scriptPubKey)
    val prevDepositAmount = prevTx.txOut(prevDepositIndex).amount
    val newDepositAmount = prevDepositAmount + depositAmount
    val rq = to(app.params, scriptPubKey, newDepositAmount)

    // Add OP_RETURN with our pubkey hash so sidechain knows about it
    val opReturnScript = Script.write(OP_0 :: OP_PUSHDATA(hiveMindHash) :: Nil)
    rq.tx.addOutput(Coin.ZERO, ScriptBuilder createOpReturnScript opReturnScript)

    val outPoint = new TransactionOutPoint(app.params, prevDepositIndex, prevTx)
    // Add an input (without signature for now) which spends from previous hivemind deposit tx
    val input = new TransactionInput(app.params, prevTx, Array.emptyByteArray, outPoint, prevDepositAmount)
    rq.tx.addInput(input)

    val currentDepositIndex = new PubKeyScriptIndexFinder(rq.tx).findPubKeyScriptIndex(scriptPubKey)
    val sig = Transaction.signInput(rq.tx, currentDepositIndex, scriptPubKey, SIGHASH_ALL, newDepositAmount, SIGVERSION_BASE, privateKey)
    // Add a signature to hivemind input using a drivechain private key, then sign the rest of transaction inputs in a standard way
    input setScriptSig Script.write(OP_PUSHDATA(sig) :: OP_PUSHDATA(privateKey.publicKey) :: Nil)
    app.kit.wallet addLocalInputsToTx rq

    // Deposit input will be treated as already signed
    // the rest of inputs will be signed using wallet keys
    input.disconnect
    app.kit sign rq
  }
}