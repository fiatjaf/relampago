package com.lightning.walletapp.test

import java.nio.{ByteBuffer, ByteOrder}

import concurrent.ExecutionContext.Implicits.global
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.Helpers.Funding
import com.lightning.walletapp.ln.Scripts._
import com.lightning.walletapp.ln.crypto.Generators
import com.lightning.walletapp.ln.wire.UpdateAddHtlc
import fr.acinq.bitcoin.{BinaryData, Btc, MilliBtc, OutPoint, Satoshi, Script, Transaction, TxOut, _}
import fr.acinq.bitcoin.Crypto.{sign => _, _}
import fr.acinq.bitcoin.Script._

import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Random, Success, Try}


class TransactionsSpec {
  def allTests = {


    {
      println("encode/decode sequence and locktime (one example)")
      val txnumber = 0x11F71FB268DL

      val (sequence, locktime) = encodeTxNumber(txnumber)
      assert(sequence == 0x80011F71L)
      assert(locktime == 0x20FB268DL)

      val txnumber1 = decodeTxNumber(sequence, locktime)
      assert(txnumber == txnumber1)
    }

    {
      println("reconstruct txnumber from sequence and locktime")
      for (i <- 0 until 1000) {
        val txnumber = Random.nextLong() & 0xffffffffffffL
        val (sequence, locktime) = encodeTxNumber(txnumber)
        val txnumber1 = decodeTxNumber(sequence, locktime)
        assert(txnumber == txnumber1)
      }
    }

    {
      println("compute fees")
      // see BOLT #3 specs
      val htlcs = Set(
        Htlc(incoming = false, UpdateAddHtlc("00" * 32, 0, MilliSatoshi(5000000).amount, Hash.Zeroes, 552, BinaryData(""))),
        Htlc(incoming = false, UpdateAddHtlc("00" * 32, 0, MilliSatoshi(1000000).amount, Hash.Zeroes, 553, BinaryData(""))),
        Htlc(incoming = true, UpdateAddHtlc("00" * 32, 0, MilliSatoshi(7000000).amount, Hash.Zeroes, 550, BinaryData(""))),
        Htlc(incoming = true, UpdateAddHtlc("00" * 32, 0, MilliSatoshi(800000).amount, Hash.Zeroes, 551, BinaryData("")))
      )

      val spec = CommitmentSpec(feeratePerKw = 5000, toLocalMsat = 0, toRemoteMsat = 0, htlcs)
      val fee = Scripts.commitTxFee(Satoshi(546), spec)
      assert(fee == Satoshi(5340))
    }

    {
      println("check pre-computed transaction weights")
      val localRevocationPriv = PrivateKey(BinaryData("cc" * 32), compressed = true)
      val localPaymentPriv = PrivateKey(BinaryData("dd" * 32), compressed = true)
      val localHtlcPriv = PrivateKey(BinaryData("ea" * 32), compressed = true)
      val remoteHtlcPriv = PrivateKey(BinaryData("eb" * 32), compressed = true)
      val finalPubKeyScript = Script.write(Script.pay2wpkh(PrivateKey(BinaryData("fe" * 32), compressed = true).publicKey))
      val localDustLimit = Satoshi(542)
      val toLocalDelay = 144
      val feeratePerKw = 1000

      {
        // ClaimP2WPKHOutputTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the ClaimP2WPKHOutputTx
        val pubKeyScript = write(pay2wpkh(localPaymentPriv.publicKey))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(20000), pubKeyScript) :: Nil, lockTime = 0)
        val claimP2WPKHOutputTx = makeClaimP2WPKHOutputTx(commitTx, localPaymentPriv.publicKey, finalPubKeyScript, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(claimP2WPKHOutputTx.get, "bb" * 71, localPaymentPriv.publicKey).tx)
        assert(claimP2WPKHOutputWeight == weight)
      }

      {
        // ClaimHtlcDelayedTx
        // first we create a fake htlcSuccessOrTimeoutTx tx, containing only the output that will be spent by the ClaimDelayedOutputTx
        val pubKeyScript = write(pay2wsh(toLocalDelayed(localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey)))
        val htlcSuccessOrTimeoutTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(20000), pubKeyScript) :: Nil, lockTime = 0)
        val claimHtlcDelayedTx = makeClaimDelayedOutputTx(htlcSuccessOrTimeoutTx, localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey, finalPubKeyScript, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(claimHtlcDelayedTx.get, "bb" * 71).tx)
        assert(claimHtlcDelayedWeight == weight)
      }

      {
        // MainPenaltyTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the MainPenaltyTx
        val pubKeyScript = write(pay2wsh(toLocalDelayed(localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey)))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(20000), pubKeyScript) :: Nil, lockTime = 0)
        val mainPenaltyTx = makeMainPenaltyTx(commitTx, localRevocationPriv.publicKey, finalPubKeyScript, toLocalDelay, localPaymentPriv.publicKey, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(mainPenaltyTx.get, "bb" * 71).tx)
        assert(mainPenaltyWeight == weight)
      }

      {
        // HtlcPenaltyTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the ClaimHtlcSuccessTx
        val paymentPreimage = BinaryData("42" * 32)
        val htlc = UpdateAddHtlc("00" * 32, 0, Satoshi(20000).amount * 1000, sha256(paymentPreimage), expiry = 400144, BinaryData.empty)
        val redeemScript = htlcReceived(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, ripemd160(htlc.paymentHash), htlc.expiry)
        val pubKeyScript = write(pay2wsh(redeemScript))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(htlc.amountMsat / 1000), pubKeyScript) :: Nil, lockTime = 0)
        val htlcPenaltyTx = makeHtlcPenaltyTx(new PubKeyScriptIndexFinder(commitTx), Script.write(redeemScript), finalPubKeyScript, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(htlcPenaltyTx.get, "bb" * 71, localRevocationPriv.publicKey).tx)
        assert(htlcPenaltyWeight == weight)
      }

      {
        // ClaimHtlcSuccessTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the ClaimHtlcSuccessTx
        val paymentPreimage = BinaryData("42" * 32)
        val htlc = UpdateAddHtlc("00" * 32, 0, Satoshi(20000).amount * 1000, sha256(paymentPreimage), expiry = 400144, BinaryData(""))
        val pubKeyScript = write(pay2wsh(htlcOffered(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, ripemd160(htlc.paymentHash))))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(htlc.amountMsat / 1000), pubKeyScript) :: Nil, lockTime = 0)
        val claimHtlcSuccessTx = makeClaimHtlcSuccessTx(new PubKeyScriptIndexFinder(commitTx), remoteHtlcPriv.publicKey, localHtlcPriv.publicKey, localRevocationPriv.publicKey, finalPubKeyScript, htlc, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(claimHtlcSuccessTx.get, "bb" * 71, paymentPreimage).tx)
        assert(claimHtlcSuccessWeight == weight)
      }

      {
        // ClaimHtlcTimeoutTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the ClaimHtlcSuccessTx
        val paymentPreimage = BinaryData("42" * 32)
        val htlc = UpdateAddHtlc("00" * 32, 0, Satoshi(20000).amount * 1000, sha256(paymentPreimage), expiry = 400144, BinaryData(""))
        val pubKeyScript = write(pay2wsh(htlcReceived(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, ripemd160(htlc.paymentHash), htlc.expiry)))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(htlc.amountMsat / 1000), pubKeyScript) :: Nil, lockTime = 0)
        val claimClaimHtlcTimeoutTx = makeClaimHtlcTimeoutTx(new PubKeyScriptIndexFinder(commitTx), remoteHtlcPriv.publicKey, localHtlcPriv.publicKey, localRevocationPriv.publicKey, finalPubKeyScript, htlc, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(claimClaimHtlcTimeoutTx.get, "bb" * 71).tx)
        assert(claimHtlcTimeoutWeight == weight)
      }
    }

    {
      println("generate valid commitment and htlc transactions")
      val localFundingPriv = PrivateKey(BinaryData("a1" * 32) :+ 1.toByte)
      val remoteFundingPriv = PrivateKey(BinaryData("a2" * 32) :+ 1.toByte)
      val localRevocationPriv = PrivateKey(BinaryData("a3" * 32) :+ 1.toByte)
      val localPaymentPriv = PrivateKey(BinaryData("a4" * 32) :+ 1.toByte)
      val localDelayedPaymentPriv = PrivateKey(BinaryData("a5" * 32) :+ 1.toByte)
      val remotePaymentPriv = PrivateKey(BinaryData("a6" * 32) :+ 1.toByte)
      val localHtlcPriv = PrivateKey(BinaryData("a7" * 32) :+ 1.toByte)
      val remoteHtlcPriv = PrivateKey(BinaryData("a8" * 32) :+ 1.toByte)
      val finalPubKeyScript = Script.write(Script.pay2wpkh(PrivateKey(BinaryData("a9" * 32), true).publicKey))
      val commitInput = Funding.makeFundingInputInfo(BinaryData("a0" * 32), 0, Btc(1), localFundingPriv.publicKey, remoteFundingPriv.publicKey)
      val toLocalDelay = 144
      val localDustLimit = Satoshi(542)
      val feeratePerKw = 22000


      // htlc1 and htlc2 are regular IN/OUT htlcs
      val paymentPreimage1 = BinaryData("11" * 32)
      val htlc1 = UpdateAddHtlc("00" * 32, 0, millibtc2satoshi(MilliBtc(100)).amount * 1000, sha256(paymentPreimage1), 300, BinaryData(""))
      val paymentPreimage2 = BinaryData("22" * 32)
      val htlc2 = UpdateAddHtlc("00" * 32, 1, millibtc2satoshi(MilliBtc(200)).amount * 1000, sha256(paymentPreimage2), 300, BinaryData(""))
      // htlc3 and htlc4 are dust htlcs IN/OUT htlcs, with an amount large enough to be included in the commit tx, but too small to be claimed at 2nd stage
      val paymentPreimage3 = BinaryData("33" * 32)
      val htlc3 = UpdateAddHtlc("00" * 32, 2, (localDustLimit + weight2fee(feeratePerKw, htlcTimeoutWeight)).amount * 1000, sha256(paymentPreimage3), 300, BinaryData(""))
      val paymentPreimage4 = BinaryData("44" * 32)
      val htlc4 = UpdateAddHtlc("00" * 32, 3, (localDustLimit + weight2fee(feeratePerKw, htlcSuccessWeight)).amount * 1000, sha256(paymentPreimage4), 300, BinaryData(""))
      val spec = CommitmentSpec(
        feeratePerKw = feeratePerKw,
        toLocalMsat = millibtc2satoshi(MilliBtc(400)).amount * 1000,
        toRemoteMsat = millibtc2satoshi(MilliBtc(300)).amount * 1000,
        htlcs = Set(
          Htlc(incoming = false, htlc1),
          Htlc(incoming = true, htlc2),
          Htlc(incoming = false, htlc3),
          Htlc(incoming = true, htlc4)
        ))

      val commitTxNumber = 0x404142434445L
      val commitTx = {
        val txinfo = makeCommitTx(commitInput, commitTxNumber, localPaymentPriv.toPoint, remotePaymentPriv.toPoint, true, localDustLimit, localRevocationPriv.publicKey, toLocalDelay, localDelayedPaymentPriv.publicKey, remotePaymentPriv.publicKey, localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, spec)
        val localSig = Scripts.sign(localPaymentPriv)(txinfo)
        val remoteSig = Scripts.sign(remotePaymentPriv)(txinfo)
        Scripts.addSigs(txinfo, localFundingPriv.publicKey, remoteFundingPriv.publicKey, localSig, remoteSig)
      }

      {
        assert(getCommitTxNumber(commitTx.tx, true, localPaymentPriv.publicKey, remotePaymentPriv.publicKey) == commitTxNumber)
        val hash: Array[Byte] = Crypto.sha256(localPaymentPriv.publicKey.toBin ++ remotePaymentPriv.publicKey.toBin)
        val num = Protocol.uint64(hash.takeRight(8), ByteOrder.BIG_ENDIAN) & 0xffffffffffffL
        val check = ((commitTx.tx.txIn(0).sequence & 0xffffff) << 24) | (commitTx.tx.lockTime)
        assert((check ^ num) == commitTxNumber)
      }
      val (htlcTimeoutTxs, htlcSuccessTxs) = makeHtlcTxs(commitTx.tx, localDustLimit, localRevocationPriv.publicKey, toLocalDelay, localDelayedPaymentPriv.publicKey, localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, spec)

      assert(htlcTimeoutTxs.size == 2) // htlc1 and htlc3
      assert(htlcSuccessTxs.size == 2) // htlc2 and htlc4

      {
        // either party spends local->remote htlc output with htlc timeout tx
        for (htlcTimeoutTx <- htlcTimeoutTxs) {
          val localSig = sign(localHtlcPriv)(htlcTimeoutTx)
          val remoteSig = sign(remoteHtlcPriv)(htlcTimeoutTx)
          val signed = addSigs(htlcTimeoutTx, localSig, remoteSig)
          assert(checkValid(signed).isSuccess)
        }
      }

      {
        // remote spends local->remote htlc1/htlc3 output directly in case of success
        for ((htlc, paymentPreimage) <- (htlc1, paymentPreimage1) :: (htlc3, paymentPreimage3) :: Nil) {
          val claimHtlcSuccessTx = makeClaimHtlcSuccessTx(new PubKeyScriptIndexFinder(commitTx.tx), remoteHtlcPriv.publicKey, localHtlcPriv.publicKey, localRevocationPriv.publicKey, finalPubKeyScript, htlc, feeratePerKw, localDustLimit).get
          val localSig = sign(remoteHtlcPriv)(claimHtlcSuccessTx)
          val signed = addSigs(claimHtlcSuccessTx, localSig, paymentPreimage)
          assert(checkValid(signed).isSuccess)
        }
      }

      {
        // local spends remote->local htlc2/htlc4 output with htlc success tx using payment preimage
        for ((htlcSuccessTx, paymentPreimage) <- (htlcSuccessTxs(0), paymentPreimage2) :: (htlcSuccessTxs(1), paymentPreimage4) :: Nil) {
          val localSig = sign(localHtlcPriv)(htlcSuccessTx)
          val remoteSig = sign(remoteHtlcPriv)(htlcSuccessTx)
          val signedTx = addSigs(htlcSuccessTx, localSig, remoteSig, paymentPreimage)
          assert(checkValid(signedTx).isSuccess)
          // check remote sig
          assert(checkSig(htlcSuccessTx, remoteSig, remoteHtlcPriv.publicKey))
        }
      }

      {
        // remote spends main output
        val claimP2WPKHOutputTx = makeClaimP2WPKHOutputTx(commitTx.tx, remotePaymentPriv.publicKey, finalPubKeyScript, feeratePerKw, localDustLimit).get
        val localSig = sign(remotePaymentPriv)(claimP2WPKHOutputTx)
        val signedTx = addSigs(claimP2WPKHOutputTx, localSig, remotePaymentPriv.publicKey)
        assert(checkValid(signedTx).isSuccess)
      }

      {
        // remote spends remote->local htlc output directly in case of timeout
        val claimHtlcTimeoutTx = makeClaimHtlcTimeoutTx(new PubKeyScriptIndexFinder(commitTx.tx), remoteHtlcPriv.publicKey, localHtlcPriv.publicKey, localRevocationPriv.publicKey, finalPubKeyScript, htlc2, feeratePerKw, localDustLimit).get
        val localSig = sign(remoteHtlcPriv)(claimHtlcTimeoutTx)
        val signed = addSigs(claimHtlcTimeoutTx, localSig)
        assert(checkValid(signed).isSuccess)
      }

      {
        // remote spends offered HTLC output with revocation key
        val script = Script.write(Scripts.htlcOffered(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, Crypto.ripemd160(htlc1.paymentHash)))
        val htlcPenaltyTx = makeHtlcPenaltyTx(new PubKeyScriptIndexFinder(commitTx.tx), script, finalPubKeyScript, feeratePerKw, localDustLimit).get
        val sig = sign(localRevocationPriv)(htlcPenaltyTx)
        val signed = addSigs(htlcPenaltyTx, sig, localRevocationPriv.publicKey)
        assert(checkValid(signed).isSuccess)
      }

      {
        // remote spends received HTLC output with revocation key
        val script = Script.write(Scripts.htlcReceived(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, Crypto.ripemd160(htlc2.paymentHash), htlc2.expiry))
        val htlcPenaltyTx = makeHtlcPenaltyTx(new PubKeyScriptIndexFinder(commitTx.tx), script, finalPubKeyScript, feeratePerKw, localDustLimit).get
        val sig = sign(localRevocationPriv)(htlcPenaltyTx)
        val signed = addSigs(htlcPenaltyTx, sig, localRevocationPriv.publicKey)
        assert(checkValid(signed).isSuccess)
      }

    }


    def htlc(incoming: Boolean, amount: Satoshi): Htlc =
      Htlc(incoming, UpdateAddHtlc("00" * 32, 0, amount.amount * 1000, "00" * 32, 144, ""))

    Future {
      println("BOLT 2 fee tests")

      val bolt3 = Source
        .fromURL("https://raw.githubusercontent.com/lightningnetwork/lightning-rfc/master/03-transactions.md")
        .mkString
        .replace("    name:", "$   name:")
      // character '$' separates tests

      // this regex extract params from a given test
      val testRegex = ("""name: (.*)\n""" +
        """.*to_local_msat: ([0-9]+)\n""" +
        """.*to_remote_msat: ([0-9]+)\n""" +
        """.*feerate_per_kw: ([0-9]+)\n""" +
        """.*base commitment transaction fee = ([0-9]+)\n""" +
        """[^$]+""").r
      // this regex extracts htlc direction and amounts
      val htlcRegex =
      """.*HTLC ([a-z]+) amount ([0-9]+).*""".r

      val dustLimit = Satoshi(546)
      case class TestSetup(name: String, dustLimit: Satoshi, spec: CommitmentSpec, expectedFee: Satoshi)

      val tests = testRegex.findAllIn(bolt3).map(s => {
        val testRegex(name, to_local_msat, to_remote_msat, feerate_per_kw, fee) = s
        val htlcs = htlcRegex.findAllIn(s).map(l => {
          val htlcRegex(direction, amount) = l
          direction match {
            case "offered" => htlc(incoming = false, Satoshi(amount.toLong))
            case "received" => htlc(incoming = true, Satoshi(amount.toLong))
          }
        }).toSet
        TestSetup(name, dustLimit, CommitmentSpec(feerate_per_kw.toLong, to_local_msat.toLong, to_remote_msat.toLong, htlcs), Satoshi(fee.toLong))
      })

      // simple non-reg test making sure we are not missing tests
      println(tests.size == 15, "there were 15 tests at ec99f893f320e8c88f564c1c8566f3454f0f1f5f")

      tests.foreach(test => {
        println(s"running BOLT 2 test: '${test.name}'")
        val fee = commitTxFee(test.dustLimit, test.spec)
        assert(fee == test.expectedFee)
      })

      println("SUCCESS")
    }
    

  }
}