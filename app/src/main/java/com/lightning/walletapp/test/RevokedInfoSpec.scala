package com.lightning.walletapp.test

import com.lightning.walletapp.helper.AES
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.wire.{CerberusPayload, LightningMessageCodecs}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.lnutils.{PaymentInfoWrap, RevokedInfoTable}
import fr.acinq.bitcoin.BinaryData
import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.DecodeResult
import scodec.bits.BitVector

class RevokedInfoSpec {
  val chanId = BinaryData(random getBytes 32)

  val ri = RevocationInfo(redeemScriptsToSigs = Nil, claimMainTxSig = None, claimPenaltyTxSig = None, LNParams.broadcaster.perKwThreeSat,
    LNParams.dust.amount, randomPrivKey.publicKey, 144, randomPrivKey.publicKey, randomPrivKey.publicKey, randomPrivKey.publicKey)

  val txid1 = BinaryData(random getBytes 32)
  val txid2 = BinaryData(random getBytes 32)
  val txid3 = BinaryData(random getBytes 32)
  val txid4 = BinaryData(random getBytes 32)

  def allTests = {
    val serialized1 = LightningMessageCodecs.serialize(revocationInfoCodec encode ri)
    db.change(RevokedInfoTable.newSql, txid1, chanId, 1000000000L, serialized1)
    db.change(RevokedInfoTable.newSql, txid2, chanId, 800000000L, serialized1)
    db.change(RevokedInfoTable.newSql, txid3, chanId, 600000000L, serialized1)
    db.change(RevokedInfoTable.newSql, txid4, chanId, 900000000L, serialized1)

    val cerberusPayloadHex = PaymentInfoWrap.getVulnerableRevInfos(900000000L, chanId).next.data.toString

    // Taken from Olympus
    val cerberusPayloadBitVec = BitVector(BinaryData(cerberusPayloadHex).data)
    val cerberusPayloadDecoded = cerberusPayloadCodec decode cerberusPayloadBitVec
    val CerberusPayload(aesZygotes, halfTxIds) = cerberusPayloadDecoded.require.value

    assert(aesZygotes.size == halfTxIds.size)
    assert(Set(txid2.toString take 16, txid3.toString take 16) == halfTxIds.toSet)

    for {
    // Taken from Olympus
      halfTxId \ aesz <- halfTxIds zip aesZygotes
      fullTxidBin <- halfTxIds.zip(Vector(txid2, txid3)).toMap get halfTxId
      revBitVec <- AES.decZygote(aesz, fullTxidBin) map BitVector.apply
      DecodeResult(ri1, _) <- revocationInfoCodec.decode(revBitVec).toOption
    } assert(ri1 == ri)
  }
}
