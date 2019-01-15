package com.lightning.walletapp.test

import com.lightning.walletapp.helper.AES
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.Scripts.InputInfo
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.lnutils.{PaymentInfoWrap, RevokedInfoTable}
import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction, TxOut}
import fr.acinq.bitcoin.Crypto.Point
import scodec.DecodeResult
import scodec.bits.BitVector

class RevokedInfoSpec {
  val chanId1 = BinaryData(random getBytes 32)
  val chanId2 = BinaryData(random getBytes 32)

  val ri = RevocationInfo(redeemScriptsToSigs = Nil, claimMainTxSig = None, claimPenaltyTxSig = None, LNParams.broadcaster.perKwThreeSat,
    LNParams.dust.amount, randomPrivKey.publicKey, 144, randomPrivKey.publicKey, randomPrivKey.publicKey, randomPrivKey.publicKey)

  val txid1 = BinaryData(random getBytes 32)
  val txid2 = BinaryData(random getBytes 32)
  val txid3 = BinaryData(random getBytes 32)
  val txid4 = BinaryData(random getBytes 32)

  val txid5 = BinaryData(random getBytes 32)
  val txid6 = BinaryData(random getBytes 32)
  val txid7 = BinaryData(random getBytes 32)
  val txid8 = BinaryData(random getBytes 32)

  def allTests = {
    val serialized1 = LightningMessageCodecs.serialize(revocationInfoCodec encode ri)
    // Sent 3 outgoing payments in chan1
    db.change(RevokedInfoTable.newSql, txid1, chanId1, 1000000000L, serialized1)
    db.change(RevokedInfoTable.newSql, txid2, chanId1, 800000000L, serialized1)
    db.change(RevokedInfoTable.newSql, txid3, chanId1, 600000000L, serialized1)
    // Then got back an incoming payment in chan1
    db.change(RevokedInfoTable.newSql, txid4, chanId1, 900000000L, serialized1)


    val c1 = Commitments(
      localParams = LocalParams(null, 0, 0, 0, null, null, null, null, null, null, null, null, isFunder = true),
      remoteParams = null,
      LocalCommit(index = 0L, spec = CommitmentSpec(0L, toLocalMsat = 900000000L, toRemoteMsat = 0L), null, null),
      remoteCommit = null,
      localChanges = null,
      remoteChanges = null,
      localNextHtlcId = 0L,
      remoteNextHtlcId = 0L,
      remoteNextCommitInfo = null,
      commitInput = InputInfo(null, TxOut(Satoshi(100000L), Seq.empty), null),
      remotePerCommitmentSecrets = null,
      channelId = chanId1)

    val chan1 = new Channel {
      override def REV(cs: Commitments, rev: RevokeAndAck): Unit = none
      override def SEND(msg: LightningMessage): Unit = none
      override def ASKREFUNDTX(ref: RefundingData): Unit = none
      override def STORE(content: HasCommitments): HasCommitments = content
      override def ASKREFUNDPEER(some: HasCommitments, point: Point): Unit = none
      override def CLOSEANDWATCH(close: ClosingData): Unit = none
      override def CLOSEANDWATCHREVHTLC(cd: ClosingData): Unit = none
      override def GETREV(tx: Transaction): Option[RevokedCommitPublished] = None
      data = NormalData(announce = null, commitments = c1)
    }


    db.change(RevokedInfoTable.newSql, txid5, chanId2, 1000000000L, serialized1)
    // Attempted to send an outgoing payment in chan2
    db.change(RevokedInfoTable.newSql, txid6, chanId2, 500000000L, serialized1)
    // But payment got rejected and refunded so we got our full balance back
    // We also sent another small payment which has also been rejected
    db.change(RevokedInfoTable.newSql, txid7, chanId2, 999999990L, serialized1)
    db.change(RevokedInfoTable.newSql, txid8, chanId2, 1000000000L, serialized1)


    val c2 = Commitments(
      localParams = LocalParams(null, 0, 0, 0, null, null, null, null, null, null, null, null, isFunder = false),
      remoteParams = null,
      LocalCommit(index = 0L, spec = CommitmentSpec(0L, toLocalMsat = 1000000000L, toRemoteMsat = 0L), null, null),
      remoteCommit = null,
      localChanges = null,
      remoteChanges = null,
      localNextHtlcId = 0L,
      remoteNextHtlcId = 0L,
      remoteNextCommitInfo = null,
      commitInput = InputInfo(null, TxOut(Satoshi(100000L), Seq.empty), null),
      remotePerCommitmentSecrets = null,
      channelId = chanId2)

    val chan2 = new Channel {
      override def REV(cs: Commitments, rev: RevokeAndAck): Unit = none
      override def SEND(msg: LightningMessage): Unit = none
      override def ASKREFUNDTX(ref: RefundingData): Unit = none
      override def STORE(content: HasCommitments): HasCommitments = content
      override def ASKREFUNDPEER(some: HasCommitments, point: Point): Unit = none
      override def CLOSEANDWATCH(close: ClosingData): Unit = none
      override def CLOSEANDWATCHREVHTLC(cd: ClosingData): Unit = none
      override def GETREV(tx: Transaction): Option[RevokedCommitPublished] = None
      data = NormalData(announce = null, commitments = c2)
    }

    val reports = Vector(chan1, chan2)
    val cerberusAct = PaymentInfoWrap.getCerberusActs(reports.flatMap(PaymentInfoWrap.getVulnerableRevInfos).toMap).next
    val cerberusPayloadHex = cerberusAct.data.toString

    // Taken from Olympus
    val cerberusPayloadBitVec = BitVector(BinaryData(cerberusPayloadHex).data)
    val cerberusPayloadDecoded = cerberusPayloadCodec decode cerberusPayloadBitVec
    val CerberusPayload(aesZygotes, halfTxIds) = cerberusPayloadDecoded.require.value

    assert(aesZygotes.size == halfTxIds.size)
    assert(Set(txid2.toString take 16, txid3.toString take 16, txid6.toString take 16) == halfTxIds.toSet)

    for {
      // Taken from Olympus
      halfTxId \ aesz <- halfTxIds zip aesZygotes
      fullTxidBin <- halfTxIds.zip(Vector(txid2, txid3)).toMap get halfTxId
      revBitVec <- AES.decZygote(aesz, fullTxidBin) map BitVector.apply
      DecodeResult(ri1, _) <- revocationInfoCodec.decode(revBitVec).toOption
    } assert(ri1 == ri)

    cerberusAct.onDone
    assert(reports.flatMap(PaymentInfoWrap.getVulnerableRevInfos).isEmpty)
  }
}
