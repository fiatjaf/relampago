package com.lightning.walletapp.lnutils

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Scripts._
import com.lightning.walletapp.lnutils.olympus._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.ln.crypto.ShaHashesWithIndex
import com.lightning.walletapp.ln.crypto.ShaChain.Index
import com.lightning.walletapp.ln.Tools.Bytes
import fr.acinq.eclair.UInt64
import java.math.BigInteger
import scodec.Codec

import com.lightning.walletapp.{IncomingChannelRequest, LNUrlData, WithdrawRequest}
import com.lightning.walletapp.ln.Helpers.Closing.{SuccessAndClaim, TimeoutAndClaim}
import com.lightning.walletapp.ln.CommitmentSpec.{HtlcAndFail, HtlcAndFulfill}
import fr.acinq.bitcoin.{MilliSatoshi, OutPoint, Satoshi, Transaction, TxOut}
import fr.acinq.bitcoin.Crypto.{Point, PrivateKey, PublicKey, Scalar}
import scodec.bits.{BitVector, ByteVector}


object ImplicitJsonFormats extends DefaultJsonProtocol { me =>
  val json2String = (_: JsValue).convertTo[String]
  private val TAG = "tag"

  def json2BitVec(json: JsValue): Option[BitVector] = BitVector fromHex json2String(json)
  def sCodecJsonFmt[T](codec: Codec[T] /* Json <-> sCodec bridge */) = new JsonFormat[T] {
    def read(serialized: JsValue) = codec.decode(json2BitVec(serialized).get).require.value
    def write(unserialized: T) = codec.encode(unserialized).require.toHex.toJson
  }

  def writeExt[T](ext: (String, JsValue), base: JsValue) =
    JsObject(base.asJsObject.fields + ext)

  def taggedJsonFmt[T](base: JsonFormat[T], tag: String) = new JsonFormat[T] {
    def write(unserialized: T) = writeExt(TAG -> JsString(tag), base write unserialized)
    def read(serialized: JsValue) = base read serialized
  }

  implicit object BigIntegerFmt extends JsonFormat[BigInteger] {
    def read(json: JsValue): BigInteger = new BigInteger(me json2String json)
    def write(internal: BigInteger): JsValue = internal.toString.toJson
  }

  implicit object TransactionFmt extends JsonFormat[Transaction] {
    def read(json: JsValue): Transaction = Transaction.read(me json2String json)
    def write(internal: Transaction): JsValue = internal.bin.toHex.toJson
  }

  implicit object PublicKeyFmt extends JsonFormat[PublicKey] {
    def read(json: JsValue): PublicKey = PublicKey.fromValidHex(me json2String json)
    def write(internal: PublicKey): JsValue = internal.toBin.toHex.toJson
  }

  implicit object ShaHashesWithIndexFmt
  extends JsonFormat[ShaHashesWithIndex] {

    def read(json: JsValue): ShaHashesWithIndex = json match {
      case JsArray(hashesIndexBytesSeq +: lastIndexOption +: _) =>
        val lastIndexLongOption = lastIndexOption.convertTo[LongOption]
        val hashesIndexBytesMap = hashesIndexBytesSeq.convertTo[IndexBytesSeq]
        ShaHashesWithIndex(hashesIndexBytesMap.toMap, lastIndexLongOption)
      case _ => throw new RuntimeException
    }

    def write(internal: ShaHashesWithIndex): JsValue =
      JsArray(internal.hashes.toSeq.toJson, internal.lastIndex.toJson)

    type LongOption = Option[Long]
    type IndexBytes = (Index, Bytes)
    type IndexBytesSeq = Seq[IndexBytes]
  }

  implicit val stateUpdateFmt = sCodecJsonFmt(stateUpdateCodec)
  implicit val initHostedChannelFmt = sCodecJsonFmt(initHostedChannelCodec)
  implicit val lastCrossSignedStateFmt = sCodecJsonFmt(lastCrossSignedStateCodec)

  implicit val lightningMessageFmt = sCodecJsonFmt(lightningMessageCodec)
  implicit val nodeAnnouncementFmt = sCodecJsonFmt(nodeAnnouncementCodec)
  implicit val updateFulfillHtlcFmt = sCodecJsonFmt(updateFulfillHtlcCodec)
  implicit val updateFailHtlcFmt = sCodecJsonFmt(updateFailHtlcCodec)
  implicit val acceptChannelFmt = sCodecJsonFmt(acceptChannelCodec)
  implicit val updateAddHtlcFmt = sCodecJsonFmt(updateAddHtlcCodec)
  implicit val closingSignedFmt = sCodecJsonFmt(closingSignedCodec)
  implicit val fundingLockedFmt = sCodecJsonFmt(fundingLockedCodec)
  implicit val channelUpdateFmt = sCodecJsonFmt(channelUpdateCodec)
  implicit val channelFlagsFmt = sCodecJsonFmt(channelFlagsCodec)
  implicit val byteVectorFmt = sCodecJsonFmt(scodec.codecs.bytes)
  implicit val commitSigFmt = sCodecJsonFmt(commitSigCodec)
  implicit val shutdownFmt = sCodecJsonFmt(shutdownCodec)
  implicit val errorFmt = sCodecJsonFmt(errorCodec)
  implicit val uint64Fmt = sCodecJsonFmt(uint64)
  implicit val hopFmt = sCodecJsonFmt(hopCodec)
  implicit val pointFmt = sCodecJsonFmt(point)

  implicit val blindParamFmt =
    jsonFormat[Bytes, BigInteger, BigInteger, BigInteger, BigInteger,
      BlindParam](BlindParam.apply, "point", "a", "b", "c", "bInv")

  implicit val blindMemoFmt =
    jsonFormat[List[BlindParam], List[BigInteger], String,
      BlindMemo](BlindMemo.apply, "params", "clears", "key")

  implicit val scalarFmt = jsonFormat[BigInteger, Scalar](Scalar.apply, "value")
  implicit val privateKeyFmt = jsonFormat[Scalar, Boolean, PrivateKey](PrivateKey.apply, "value", "compressed")
  implicit val milliSatoshiFmt = jsonFormat[Long, MilliSatoshi](MilliSatoshi.apply, "amount")
  implicit val satoshiFmt = jsonFormat[Long, Satoshi](Satoshi.apply, "amount")

  // LNURL

  implicit object LNUrlDataFmt extends JsonFormat[LNUrlData] {
    def write(unserialized: LNUrlData): JsValue = unserialized match {
      case unserialiedMessage: IncomingChannelRequest => unserialiedMessage.toJson
      case unserialiedMessage: WithdrawRequest => unserialiedMessage.toJson
    }

    def read(serialized: JsValue): LNUrlData = serialized.asJsObject fields TAG match {
      case JsString("channelRequest") => serialized.convertTo[IncomingChannelRequest]
      case JsString("withdrawRequest") => serialized.convertTo[WithdrawRequest]
      case _ => throw new RuntimeException
    }
  }

  implicit val incomingChannelRequestFmt = taggedJsonFmt(jsonFormat[String, String, String,
    IncomingChannelRequest](IncomingChannelRequest.apply, "uri", "callback", "k1"),
    tag = "channelRequest")

  implicit val withdrawRequestFmt = taggedJsonFmt(jsonFormat[String, String, Long, String, Option[Long],
    WithdrawRequest](WithdrawRequest.apply, "callback", "k1", "maxWithdrawable", "defaultDescription",
    "minWithdrawable"), tag = "withdrawRequest")

  // Channel data

  implicit val outPointFmt = jsonFormat[ByteVector, Long, OutPoint](OutPoint.apply, "hash", "index")
  implicit val txOutFmt = jsonFormat[Satoshi, ByteVector, TxOut](TxOut.apply, "amount", "publicKeyScript")
  implicit val inputInfoFmt = jsonFormat[OutPoint, TxOut, ByteVector, InputInfo](InputInfo.apply, "outPoint", "txOut", "redeemScript")

  implicit object TransactionWithInputInfoFmt
  extends JsonFormat[TransactionWithInputInfo] {

    def read(json: JsValue) =
      json.asJsObject fields TAG match {
        case JsString("CommitTx") => json.convertTo[CommitTx]
        case JsString("HtlcSuccessTx") => json.convertTo[HtlcSuccessTx]
        case JsString("HtlcTimeoutTx") => json.convertTo[HtlcTimeoutTx]
        case JsString("ClaimHtlcSuccessTx") => json.convertTo[ClaimHtlcSuccessTx]
        case JsString("ClaimHtlcTimeoutTx") => json.convertTo[ClaimHtlcTimeoutTx]
        case JsString("ClaimP2WPKHOutputTx") => json.convertTo[ClaimP2WPKHOutputTx]
        case JsString("ClaimDelayedOutputTx") => json.convertTo[ClaimDelayedOutputTx]
        case JsString("ClaimDelayedOutputPenaltyTx") => json.convertTo[ClaimDelayedOutputPenaltyTx]
        case JsString("MainPenaltyTx") => json.convertTo[MainPenaltyTx]
        case JsString("HtlcPenaltyTx") => json.convertTo[HtlcPenaltyTx]
        case JsString("ClosingTx") => json.convertTo[ClosingTx]
        case _ => throw new RuntimeException
      }

    def write(internal: TransactionWithInputInfo) = internal match {
      case transactionWithInputInfo: CommitTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: HtlcSuccessTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: HtlcTimeoutTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: ClaimHtlcSuccessTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: ClaimHtlcTimeoutTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: ClaimP2WPKHOutputTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: ClaimDelayedOutputTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: ClaimDelayedOutputPenaltyTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: MainPenaltyTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: HtlcPenaltyTx => transactionWithInputInfo.toJson
      case transactionWithInputInfo: ClosingTx => transactionWithInputInfo.toJson
    }
  }

  implicit val commitTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction,
    CommitTx](CommitTx.apply, "input", "tx"), tag = "CommitTx")

  implicit val htlcSuccessTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction, UpdateAddHtlc,
    HtlcSuccessTx](HtlcSuccessTx.apply, "input", "tx", "add"), tag = "HtlcSuccessTx")

  implicit val htlcTimeoutTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction, UpdateAddHtlc,
    HtlcTimeoutTx](HtlcTimeoutTx.apply, "input", "tx", "add"), tag = "HtlcTimeoutTx")

  implicit val claimHtlcSuccessTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction,
    ClaimHtlcSuccessTx](ClaimHtlcSuccessTx.apply, "input", "tx"), tag = "ClaimHtlcSuccessTx")

  implicit val claimHtlcTimeoutTxFmt = taggedJsonFmt(jsonFormat[Option[UpdateAddHtlc], InputInfo, Transaction,
    ClaimHtlcTimeoutTx](ClaimHtlcTimeoutTx.apply, "addOpt", "input", "tx"), tag = "ClaimHtlcTimeoutTx")

  implicit val claimP2WPKHOutputTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction,
    ClaimP2WPKHOutputTx](ClaimP2WPKHOutputTx.apply, "input", "tx"), tag = "ClaimP2WPKHOutputTx")

  implicit val claimDelayedOutputTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction,
    ClaimDelayedOutputTx](ClaimDelayedOutputTx.apply, "input", "tx"), tag = "ClaimDelayedOutputTx")

  implicit val mainPenaltyTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction,
    MainPenaltyTx](MainPenaltyTx.apply, "input", "tx"), tag = "MainPenaltyTx")

  implicit val htlcPenaltyTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction,
    HtlcPenaltyTx](HtlcPenaltyTx.apply, "input", "tx"), tag = "HtlcPenaltyTx")

  implicit val claimDelayedOutputPenaltyTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction,
    ClaimDelayedOutputPenaltyTx](ClaimDelayedOutputPenaltyTx.apply, "input", "tx"), tag = "ClaimDelayedOutputPenaltyTx")

  implicit val closingTxFmt = taggedJsonFmt(jsonFormat[InputInfo, Transaction,
    ClosingTx](ClosingTx.apply, "input", "tx"), tag = "ClosingTx")

  implicit val localParamsFmt =
    jsonFormat[UInt64, Long, Int, Int, PrivateKey, Scalar, Scalar, Scalar, Scalar, ByteVector, Satoshi, ByteVector, Boolean,
      LocalParams](LocalParams.apply, "maxHtlcValueInFlightMsat", "channelReserveSat", "toSelfDelay", "maxAcceptedHtlcs", "fundingPrivKey",
      "revocationSecret", "paymentKey", "delayedPaymentKey", "htlcKey", "defaultFinalScriptPubKey", "dustLimit", "shaSeed", "isFunder")

  implicit val htlcFmt = jsonFormat[Boolean, UpdateAddHtlc, Htlc](Htlc.apply, "incoming", "add")
  implicit val commitmentSpecFmt = jsonFormat[Long, Long, Long, Set[Htlc], Set[HtlcAndFulfill], Set[HtlcAndFail], Set[Htlc],
    CommitmentSpec](CommitmentSpec.apply, "feeratePerKw", "toLocalMsat", "toRemoteMsat", "htlcs", "fulfilled", "failed", "malformed")

  implicit val htlcTxAndSigs = jsonFormat[TransactionWithInputInfo, ByteVector, ByteVector,
    HtlcTxAndSigs](HtlcTxAndSigs.apply, "txinfo", "localSig", "remoteSig")

  implicit val localCommitFmt = jsonFormat[Long, CommitmentSpec, Seq[HtlcTxAndSigs], CommitTx,
    LocalCommit](LocalCommit.apply, "index", "spec", "htlcTxsAndSigs", "commitTx")

  implicit val remoteCommitFmt = jsonFormat[Long, CommitmentSpec, Option[Transaction], Point,
    RemoteCommit](RemoteCommit.apply, "index", "spec", "txOpt", "remotePerCommitmentPoint")

  implicit val waitingForRevocationFmt = jsonFormat[RemoteCommit, CommitSig, Long,
    WaitingForRevocation](WaitingForRevocation.apply, "nextRemoteCommit", "sent",
    "localCommitIndexSnapshot")

  implicit val changesFmt =
    jsonFormat[LNMessageVector, LNMessageVector, LNMessageVector,
      Changes](Changes.apply, "proposed", "signed", "acked")

  implicit val hostedCommitsFmt = jsonFormat[NodeAnnouncement, LastCrossSignedState, Long, Long, Int,
    LNMessageVector, LNMessageVector, CommitmentSpec, Option[ChannelUpdate], Option[Error], Option[Error], Long,
    HostedCommits](HostedCommits.apply, "announce", "lastLocalCrossSignedState", "allLocalUpdatesSoFar", "allRemoteUpdatesSoFar",
    "reSentUpdates", "localChanges", "remoteChanges", "localSpec", "updateOpt", "localError", "remoteError", "startedAt")

  implicit val normalCommitsFmt = jsonFormat[LocalParams, AcceptChannel, LocalCommit, RemoteCommit, Changes, Changes, Long, Long,
    Either[WaitingForRevocation, Point], InputInfo, ShaHashesWithIndex, ByteVector, Option[ChannelUpdate], Option[ChannelFlags], Long,
    NormalCommits](NormalCommits.apply, "localParams", "remoteParams", "localCommit", "remoteCommit", "localChanges", "remoteChanges",
    "localNextHtlcId", "remoteNextHtlcId", "remoteNextCommitInfo", "commitInput", "remotePerCommitmentSecrets", "channelId",
    "updateOpt", "channelFlags", "startedAt")

  implicit val localCommitPublishedFmt =
    jsonFormat[Seq[ClaimDelayedOutputTx], Seq[SuccessAndClaim], Seq[TimeoutAndClaim], Transaction,
      LocalCommitPublished](LocalCommitPublished.apply, "claimMainDelayed", "claimHtlcSuccess",
      "claimHtlcTimeout", "commitTx")

  implicit val remoteCommitPublishedFmt =
    jsonFormat[Seq[ClaimP2WPKHOutputTx], Seq[ClaimHtlcSuccessTx], Seq[ClaimHtlcTimeoutTx], Transaction,
      RemoteCommitPublished](RemoteCommitPublished.apply, "claimMain", "claimHtlcSuccess",
      "claimHtlcTimeout", "commitTx")

  implicit val revokedCommitPublishedFmt =
    jsonFormat[Seq[ClaimP2WPKHOutputTx], Seq[MainPenaltyTx], Seq[HtlcPenaltyTx], Transaction,
      RevokedCommitPublished](RevokedCommitPublished.apply, "claimMain", "claimTheirMainPenalty",
      "htlcPenalty", "commitTx")

  implicit object HasNormalCommitsFmt
  extends JsonFormat[HasNormalCommits] {
    def read(json: JsValue) = json.asJsObject fields TAG match {
      case JsString("WaitBroadcastRemoteData") => json.convertTo[WaitBroadcastRemoteData]
      case JsString("WaitFundingDoneData") => json.convertTo[WaitFundingDoneData]
      case JsString("NegotiationsData") => json.convertTo[NegotiationsData]
      case JsString("RefundingData") => json.convertTo[RefundingData]
      case JsString("ClosingData") => json.convertTo[ClosingData]
      case JsString("NormalData") => json.convertTo[NormalData]
      case _ => throw new RuntimeException
    }

    def write(internal: HasNormalCommits) = internal match {
      case hasCommitments: WaitBroadcastRemoteData => hasCommitments.toJson
      case hasCommitments: WaitFundingDoneData => hasCommitments.toJson
      case hasCommitments: NegotiationsData => hasCommitments.toJson
      case hasCommitments: RefundingData => hasCommitments.toJson
      case hasCommitments: ClosingData => hasCommitments.toJson
      case hasCommitments: NormalData => hasCommitments.toJson
    }
  }

  implicit val closingTxProposedFmt = jsonFormat[ClosingTx, ClosingSigned,
    ClosingTxProposed](ClosingTxProposed.apply, "unsignedTx", "localClosingSigned")

  implicit val refundingDataFmt = taggedJsonFmt(jsonFormat[NodeAnnouncement, Option[Point], NormalCommits,
    RefundingData](RefundingData.apply, "announce", "remoteLatestPoint", "commitments"), tag = "RefundingData")

  implicit val closingDataFmt = taggedJsonFmt(jsonFormat[NodeAnnouncement,
    NormalCommits, Seq[ClosingTxProposed], Seq[Transaction], Seq[LocalCommitPublished],
    Seq[RemoteCommitPublished], Seq[RemoteCommitPublished], Seq[RemoteCommitPublished], Seq[RevokedCommitPublished], Long,
    ClosingData](ClosingData.apply, "announce", "commitments", "localProposals", "mutualClose", "localCommit", "remoteCommit",
    "nextRemoteCommit", "refundRemoteCommit", "revokedCommit", "closedAt"), tag = "ClosingData")

  implicit val negotiationsDataFmt =
    taggedJsonFmt(jsonFormat[NodeAnnouncement, NormalCommits, Shutdown, Shutdown, Seq[ClosingTxProposed], Option[ClosingTx],
      NegotiationsData](NegotiationsData.apply, "announce", "commitments", "localShutdown", "remoteShutdown", "localProposals", "lastSignedTx"),
      tag = "NegotiationsData")

  implicit val normalDataFmt =
    taggedJsonFmt(jsonFormat[NodeAnnouncement, NormalCommits, Option[Shutdown], Option[Shutdown], Option[Transaction],
      NormalData](NormalData.apply, "announce", "commitments", "localShutdown", "remoteShutdown", "unknownSpend"),
      tag = "NormalData")

  implicit val waitFundingDoneDataFmt =
    taggedJsonFmt(jsonFormat[NodeAnnouncement, Option[FundingLocked], Option[FundingLocked], Transaction, NormalCommits,
      WaitFundingDoneData](WaitFundingDoneData.apply, "announce", "our", "their", "fundingTx", "commitments"),
      tag = "WaitFundingDoneData")

  implicit val waitFundingSignedCoreFmt =
    jsonFormat[LocalParams, ByteVector, Option[ChannelFlags], AcceptChannel, CommitmentSpec, RemoteCommit,
      WaitFundingSignedCore](WaitFundingSignedCore.apply, "localParams", "channelId", "channelFlags",
      "remoteParams", "localSpec", "remoteCommit")

  implicit val waitBroadcastRemoteDataFmt =
    taggedJsonFmt(jsonFormat[NodeAnnouncement, WaitFundingSignedCore, NormalCommits, Option[FundingLocked], Option[String],
      WaitBroadcastRemoteData](WaitBroadcastRemoteData.apply, "announce", "core", "commitments", "their", "fundingError"),
      tag = "WaitBroadcastRemoteData")

  implicit val outRequestFmt = jsonFormat[Long, Set[String], Set[Long], Set[String], String,
      OutRequest](OutRequest.apply, "sat", "badNodes", "badChans", "from", "to")

  // Payment request, tags, upload acts, backups

  implicit object TagFmt extends JsonFormat[Tag] {
    def read(json: JsValue): Tag = PaymentRequest.Tag parse json.convertTo[Bytes]
    def write(internal: Tag): JsValue = internal.toInt5s.toJson
  }

  implicit val paymentRequestFmt =
    jsonFormat[String, Option[MilliSatoshi], Long, PublicKey, Vector[Tag], ByteVector,
      PaymentRequest](PaymentRequest.apply, "prefix", "amount", "timestamp", "nodeId", "tags", "signature")

  implicit object CloudActFmt extends JsonFormat[CloudAct] {
    def write(unserialized: CloudAct): JsValue = unserialized match {
      case unserialiedMessage: ChannelUploadAct => unserialiedMessage.toJson
      case unserialiedMessage: TxUploadAct => unserialiedMessage.toJson
      case unserialiedMessage: CerberusAct => unserialiedMessage.toJson
      case unserialiedMessage: LegacyAct => unserialiedMessage.toJson
    }

    def read(serialized: JsValue): CloudAct = serialized.asJsObject.fields get TAG match {
      case Some(s: JsString) if s.value == "ChannelUploadAct" => serialized.convertTo[ChannelUploadAct]
      case Some(s: JsString) if s.value == "TxUploadAct" => serialized.convertTo[TxUploadAct]
      case Some(s: JsString) if s.value == "CerberusAct" => serialized.convertTo[CerberusAct]
      case _ => serialized.convertTo[LegacyAct]
    }
  }

  implicit val legacyActFmt =
    jsonFormat[ByteVector, Seq[HttpParam], String,
      LegacyAct](LegacyAct.apply, "data", "plus", "path")

  implicit val cerberusActFmt = taggedJsonFmt(jsonFormat[ByteVector, Seq[HttpParam], String, StringVec,
    CerberusAct](CerberusAct.apply, "data", "plus", "path", "txids"), tag = "CerberusAct")

  implicit val txUploadActFmt = taggedJsonFmt(jsonFormat[ByteVector, Seq[HttpParam], String,
    TxUploadAct](TxUploadAct.apply, "data", "plus", "path"), tag = "TxUploadAct")

  implicit val channelUploadActFmt = taggedJsonFmt(jsonFormat[ByteVector, Seq[HttpParam], String, String,
    ChannelUploadAct](ChannelUploadAct.apply, "data", "plus", "path", "alias"), tag = "ChannelUploadAct")

  implicit val gDriveBackup = jsonFormat[Vector[HasNormalCommits], Int, GDriveBackup](GDriveBackup.apply, "chans", "v")
  implicit val cloudDataFmt = jsonFormat[Option[RequestAndMemo], Vector[ClearToken], Vector[CloudAct], CloudData](CloudData.apply, "info", "tokens", "acts")
  implicit val ratesFmt = jsonFormat[Seq[Double], Seq[Double], Fiat2Btc, Long, Rates](Rates.apply, "feesSix", "feesThree", "exchange", "stamp")
  implicit val topNodesFmt = jsonFormat[StringVec, Long, TopNodes](TopNodes.apply, "nodes", "stamp")
}