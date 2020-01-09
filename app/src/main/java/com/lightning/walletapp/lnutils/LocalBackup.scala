package com.lightning.walletapp.lnutils

import spray.json._
import com.lightning.walletapp.ln._
import scala.collection.JavaConverters._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.JsonHttpUtils.to
import com.lightning.walletapp.ln.wire.LocalBackups
import android.support.v4.content.ContextCompat
import android.support.v7.app.AppCompatActivity
import android.support.v4.app.ActivityCompat
import com.lightning.walletapp.helper.AES
import android.content.pm.PackageManager
import com.google.common.io.Files
import org.bitcoinj.wallet.Wallet
import android.os.Environment
import fr.acinq.bitcoin.Block
import scodec.bits.ByteVector
import scala.util.Try
import java.io.File


object LocalBackup {
  final val BACKUP_DIR = "BLW"
  final val BACKUP_FILE_NAME = "encrypted.channels.bkp"
  final val LOCAL_BACKUP_REQUEST_NUMBER = 105

  def isExternalStorageWritable: Boolean = {
    val isMounted = Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState)
    val isWritable = Environment.getExternalStorageDirectory.canWrite
    isMounted && isWritable
  }

  def askPermission(activity: AppCompatActivity) = ActivityCompat.requestPermissions(activity, Array(android.Manifest.permission.WRITE_EXTERNAL_STORAGE), LOCAL_BACKUP_REQUEST_NUMBER)
  def isAllowed(activity: AppCompatActivity) = ContextCompat.checkSelfPermission(activity, android.Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED

  def getBackupDirectory(chainHash: ByteVector) = chainHash match {
    case Block.LivenetGenesisBlock.hash => "mainnet"
    case Block.TestnetGenesisBlock.hash => "testnet"
    case Block.RegtestGenesisBlock.hash => "regtest"
    case _ => "unknown"
  }

  def getBackupFileUnsafe(channelBackupDirName: String) = {
    val publicDir = new File(Environment.getExternalStorageDirectory, BACKUP_DIR)
    val chainDir = new File(publicDir, channelBackupDirName)
    val backup = new File(chainDir, BACKUP_FILE_NAME)
    if (!backup.isFile) chainDir.mkdirs
    backup
  }

  def encryptAndWrite(file: File, channels: Vector[Channel], wallet: Wallet, secret: ByteVector) = {
    // Collect siutable commitments data and calculate earliest watch timestamp for restored wallet

    // Everything except refunds for which we don't have points
    val datas: Vector[ChannelData] = channels.map(_.data).filter {
      case refund: RefundingData => refund.remoteLatestPoint.isDefined
      case _ => true
    }

    val channelStampsMsec: Vector[Long] = datas.map {
      case wd: WaitFundingDoneData => wd.commitments.startedAt
      case closing: ClosingData => closing.closedAt
      case _ => System.currentTimeMillis
    }

    // When no UTXO is present: current timestamp
    // When UTXO timestamps are known: an oldest timestamp
    // When UTXO is present but timestamps fail: wallet timestamp
    val unspentUtxos = wallet.getUnspents.asScala.filter(_ isMine wallet)
    val oldestUtxoStampMsec = if (unspentUtxos.isEmpty) System.currentTimeMillis else {
      val minStampTry = Try(unspentUtxos.map(_.getParentTransaction.getUpdateTime.getTime).min)
      minStampTry.getOrElse(wallet.getEarliestKeyCreationTime * 1000L)
    }

    // When no channel is present: current timestamp
    val oldestChannelStampMsec = Try(channelStampsMsec.min).getOrElse(System.currentTimeMillis)
    val oldestStampSecs = math.min(oldestUtxoStampMsec, oldestChannelStampMsec) / 1000L - 3600 * 24 * 7

    val backup = (LocalBackups(Vector.empty, Vector.empty, oldestStampSecs, v = 1) /: datas) {
      case (LocalBackups(normal, hosted, _, v), hasNorm: HasNormalCommits) => LocalBackups(normal :+ hasNorm, hosted, oldestStampSecs, v)
      case (LocalBackups(normal, hosted, _, v), hostedCommits: HostedCommits) => LocalBackups(normal, hosted :+ hostedCommits, oldestStampSecs, v)
      case (updatedLocalBackups, _) => updatedLocalBackups
    }

    val encrypted = AES.encReadable(backup.toJson.toString, secret.toArray)
    Files.write(encrypted.toByteArray, file)
  }

  def readAndDecrypt(file: File, secret: ByteVector) = for {
    encryptedLocalBackupByteArray <- Try(Files toByteArray file)
    plaintextJson <- AES.decBytes(encryptedLocalBackupByteArray, secret.toArray)
    decryptedBackup = to[LocalBackups](Tools bin2readable plaintextJson)
  } yield decryptedBackup
}