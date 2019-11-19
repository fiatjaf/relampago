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

  def getBackupFileUnsafe(channelBackupDirName: String, force: Boolean) = {
    val publicDir = new File(Environment.getExternalStorageDirectory, BACKUP_DIR)
    val chainDir = new File(publicDir, channelBackupDirName)
    val backup = new File(chainDir, BACKUP_FILE_NAME)
    if (force && !backup.isFile) chainDir.mkdirs
    backup
  }

  def encryptAndWrite(file: File, datas: Vector[ChannelData], wallet: Wallet, secret: ByteVector) = {
    // Collect all channels with commitments and calculate earliest watch timestap for restored wallet

    val unspentUtxos = wallet.getUnspents.asScala.filter(_ isMine wallet)
    val utxoMsecOpt = Try { if (unspentUtxos.isEmpty) System.currentTimeMillis else unspentUtxos.map { utxo => utxo.getParentTransaction.getUpdateTime.getTime }.min }
    val channelsMsecOpt = Try { datas.map { case cd: ClosingData => cd.closedAt case wd: WaitFundingDoneData => wd.commitments.startedAt case _ => System.currentTimeMillis }.min }
    val earliest = math.min(channelsMsecOpt getOrElse System.currentTimeMillis, utxoMsecOpt getOrElse wallet.getEarliestKeyCreationTime * 1000L) / 1000L - 3600 * 24 * 7

    val backup = (LocalBackups(Vector.empty, Vector.empty, earliest, v = 1) /: datas) {
      case (LocalBackups(normal, hosted, _, v), hasNorm: HasNormalCommits) => LocalBackups(normal :+ hasNorm, hosted, earliest, v)
      case (LocalBackups(normal, hosted, _, v), hostedCommits: HostedCommits) => LocalBackups(normal, hosted :+ hostedCommits, earliest, v)
      case (updatedLocalBackups, _) => updatedLocalBackups
    }

    val encrypted = AES.encReadable(backup.toJson.toString, secret.toArray)
    Files.write(encrypted.toByteArray, file)
  }

  def readAndDecrypt(file: File, secret: ByteVector) = for {
    encryptedlOCALBackupByteArray <- Try(Files toByteArray file)
    plaintextJson <- AES.decBytes(encryptedlOCALBackupByteArray, secret.toArray)
    decryptedBackup = to[LocalBackups](Tools bin2readable plaintextJson)
  } yield decryptedBackup
}
