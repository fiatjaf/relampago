package com.lightning.walletapp.lnutils

import spray.json._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.{HasNormalCommits, HostedCommits, LNParams}
import com.lightning.walletapp.lnutils.JsonHttpUtils.to
import com.lightning.walletapp.ln.wire.LocalBackups
import android.support.v4.content.ContextCompat
import android.support.v7.app.AppCompatActivity
import com.lightning.walletapp.ChannelManager
import android.support.v4.app.ActivityCompat
import com.lightning.walletapp.helper.AES
import android.content.pm.PackageManager
import com.lightning.walletapp.ln.Tools
import com.google.common.io.Files
import android.os.Environment
import fr.acinq.bitcoin.Block
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

  def channelBackupDirectory = LNParams.chainHash match {
    case Block.LivenetGenesisBlock.hash => "mainnet"
    case Block.TestnetGenesisBlock.hash => "testnet"
    case Block.RegtestGenesisBlock.hash => "regtest"
    case _ => "unknown"
  }

  def getBackupFile: File = {
    val publicDir = new File(Environment.getExternalStorageDirectory, BACKUP_DIR)
    val chainDir = new File(publicDir, channelBackupDirectory)
    val backup = new File(chainDir, BACKUP_FILE_NAME)
    if (!backup.isFile) chainDir.mkdirs
    backup
  }

  def encryptAndWrite(file: File) = {
    val backup = LocalBackups(Vector.empty, Vector.empty, v = 1)
    val encrypted = AES.encReadable(ChannelManager.all.map(_.data).foldLeft(backup) {
      case (LocalBackups(normal, hosted, v), hasNorm: HasNormalCommits) => LocalBackups(normal :+ hasNorm, hosted, v)
      case (LocalBackups(normal, hosted, v), hostedCommits: HostedCommits) => LocalBackups(normal, hosted :+ hostedCommits, v)
      case (updatedLocalBackups, _) => updatedLocalBackups
    }.toJson.toString, LNParams.cloudSecret.toArray)
    Files.write(encrypted.toByteArray, file)
  }

  def readAndDecrypt(file: File): Try[LocalBackups] = for {
    plain <- AES.decBytes(Files.toByteArray(file), LNParams.cloudSecret.toArray)
    decryptedBackup = to[LocalBackups](Tools bin2readable plain)
  } yield decryptedBackup
}
