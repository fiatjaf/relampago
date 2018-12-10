package com.lightning.walletapp.lnutils

import spray.json._
import com.google.android.gms.drive._
import com.google.android.gms.tasks._
import com.google.android.gms.drive.query._
import com.google.android.gms.auth.api.signin._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import com.lightning.walletapp.helper.{AES, RichCursor}
import androidx.work.{Data, OneTimeWorkRequest, Worker, WorkerParameters}
import com.lightning.walletapp.lnutils.olympus.OlympusWrap.toCloud
import com.google.android.gms.common.GoogleApiAvailability
import com.lightning.walletapp.ln.crypto.MultiStreamUtils
import com.lightning.walletapp.lnutils.JsonHttpUtils.to
import com.google.android.gms.common.ConnectionResult
import com.lightning.walletapp.ln.Tools.bin2readable
import com.lightning.walletapp.ln.wire.GDriveBackup
import androidx.work.ListenableWorker.Result
import com.lightning.walletapp.AbstractKit
import com.google.common.collect.Sets
import java.util.concurrent.TimeUnit
import fr.acinq.bitcoin.BinaryData
import android.content.Context
import scala.util.Try


object GDrive {
  private[this] val scopes = Sets.newHashSet(Drive.SCOPE_APPFOLDER, Drive.SCOPE_FILE)
  def driveResClient(ctxt: Context)(signInAccount: GoogleSignInAccount) = Drive.getDriveResourceClient(ctxt, signInAccount)
  def syncClientTask(ctxt: Context)(signInAccount: GoogleSignInAccount) = Drive.getDriveClient(ctxt, signInAccount).requestSync
  def isMissing(ctxt: Context) = GoogleApiAvailability.getInstance.isGooglePlayServicesAvailable(ctxt) != ConnectionResult.SUCCESS

  def updatePreferences(ctxt: Context, isEnabled: Boolean, lastSave: Long) =
    ctxt.getSharedPreferences("prefs", Context.MODE_PRIVATE).edit
      .putBoolean(AbstractKit.GDRIVE_ENABLED, isEnabled)
      .putLong(AbstractKit.GDRIVE_LAST_SAVE, lastSave)
      .commit

  def signInAttemptClient(ctxt: Context) =
    GoogleSignIn.getClient(ctxt, (new GoogleSignInOptions.Builder)
      .requestScopes(Drive.SCOPE_APPFOLDER).requestScopes(Drive.SCOPE_FILE)
      .requestId.requestEmail.build)

  def signInAccount(ctxt: Context) =
    Option(GoogleSignIn getLastSignedInAccount ctxt)
      .filter(_.getGrantedScopes containsAll scopes)

  def encrypt(ctxt: Context, secret: BinaryData, dbFileName: String) = {
    // Obtain all channels and all storage tokens and encode a backup blob

    val db1 = new LNOpenHelper(ctxt, dbFileName)
    val tokensBackup = RichCursor(db1 select OlympusTable.selectAllSql).vec(cd => toCloud(cd).snapshot)
    val backup = GDriveBackup(chans = ChannelWrap doGet db1, tokensBackup, v = 1).toJson.toString
    AES.encReadable(backup, secret).toByteArray
  }

  def decrypt(contents: DriveContents, secret: BinaryData) = for {
    encryptedData <- Try(MultiStreamUtils readone contents.getInputStream)
    backup <- AES.decBytes(encryptedData, secret) map bin2readable map to[GDriveBackup]
  } yield backup

  def getMetaTask(folderTask: Task[DriveFolder], res: DriveResourceClient, fileName: String) =
    new TaskWrap[DriveFolder, MetadataBuffer] sContinueWithTask folderTask apply { folderTaskReady =>
      val sortOrder = (new SortOrder.Builder).addSortDescending(SortableField.MODIFIED_DATE).build
      val query = new Query.Builder addFilter Filters.eq(SearchableField.TITLE, fileName)
      res.queryChildren(folderTaskReady.getResult, query.setSortOrder(sortOrder).build)
    }

  def getFileTask(metaTask: Task[MetadataBuffer], res: DriveResourceClient) =
    new TaskWrap[MetadataBuffer, DriveContents] sContinueWithTask metaTask apply { metaTaskReady =>
      res.openFile(metaTaskReady.getResult.get(0).getDriveId.asDriveFile, DriveFile.MODE_READ_ONLY)
    }

  def createBackupTask(ctxt: Context, res: DriveResourceClient, dbFileName: String,
                       backupFileName: String, secret: BinaryData) = {

    val appFolderTask = res.getAppFolder
    val contentsTask = res.createContents

    new TaskWrap[Void, DriveFile] sContinueWithTask Tasks.whenAll(appFolderTask, contentsTask) apply { _ =>
      val changeSet = (new MetadataChangeSet.Builder).setTitle(backupFileName).setMimeType("application/octet-stream")
      MultiStreamUtils.writeone(encrypt(ctxt, secret, dbFileName), contentsTask.getResult.getOutputStream)
      res.createFile(appFolderTask.getResult, changeSet.build, contentsTask.getResult)
    }
  }

  def updateBackupTask(ctxt: Context, res: DriveResourceClient, dbFileName: String, driveFile: DriveFile, secret: BinaryData) =
    new TaskWrap[DriveContents, Void] sContinueWithTask res.openFile(driveFile, DriveFile.MODE_WRITE_ONLY) apply { contentsTask =>
      MultiStreamUtils.writeone(encrypt(ctxt, secret, dbFileName), contentsTask.getResult.getOutputStream)
      res.commitContents(contentsTask.getResult, null)
    }

  def createOrUpdateBackup(ctxt: Context, dbFileName: String, backupFileName: String,
                           secret: BinaryData, drc: DriveResourceClient) = Try {

    val buffer = Tasks await getMetaTask(drc.getAppFolder, drc, backupFileName)
    if (0 == buffer.getCount) Tasks await createBackupTask(ctxt, drc, dbFileName, backupFileName, secret)
    else Tasks await updateBackupTask(ctxt, drc, dbFileName, buffer.get(0).getDriveId.asDriveFile, secret)
  }
}

object TaskWrap {
  def onSuccess[T](fun: T => Unit) = new OnSuccessListener[T] { def onSuccess(result: T) = fun apply result }
  def onFailure(fun: Exception => Unit) = new OnFailureListener { def onFailure(exc: Exception): Unit = fun apply exc }
}

class TaskWrap[S, R] {
  type TaskSource = Task[S]
  type TaskResult = Task[R]
  def cont(fun: TaskSource => TaskResult) = new Continuation[S, TaskResult] { def `then`(ts: TaskSource) = fun apply ts }
  val sContinueWithTask = (task: TaskSource) => (continuation: TaskSource => TaskResult) => task continueWithTask cont(continuation)
}

object BackupWorker {
  val SECRET = "secret"
  val DB_FILE_NAME = "dbFileName"
  val BACKUP_FILE_NAME = "backupFileName"
  private[this] val bwClass = classOf[BackupWorker]

  def workRequest(dbFileName: String, backupFileName: String, secret: BinaryData) = {
    val bld = (new Data.Builder).putString(DB_FILE_NAME, dbFileName).putString(BACKUP_FILE_NAME, backupFileName).putString(SECRET, secret.toString)
    new OneTimeWorkRequest.Builder(bwClass).setInputData(bld.build).setInitialDelay(5, TimeUnit.SECONDS).addTag("ChannelsBackupWork").build
  }
}

class BackupWorker(ctxt: Context, params: WorkerParameters) extends Worker(ctxt, params) {
  // Attempt to save channel state dat aand storage tokens on a gdrive server

  def doWork: Result = if (GDrive isMissing ctxt) Result.SUCCESS else {
    val prefs = ctxt.getSharedPreferences("prefs", Context.MODE_PRIVATE)
    val isEnabled = prefs.getBoolean(AbstractKit.GDRIVE_ENABLED, true)
    if (!isEnabled) return Result.SUCCESS

    val secret = getInputData.getString(BackupWorker.SECRET)
    val dbFileName = getInputData.getString(BackupWorker.DB_FILE_NAME)
    val backupFileName = getInputData.getString(BackupWorker.BACKUP_FILE_NAME)
    if (null == secret || null == dbFileName || null == backupFileName) return Result.FAILURE
    val dcrOpt = GDrive.signInAccount(ctxt) map GDrive.driveResClient(ctxt)

    dcrOpt match {
      case Some(dcr) =>
        val res = GDrive.createOrUpdateBackup(ctxt, dbFileName, backupFileName, BinaryData(secret), dcr)
        GDrive.updatePreferences(ctxt, res.isSuccess, if (res.isSuccess) System.currentTimeMillis else -1L)
        if (res.isSuccess) Result.SUCCESS else Result.FAILURE

      case None =>
        // We could not get a resource client so data can't be saved
        // user should see a login window when app gets opened next time
        GDrive.updatePreferences(ctxt, isEnabled = true, lastSave = -1L)
        Result.FAILURE
    }
  }
}