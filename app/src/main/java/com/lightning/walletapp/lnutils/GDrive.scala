package com.lightning.walletapp.lnutils

import spray.json._
import com.google.android.gms.drive._
import com.google.android.gms.tasks._
import com.google.android.gms.drive.query._
import com.google.android.gms.auth.api.signin._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.{AbstractKit, ChannelManager}
import com.lightning.walletapp.ln.Tools.{Bytes, bin2readable}
import androidx.work.{Data, OneTimeWorkRequest, Worker, WorkerParameters}
import com.google.android.gms.common.GoogleApiAvailability
import com.lightning.walletapp.ln.crypto.MultiStreamUtils
import com.google.android.gms.common.ConnectionResult
import com.lightning.walletapp.ln.wire.GDriveBackup
import com.lightning.walletapp.ln.HasNormalCommits
import androidx.work.ListenableWorker.Result
import com.lightning.walletapp.helper.AES
import com.lightning.walletapp.Utils.app
import com.google.common.collect.Sets
import java.util.concurrent.TimeUnit
import android.content.Context
import scodec.bits.ByteVector
import scala.util.Try


object GDrive {
  private[this] val scopes = Sets.newHashSet(Drive.SCOPE_APPFOLDER, Drive.SCOPE_FILE)
  def driveResClient(ctxt: Context)(signInAccount: GoogleSignInAccount) = Drive.getDriveResourceClient(ctxt, signInAccount)
  def syncClientTask(ctxt: Context)(signInAccount: GoogleSignInAccount) = Drive.getDriveClient(ctxt, signInAccount).requestSync
  def isMissing(ctxt: Context) = GoogleApiAvailability.getInstance.isGooglePlayServicesAvailable(ctxt) != ConnectionResult.SUCCESS

  def updateLastSaved(ctxt: Context, lastSave: Long) =
    ctxt.getSharedPreferences("prefs", Context.MODE_PRIVATE)
      .edit.putLong(AbstractKit.GDRIVE_LAST_SAVE, lastSave).commit

  def signInAttemptClient(ctxt: Context) =
    GoogleSignIn.getClient(ctxt, (new GoogleSignInOptions.Builder)
      .requestScopes(Drive.SCOPE_APPFOLDER).requestScopes(Drive.SCOPE_FILE)
      .requestId.requestEmail.build)

  def signInAccount(ctxt: Context) =
    Option(GoogleSignIn getLastSignedInAccount ctxt)
      .filter(_.getGrantedScopes containsAll scopes)

  def decrypt(contents: DriveContents, secret: ByteVector) = for {
    encryptedData <- Try(MultiStreamUtils readone contents.getInputStream)
    someBackup <- AES.decBytes(encryptedData, secret.toArray) map bin2readable
  } yield someBackup

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

  def createBackupTask(res: DriveResourceClient, backup: Bytes, backupFileName: String) = {
    val (appFolderObtainingTask, contentsCreationTask) = res.getAppFolder -> res.createContents
    new TaskWrap[Void, DriveFile] sContinueWithTask Tasks.whenAll(appFolderObtainingTask, contentsCreationTask) apply { _ =>
      val changeSet = (new MetadataChangeSet.Builder).setTitle(backupFileName).setMimeType("application/octet-stream")
      MultiStreamUtils.writeone(inputData = backup, out = contentsCreationTask.getResult.getOutputStream)
      res.createFile(appFolderObtainingTask.getResult, changeSet.build, contentsCreationTask.getResult)
    }
  }

  def updateBackupTask(res: DriveResourceClient, backup: Bytes, driveFile: DriveFile) =
    new TaskWrap[DriveContents, Void] sContinueWithTask res.openFile(driveFile, DriveFile.MODE_WRITE_ONLY) apply { contentsTask =>
      MultiStreamUtils.writeone(inputData = backup, out = contentsTask.getResult.getOutputStream)
      res.commitContents(contentsTask.getResult, null)
    }

  def createOrUpdateBackup(backup: Bytes, backupFileName: String, drc: DriveResourceClient) = Try {
    val listOfAllExistingBackupFiles = Tasks await getMetaTask(drc.getAppFolder, drc, backupFileName)
    if (0 == listOfAllExistingBackupFiles.getCount) Tasks await createBackupTask(drc, backup, backupFileName)
    else Tasks await updateBackupTask(drc, backup, listOfAllExistingBackupFiles.get(0).getDriveId.asDriveFile)
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
  val BACKUP_FILE_NAME = "backupFileName"
  private[this] val bwClass = classOf[BackupWorker]

  def workRequest(backupFileName: String, secret: ByteVector) = {
    val bld = (new Data.Builder).putString(BACKUP_FILE_NAME, backupFileName).putString(SECRET, secret.toHex).build
    new OneTimeWorkRequest.Builder(bwClass).setInputData(bld).setInitialDelay(5, TimeUnit.SECONDS).addTag("ChannelsBackupWork").build
  }
}

class BackupWorker(ctxt: Context, params: WorkerParameters) extends Worker(ctxt, params) {
  // Attempt to save channel state data and storage tokens on a gdrive server, update settings

  def doWork: Result = {
    if (!app.isAlive) return Result.FAILURE
    if (GDrive isMissing ctxt) return Result.SUCCESS
    val prefs = ctxt.getSharedPreferences("prefs", Context.MODE_PRIVATE)
    val isEnabled = prefs.getBoolean(AbstractKit.GDRIVE_ENABLED, true)
    if (!isEnabled) return Result.SUCCESS

    val secretHex = getInputData.getString(BackupWorker.SECRET)
    val backupFileName = getInputData.getString(BackupWorker.BACKUP_FILE_NAME)
    val backups = ChannelManager.all.map(_.data).collect { case nc: HasNormalCommits => nc }
    if (null == secretHex || null == backupFileName || backups.isEmpty) return Result.SUCCESS

    // Convert hex to byte array
    GDrive.signInAccount(ctxt) map GDrive.driveResClient(ctxt) map { drc =>
      val plainBackup = GDriveBackup(chans = backups, v = 1).toJson.toString
      val cipher = AES.encReadable(plainBackup, ByteVector.fromValidHex(secretHex).toArray)
      val res = GDrive.createOrUpdateBackup(backup = cipher.toByteArray, backupFileName, drc)
      GDrive.updateLastSaved(ctxt, if (res.isSuccess) System.currentTimeMillis else -1L)
      if (res.isSuccess) Result.SUCCESS else Result.FAILURE
    } getOrElse {
      // We could not get a resource client so data can't be saved
      // user should see a login window when app gets opened next time
      GDrive.updateLastSaved(ctxt, -1L)
      Result.FAILURE
    }
  }
}