package com.lightning.walletapp.lnutils

import com.google.android.gms.drive._
import com.google.android.gms.drive.query._
import com.google.android.gms.tasks.{Continuation, Task, Tasks}
import com.google.android.gms.auth.api.signin.{GoogleSignIn, GoogleSignInAccount, GoogleSignInOptions}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.aesZygoteCodec
import com.google.android.gms.common.GoogleApiAvailability
import com.lightning.walletapp.ln.crypto.MultiStreamUtils
import com.lightning.walletapp.ln.LNParams
import com.lightning.walletapp.helper.AES
import com.google.common.collect.Sets
import fr.acinq.bitcoin.BinaryData
import com.google.common.io.Files
import android.content.Context
import scala.util.Try
import java.io.File


object GDrive {
  final val REQUEST_CODE_SIGN_IN = 102
  def backupFileName = s"blw${LNParams.chainHash.toString}-${LNParams.cloudId.toString}.bkup"
  def isAvailable(ctxt: Context) = GoogleApiAvailability.getInstance.isGooglePlayServicesAvailable(ctxt)
  def driveClient(ctxt: Context)(signInAccount: GoogleSignInAccount) = Drive.getDriveClient(ctxt, signInAccount)
  def driveResClient(ctxt: Context)(signInAccount: GoogleSignInAccount) = Drive.getDriveResourceClient(ctxt, signInAccount)

  def signInOptions =
    (new GoogleSignInOptions.Builder)
      .requestScopes(Drive.SCOPE_APPFOLDER)
      .requestScopes(Drive.SCOPE_FILE)
      .requestId.requestEmail.build

  def signInAccount(ctxt: Context): Option[GoogleSignInAccount] = {
    val scopes = Sets.newHashSet(Drive.SCOPE_APPFOLDER, Drive.SCOPE_FILE)
    Option(GoogleSignIn getLastSignedInAccount ctxt).filter(_.getGrantedScopes containsAll scopes)
  }

  def encryptCoreDb(ctxt: Context, secret: BinaryData, fileName: String) = {
    val dbCoreFile = new File(ctxt.getDatabasePath(fileName).getPath)
    val zygote = AES.encBytes(Files toByteArray dbCoreFile, secret)
    aesZygoteCodec.encode(zygote).require.toByteArray
  }

  def decryptCoreDb(contents: DriveContents, secret: BinaryData) = for {
    encryptedData <- Try(MultiStreamUtils readone contents.getInputStream)
    decryptedData <- AES.decBytes(encryptedData, secret)
  } yield decryptedData

  def getBackupTask(appFolderTask: Task[DriveFolder], res: DriveResourceClient) =
    new TaskWrap[DriveFolder, MetadataBuffer] sContinueWithTask appFolderTask apply { folderTask =>
      val sortOrder = (new SortOrder.Builder).addSortDescending(SortableField.MODIFIED_DATE).build
      val query = new Query.Builder addFilter Filters.eq(SearchableField.TITLE, backupFileName)
      res.queryChildren(folderTask.getResult, query.setSortOrder(sortOrder).build)
    }

  def createBackupTask(ctxt: Context, res: DriveResourceClient, dbFileName: String,
                       backupFileName: String, secret: BinaryData) = {

    val appFolderTask = res.getAppFolder
    val contentsTask = res.createContents

    new TaskWrap[Void, DriveFile] sContinueWithTask Tasks.whenAll(appFolderTask, contentsTask) apply { readyTask =>
      val changeSet = (new MetadataChangeSet.Builder).setTitle(backupFileName).setMimeType("application/octet-stream")
      MultiStreamUtils.writeone(encryptCoreDb(ctxt, secret, dbFileName), contentsTask.getResult.getOutputStream)
      res.createFile(appFolderTask.getResult, changeSet.build, contentsTask.getResult)
    }
  }

  def updateBackupTask(ctxt: Context, res: DriveResourceClient, dbFileName: String, driveFile: DriveFile, secret: BinaryData) =
    new TaskWrap[DriveContents, Void] sContinueWithTask res.openFile(driveFile, DriveFile.MODE_WRITE_ONLY) apply { contentsTask =>
      MultiStreamUtils.writeone(encryptCoreDb(ctxt, secret, dbFileName), contentsTask.getResult.getOutputStream)
      res.commitContents(contentsTask.getResult, null)
    }
}

class TaskWrap[S, R] {
  type TaskSource = Task[S]
  type TaskResult = Task[R]
  def cont(fun: TaskSource => TaskResult) = new Continuation[S, TaskResult] { def `then`(ts: TaskSource) = fun apply ts }
  val sContinueWithTask = (task: TaskSource) => (continuation: TaskSource => TaskResult) => task continueWithTask cont(continuation)
}
