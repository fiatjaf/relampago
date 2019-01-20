package com.lightning.walletapp

import android.widget._
import android.widget.DatePicker._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.hootsuite.nachos.terminator.ChipTerminatorHandler._
import com.lightning.walletapp.lnutils.{ChannelWrap, GDrive, TaskWrap}
import com.lightning.walletapp.ln.Tools.{none, runAnd, wrap}
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import android.view.{View, ViewGroup}

import com.lightning.walletapp.lnutils.JsonHttpUtils.to
import com.lightning.walletapp.ln.wire.GDriveBackup
import com.google.android.gms.drive.DriveContents
import com.hootsuite.nachos.NachoTextView
import com.lightning.walletapp.Utils.app
import org.bitcoinj.crypto.MnemonicCode
import android.content.Intent
import android.app.Activity
import scala.util.Success
import java.util.Calendar
import android.os.Bundle


class WhenPicker(host: TimerActivity, start: Long) extends DatePicker(host) with OnDateChangedListener { me =>
  def humanTime = java.text.DateFormat getDateInstance java.text.DateFormat.MEDIUM format cal.getTime
  def onDateChanged(view: DatePicker, year: Int, mon: Int, date: Int) = cal.set(year, mon, date)
  def refresh = runAnd(me)(try getParent.asInstanceOf[ViewGroup] removeView me catch none)
  init(cal get Calendar.YEAR, cal get Calendar.MONTH, cal get Calendar.DATE, me)

  lazy val cal = {
    val calendar = Calendar.getInstance
    calendar setTimeInMillis start
    me setMinDate start
    calendar
  }
}

class WalletRestoreActivity extends TimerActivity with FirstActivity { me =>
  lazy val restoreProgress = findViewById(R.id.restoreProgress).asInstanceOf[View]
  lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[NachoTextView]
  lazy val restoreWallet = findViewById(R.id.restoreWallet).asInstanceOf[Button]
  lazy val restoreWhen = findViewById(R.id.restoreWhen).asInstanceOf[Button]
  lazy val restoreInfo = findViewById(R.id.restoreInfo).asInstanceOf[View]
  lazy val dp = new WhenPicker(me, 1526817600 * 1000L)

  def INIT(state: Bundle) = {
    setContentView(R.layout.activity_restore)
    restoreCode addTextChangedListener new TextChangedWatcher {
      def isMnemonicCorrect = getMnemonic.split("\\s+").length > 11
      override def onTextChanged(c: CharSequence, x: Int, y: Int, z: Int) = {
        val txt = if (isMnemonicCorrect) wallet_restore else restore_mnemonic_wrong
        restoreWallet setEnabled isMnemonicCorrect
        restoreWallet setText txt
      }
    }

    restoreWhen setText dp.humanTime
    restoreCode.addChipTerminator(' ', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator(',', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator('\n', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode setDropDownBackgroundResource R.color.button_material_dark
    restoreCode setAdapter new ArrayAdapter(me, android.R.layout.simple_list_item_1,
      MnemonicCode.INSTANCE.getWordList)

    // Sign in before wallet restoring
    val noGDrive = GDrive isMissing me
    if (!noGDrive) askGDriveSignIn
  }

  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  override def onActivityResult(reqCode: Int, resultCode: Int, result: Intent) =
    if (reqCode == 102 && resultCode != Activity.RESULT_OK) warnNoBackups(none).run

  def warnNoBackups(go: => Unit) = UITask {
    val bld = baseTextBuilder(me getString err_gdrive_sign_in_failed)
    mkCheckForm(alert => rm(alert)(go), finish, bld, dialog_ok, dialog_cancel)
  }

  def getMnemonic = restoreCode.getText.toString.trim.toLowerCase.replaceAll("[^a-zA-Z0-9']+", " ")
  def setWhen(button: View) = mkCheckForm(alert => rm(alert)(restoreWhen setText dp.humanTime),
    none, baseBuilder(null, dp.refresh), dialog_ok, dialog_cancel)

  def recWallet(top: View) =
    app.kit = new app.WalletKit {
      restoreProgress setVisibility View.VISIBLE
      restoreInfo setVisibility View.GONE
      startAsync

      def startUp = {
        // Make a seed from user provided mnemonic code and restore a wallet using it
        val seed = new DeterministicSeed(getMnemonic, null, "", dp.cal.getTimeInMillis / 1000)
        wallet = Wallet.fromSeed(app.params, seed)
        LNParams setup seed.getSeedBytes

        // Proceed to wallet or try to use gdrive backups
        // user should have been logged in gdrive at this point
        if (GDrive isMissing app) me prepareFreshWallet app.kit
        else attemptToLoadBackup
      }
    }

  def restoreAnyChannel(some: HasCommitments) = {
    val chan = ChannelManager.createChannel(ChannelManager.operationalListeners, some)
    app.kit.wallet.addWatchedScripts(app.kit fundingPubScript some)
    // Do not use STORE because it invokes a backup upload
    ChannelWrap put some
    chan
  }

  def restoreClosedChannel(closing: ClosingData) = {
    // Closing channels may have in-flight 2nd level HTLCs present
    app.kit.wallet.addWatchedScripts(app.kit closingPubKeyScripts closing)
    restoreAnyChannel(closing)
  }

  def restoreChannel(some: HasCommitments) = some match {
    case closing: ClosingData => restoreClosedChannel(closing)
    case _ => restoreAnyChannel(some)
  }

  def useGDriveBackup(googleDriveBackup: GDriveBackup) = {
    for (snapshot <- googleDriveBackup.clouds) app.olympus tellClouds snapshot
    ChannelManager.all = for (data <- googleDriveBackup.chans) yield restoreChannel(data)
    GDrive.updatePreferences(app, isEnabled = true, lastSave = System.currentTimeMillis)
    me prepareFreshWallet app.kit
  }

  def attemptToLoadBackup =
    GDrive signInAccount app match {
      case Some(googleSignInAccount) =>
        val syncTask = GDrive.syncClientTask(app)(googleSignInAccount)
        val driveResClient = GDrive.driveResClient(app)(googleSignInAccount)

        val onError = TaskWrap.onFailure { exc =>
          // This may be normal if user has no backup at all
          warnNoBackups(me prepareFreshWallet app.kit).run
        }

        val onBackup = TaskWrap.onSuccess[DriveContents] { contents =>
          GDrive.decrypt(contents, LNParams.cloudSecret) map to[GDriveBackup] match {
            case Success(decodedGDriveBackupData) => useGDriveBackup(decodedGDriveBackupData)
            case _ => onError onFailure new Exception("Decryption has failed")
          }
        }

        new TaskWrap[Void, DriveContents] sContinueWithTask syncTask apply { _ =>
          val metaTask = GDrive.getMetaTask(driveResClient.getAppFolder, driveResClient, backupFileName)
          GDrive.getFileTask(metaTask, driveResClient).addOnSuccessListener(onBackup).addOnFailureListener(onError)
        }

      case None =>
        me prepareFreshWallet app.kit
        UITask(app toast gdrive_disabled).run
    }
}