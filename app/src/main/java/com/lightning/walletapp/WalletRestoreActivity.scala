package com.lightning.walletapp

import android.widget._
import android.widget.DatePicker._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.R.string._
import com.hootsuite.nachos.terminator.ChipTerminatorHandler._
import com.lightning.walletapp.lnutils.{ChannelWrap, LocalBackup}
import com.lightning.walletapp.ln.Tools.{none, runAnd, wrap}
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import android.view.{View, ViewGroup}

import com.lightning.walletapp.ln.wire.LocalBackups
import com.hootsuite.nachos.NachoTextView
import com.lightning.walletapp.Utils.app
import android.content.pm.PackageManager
import org.bitcoinj.crypto.MnemonicCode
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
  lazy val localBackupStatus = findViewById(R.id.localBackupStatus).asInstanceOf[TextView]
  lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[NachoTextView]
  lazy val restoreProgress = findViewById(R.id.restoreProgress).asInstanceOf[View]
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

    val wordList = MnemonicCode.INSTANCE.getWordList
    val wordView = android.R.layout.simple_list_item_1
    restoreCode setAdapter new ArrayAdapter(me, wordView, wordList)

    val backupAllowed = LocalBackup.isAllowed(activity = me)
    if (!backupAllowed) LocalBackup.askPermission(activity = me)
    else onRequestPermissionsResult(100, Array.empty, Array.empty)
  }

  type GrantResults = Array[Int]
  override def onBackPressed: Unit = wrap(super.onBackPressed)(app.kit.stopAsync)
  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], grantResults: GrantResults): Unit = {
    val isFilePresent = LocalBackup.isAllowed(me) && LocalBackup.isExternalStorageWritable && LocalBackup.getBackupFile.exists
    if (isFilePresent) localBackupStatus setText ln_backup_detected
  }

  def getMnemonic: String = {
    val trimmed = restoreCode.getText.toString.trim
    trimmed.toLowerCase.replaceAll("[^a-zA-Z0-9']+", " ")
  }

  def setWhen(btn: View) =
    mkCheckForm(alert => rm(alert)(restoreWhen setText dp.humanTime),
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

        val attemptBackup = LocalBackup.isAllowed(me) && LocalBackup.isExternalStorageWritable
        if (attemptBackup) LocalBackup.readAndDecrypt(LocalBackup.getBackupFile).foreach(restoreChans)
        me prepareFreshWallet app.kit
      }
    }

  // Restoring from local backup
  // app.kit has been assigned at this point
  // LNParams.setup has been called at this point

  def restoreChans(localBackups: LocalBackups) = {
    localBackups.hosted.foreach(restoreHostedChannel)
    localBackups.normal.foreach(restoreNormalChannel)
  }

  def restoreHostedChannel(some: HostedCommits) = {
    val chan = ChannelManager.createHostedChannel(ChannelManager.operationalListeners, some)
    // Do not use STORE because it invokes a backup saving while we already have it
    ChannelManager.all :+= chan
    ChannelWrap put some
  }

  def restoreNormalChannel(some: HasNormalCommits) = some match {
    case closing: ClosingData => restoreClosedNormalChannel(closing)
    case _ => restoreNotClosedNormalChannel(some)
  }

  def restoreClosedNormalChannel(closing: ClosingData) = {
    // Closing channels may have in-flight 2nd level HTLCs present
    app.kit.wallet.addWatchedScripts(app.kit closingPubKeyScripts closing)
    restoreNotClosedNormalChannel(closing)
  }

  def restoreNotClosedNormalChannel(some: HasNormalCommits) = {
    val chan = ChannelManager.createChannel(ChannelManager.operationalListeners, some)
    app.kit.wallet.addWatchedScripts(app.kit fundingPubScript some)
    // Do not use STORE because it invokes a backup saving
    ChannelManager.all :+= chan
    ChannelWrap put some
  }
}