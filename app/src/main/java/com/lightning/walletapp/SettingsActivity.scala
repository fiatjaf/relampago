package com.lightning.walletapp

import android.widget._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.google.android.gms.auth.api.signin._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.lnutils.{RatesSaver, TaskWrap}
import android.view.{Menu, MenuItem, View}

import com.lightning.walletapp.ln.wire.WalletZygote
import com.google.android.gms.drive.MetadataBuffer
import android.support.v4.content.FileProvider
import com.lightning.walletapp.lnutils.GDrive
import com.lightning.walletapp.helper.AES
import android.support.v7.widget.Toolbar
import org.bitcoinj.store.SPVBlockStore
import org.bitcoinj.core.Utils.HEX
import com.google.common.io.Files
import android.content.Intent
import android.app.Activity
import android.os.Bundle
import android.net.Uri
import java.util.Date
import java.io.File


class SettingsActivity extends TimerActivity with HumanTimeDisplay { me =>
  lazy val gDriveBackups = findViewById(R.id.gDriveBackups).asInstanceOf[CheckBox]
  lazy val gDriveBackupState = findViewById(R.id.gDriveBackupState).asInstanceOf[TextView]
  lazy val exportWalletSnapshot = findViewById(R.id.exportWalletSnapshot).asInstanceOf[Button]
  lazy val chooseBitcoinUnit = findViewById(R.id.chooseBitcoinUnit).asInstanceOf[Button]
  lazy val recoverFunds = findViewById(R.id.recoverChannelFunds).asInstanceOf[Button]
  lazy val setFiatCurrency = findViewById(R.id.setFiatCurrency).asInstanceOf[Button]
  lazy val manageOlympus = findViewById(R.id.manageOlympus).asInstanceOf[Button]
  lazy val rescanWallet = findViewById(R.id.rescanWallet).asInstanceOf[Button]
  lazy val viewMnemonic = findViewById(R.id.viewMnemonic).asInstanceOf[Button]
  lazy val host = me

  override def onActivityResult(reqCode: Int, resultCode: Int, result: Intent) = reqCode match {
    case 102 if resultCode == Activity.RESULT_OK => checkBackup(GoogleSignIn.getSignedInAccountFromIntent(result).getResult)
    case 102 => runAnd(app.prefs.edit.putBoolean(AbstractKit.GDRIVE_ENABLED, false).commit)(updateBackupView)
    case _ =>
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.status, menu)
    true
  }

  override def onOptionsItemSelected(m: MenuItem) = {
    val walletManual = Uri parse "http://lightning-wallet.com"
    me startActivity new Intent(Intent.ACTION_VIEW, walletManual)
    true
  }

  def checkBackup(signInAcc: GoogleSignInAccount) = {
    val syncTask = GDrive.syncClientTask(app)(signInAcc)
    val driveResClient = GDrive.driveResClient(app)(signInAcc)

    val onMedaData = TaskWrap.onSuccess[MetadataBuffer] { buf =>
      val stamp = if (buf.getCount < 1) 0L else System.currentTimeMillis
      GDrive.updatePreferences(app, isEnabled = true, lastSave = stamp)
      UITask(updateBackupView).run
      ChannelManager.backUp
    }

    val onFailure = TaskWrap.onFailure { exc =>
      // At this point we know it does not work so update UI and settings
      app.prefs.edit.putBoolean(AbstractKit.GDRIVE_ENABLED, false).commit
      UITask(updateBackupView).run
      app toast exc.getMessage
    }

    new TaskWrap[Void, MetadataBuffer] sContinueWithTask syncTask apply { _ =>
      GDrive.getMetaTask(driveResClient.getAppFolder, driveResClient, backupFileName)
        .addOnSuccessListener(onMedaData).addOnFailureListener(onFailure)
    }
  }

  def onGDrive(cb: View) =
    if (gDriveBackups.isChecked) check(GDrive signInAttemptClient me) else {
      app.prefs.edit.putBoolean(AbstractKit.GDRIVE_ENABLED, false).commit
      GDrive.signInAttemptClient(me).revokeAccess
      updateBackupView
    }

  def check(client: GoogleSignInClient) =
    obsOnIO.map(_ => GDrive signInAccount me) foreach {
      case Some(signedInAccount) => checkBackup(signedInAccount)
      case _ => startActivityForResult(client.getSignInIntent, 102)
    }

  def INIT(s: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_settings
    me initToolbar findViewById(R.id.toolbar).asInstanceOf[Toolbar]
    getSupportActionBar setSubtitle "App version 0.3-100"
    getSupportActionBar setTitle wallet_settings
    updateBackupView

    setFiatCurrency setOnClickListener onButtonTap {
      val fiatCodes \ fiatHumanNames = fiatNames.toSeq.reverse.unzip
      val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
      val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

      def updateFiatType(pos: Int) = {
        fiatCode = fiatCodes.toList(pos)
        // Update fiatCode so UI update can react to changes right away
        app.prefs.edit.putString(AbstractKit.FIAT_TYPE, fiatCode).commit
        Option(FragWallet.worker).foreach(_.updTitle.run)
      }

      lst setOnItemClickListener onTap(updateFiatType)
      lst setAdapter new ArrayAdapter(me, singleChoice, fiatHumanNames.toArray)
      showForm(negBuilder(dialog_ok, me getString sets_set_fiat, form).create)
      lst.setItemChecked(fiatCodes.toList indexOf fiatCode, true)
    }

    chooseBitcoinUnit setOnClickListener onButtonTap {
      val currentDenom = app.prefs.getInt(AbstractKit.DENOM_TYPE, 0)
      val allDenoms = getResources.getStringArray(R.array.denoms).map(_.html)
      val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
      val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

      def updateDenomination(pos: Int) = {
        // Update denom so UI update can react to changes
        // then persist user choice in local data storage

        denom = denoms(pos)
        app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
        Option(FragWallet.worker).foreach(_.adapter.notifyDataSetChanged)
        Option(FragWallet.worker).foreach(_.updTitle.run)
      }

      showForm(negBuilder(dialog_ok, me getString sets_choose_unit, form).create)
      lst setAdapter new ArrayAdapter(me, singleChoice, allDenoms)
      lst setOnItemClickListener onTap(updateDenomination)
      lst.setItemChecked(currentDenom, true)
    }

    manageOlympus setOnClickListener onButtonTap {
      // Just show a list of available Olympus servers
      me goTo classOf[OlympusActivity]
    }

    rescanWallet setOnClickListener onButtonTap {
      val bld = baseTextBuilder(me getString sets_rescan_ok).setTitle(me getString sets_rescan)
      mkCheckForm(alert => rm(alert)(go), none, bld, dialog_ok, dialog_cancel)

      def go = try {
        app.chainFile.delete
        app.kit.wallet.reset
        app.kit.store = new SPVBlockStore(app.params, app.chainFile)
        app.kit useCheckPoints app.kit.wallet.getEarliestKeyCreationTime
        app.kit.wallet saveToFile app.walletFile
        finishAffinity
        System exit 0
      } catch none
    }

    viewMnemonic setOnClickListener onButtonTap {
      // Can be accessed here and from page button
      me viewMnemonic null
    }

    exportWalletSnapshot setOnClickListener onButtonTap {
      val bld = me baseTextBuilder getString(migrator_usage_warning).html
      mkCheckForm(alert => rm(alert)(proceed), none, bld, dialog_next, dialog_cancel)
      def proceed = <(createZygote, onFail)(none)

      def createZygote = {
        // Prevent channel state updates
        RatesSaver.subscription.unsubscribe
        val dbFile = new File(app.getDatabasePath(dbFileName).getPath)
        val sourceFilesSeq = Seq(dbFile, app.walletFile, app.chainFile)
        val Seq(dbBytes, walletBytes, chainBytes) = sourceFilesSeq map Files.toByteArray
        val encoded = walletZygoteCodec encode WalletZygote(1, dbBytes, walletBytes, chainBytes)

        val name = s"BLW Snapshot ${new Date}.txt"
        val walletSnapshotFilePath = new File(getCacheDir, "images")
        if (!walletSnapshotFilePath.isFile) walletSnapshotFilePath.mkdirs
        val savedFile = new File(walletSnapshotFilePath, name)
        Files.write(encoded.require.toByteArray, savedFile)

        val fileURI = FileProvider.getUriForFile(me, "com.lightning.walletapp", savedFile)
        val share = new Intent setAction Intent.ACTION_SEND addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
        share.putExtra(Intent.EXTRA_STREAM, fileURI).setDataAndType(fileURI, getContentResolver getType fileURI)
        me startActivity Intent.createChooser(share, "Choose an app")
      }
    }

    recoverFunds setOnClickListener onButtonTap {
      def recover = app.olympus getBackup cloudId foreach { backups =>
        // Decrypt channel recovery data and put it to channels list if it is not present already
        // then try to get new NodeAnnouncement for refunding channels, otherwise use an old one

        for {
          encryptedHexBackup <- backups
          ref <- AES.decBytes(HEX decode encryptedHexBackup, cloudSecret) map bin2readable map to[RefundingData]
          if ChannelManager.all.forall(_.hasCsOr(_.commitments.channelId, null) != ref.commitments.channelId)
        } ChannelManager.all +:= ChannelManager.createChannel(ChannelManager.operationalListeners, ref)

        for {
          chan <- ChannelManager.all if chan.state == REFUNDING
          // Try to connect right away and maybe use new address later
          _ = ConnectionManager.connectTo(chan.data.announce, notify = false)
          // Can call findNodes without `retry` wrapper because it gives `Obs.empty` on error
          Vector(ann1 \ _, _*) <- app.olympus findNodes chan.data.announce.nodeId.toString
        } chan process ann1
      }

      def go = {
        timer.schedule(finish, 3000)
        app toast olympus_recovering
        recover
      }

      val bld = baseTextBuilder(me getString channel_recovery_info)
      mkCheckForm(alert => rm(alert)(go), none, bld, dialog_next, dialog_cancel)
    }

    // Wallet may not see incoming txs immediately if channel gets broken while not synched
    recoverFunds.setEnabled(ChannelManager.currentBlocksLeft < broadcaster.blocksPerDay)
  } else me exitTo classOf[MainActivity]

  def updateBackupView = {
    val isUserEnabled = app.prefs.getBoolean(AbstractKit.GDRIVE_ENABLED, true)
    val lastStamp = app.prefs.getLong(AbstractKit.GDRIVE_LAST_SAVE, 0L)
    val state = time apply new java.util.Date(lastStamp)
    val gDriveMissing = GDrive isMissing app

    if (gDriveMissing) gDriveBackupState setText gdrive_not_present
    else if (!isUserEnabled) gDriveBackupState setText gdrive_disabled
    else if (lastStamp == -1L) gDriveBackupState setText gdrive_failed
    else if (lastStamp == 0L) gDriveBackupState setText gdrive_not_present
    else gDriveBackupState setText getString(gdrive_last_saved).format(state).html
    gDriveBackups.setChecked(!gDriveMissing && isUserEnabled)
    gDriveBackups.setEnabled(!gDriveMissing)
  }
}