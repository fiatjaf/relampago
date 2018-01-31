package com.lightning.wallet

import android.widget._
import collection.JavaConverters._
import android.widget.DatePicker._
import com.lightning.wallet.R.string._
import org.bitcoinj.wallet.KeyChain.KeyPurpose._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.hootsuite.nachos.terminator.ChipTerminatorHandler._

import com.lightning.wallet.Utils.{app, isMnemonicCorrect}
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import org.bitcoinj.core.{BlockChain, PeerGroup}
import android.view.{View, ViewGroup}

import com.hootsuite.nachos.NachoTextView
import org.bitcoinj.store.SPVBlockStore
import com.lightning.wallet.ln.LNParams
import org.bitcoinj.crypto.MnemonicCode
import java.util.Calendar
import android.os.Bundle


class WhenPicker(host: TimerActivity, start: Long)
extends DatePicker(host) with OnDateChangedListener { me =>
  def pure = runAnd(me)(try getParent.asInstanceOf[ViewGroup] removeView me catch none)
  def human = java.text.DateFormat getDateInstance java.text.DateFormat.MEDIUM format cal.getTime
  def onDateChanged(view: DatePicker, year: Int, mon: Int, dt: Int) = cal.set(year, mon, dt)
  init(cal get Calendar.YEAR, cal get Calendar.MONTH, cal get Calendar.DATE, me)

  lazy val cal = {
    val calendar = Calendar.getInstance
    calendar setTimeInMillis start
    calendar
  }
}

class WalletRestoreActivity extends TimerActivity with ViewSwitch { me =>
  lazy val views = findViewById(R.id.restoreInfo) :: findViewById(R.id.restoreProgress) :: Nil
  lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[NachoTextView]
  lazy val restoreWallet = findViewById(R.id.restoreWallet).asInstanceOf[Button]
  lazy val restoreWhen = findViewById(R.id.restoreWhen).asInstanceOf[Button]
  lazy val password = findViewById(R.id.restorePass).asInstanceOf[EditText]
  lazy val datePicker = new WhenPicker(me, 1501545600L * 1000)

  def INIT(state: Bundle) = {
    setContentView(R.layout.activity_restore)
    val allowed = MnemonicCode.INSTANCE.getWordList
    val lineStyle = android.R.layout.simple_list_item_1
    val adapter = new ArrayAdapter(me, lineStyle, allowed)

    val changeListener = new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, x: Int, y: Int, z: Int) =
        // Both password and mnemonic should be valid in order to proceed
        checkValidity

      private def checkValidity = {
        val passIsOk = password.getText.length >= 6
        val mnemonicIsOk = isMnemonicCorrect(getMnemonicText)

        restoreWallet.setEnabled(mnemonicIsOk & passIsOk)
        if (!mnemonicIsOk) restoreWallet setText restore_mnemonic_wrong
        else if (!passIsOk) restoreWallet setText secret_too_short
        else restoreWallet setText wallet_restore
      }
    }

    if (app.TransData.value != null) {
      // Should be an unencrypted mnemonic string
      val chips = app.TransData.value.toString split "\\s+"
      timer.schedule(UITask(restoreCode setText chips.toList.asJava), 100)
      app.TransData.value = null
    }

    restoreWhen setText datePicker.human
    password addTextChangedListener changeListener
    restoreCode addTextChangedListener changeListener
    restoreCode.addChipTerminator(' ', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator(',', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator('\n', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode setDropDownBackgroundResource R.color.button_material_dark
    restoreCode setAdapter adapter
  }

  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  def getMnemonicText = restoreCode.getText.toString.trim.toLowerCase
    .replaceAll("[^a-zA-Z0-9']+", " ")

  def doRecoverWallet =
    app.kit = new app.WalletKit {
      setVis(View.GONE, View.VISIBLE)
      startAsync

      def startUp = {
        val seed = new DeterministicSeed(getMnemonicText,
          null, "", datePicker.cal.getTimeInMillis / 1000)

        // Recreate a wallet and use checkpoints
        wallet = Wallet.fromSeed(app.params, seed, true)
        store = new SPVBlockStore(app.params, app.chainFile)

        LNParams.setup(seed.getSeedBytes)
        useCheckPoints(seed.getCreationTimeSeconds)
        app.encryptWallet(wallet, password.getText)
        wallet currentAddress RECEIVE_FUNDS
        wallet currentAddress CHANGE

        // Must be initialized after checkpoints
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)

        if (app.isAlive) {
          setupAndStartDownload
          wallet saveToFile app.walletFile
          exitTo apply classOf[WalletActivity]
        }
      }
    }

  def recWallet(v: View) = hideKeys(doRecoverWallet)
  def setWhen(v: View) = mkForm(mkChoiceDialog(restoreWhen setText datePicker.human,
    none, dialog_ok, dialog_cancel), title = null, content = datePicker.pure)
}