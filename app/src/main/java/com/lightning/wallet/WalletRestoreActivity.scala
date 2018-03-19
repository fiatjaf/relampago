package com.lightning.wallet

import android.widget._
import android.widget.DatePicker._
import com.lightning.wallet.R.string._
import com.hootsuite.nachos.terminator.ChipTerminatorHandler._
import com.lightning.wallet.Utils.{app, isMnemonicCorrect}
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import android.view.{View, ViewGroup}

import com.hootsuite.nachos.NachoTextView
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

class WalletRestoreActivity extends TimerActivity with ViewSwitch with FirstActivity { me =>
  lazy val views = findViewById(R.id.restoreInfo) :: findViewById(R.id.restoreProgress) :: Nil
  lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[NachoTextView]
  lazy val restoreWallet = findViewById(R.id.restoreWallet).asInstanceOf[Button]
  lazy val restoreWhen = findViewById(R.id.restoreWhen).asInstanceOf[Button]
  lazy val password = findViewById(R.id.restorePass).asInstanceOf[EditText]
  lazy val dp = new WhenPicker(me, 1519862400L * 1000)

  def INIT(state: Bundle) = {
    setContentView(R.layout.activity_restore)
    val style = android.R.layout.simple_list_item_1

    val changeListener = new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = {
        // Both password and mnemonic should be valid in order to proceed here

        val mnemonicIsOk = isMnemonicCorrect(getMnemonicText)
        if (mnemonicIsOk) restoreWallet setText wallet_restore
        else restoreWallet setText restore_mnemonic_wrong
        restoreWallet setEnabled mnemonicIsOk
      }
    }

    restoreWhen setText dp.human
    password addTextChangedListener changeListener
    restoreCode addTextChangedListener changeListener
    restoreCode.addChipTerminator(' ', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator(',', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator('\n', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode setDropDownBackgroundResource R.color.button_material_dark
    restoreCode setAdapter new ArrayAdapter(me, style, MnemonicCode.INSTANCE.getWordList)
  }

  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  def getMnemonicText = restoreCode.getText.toString.trim.toLowerCase.replaceAll("[^a-zA-Z0-9']+", " ")
  def setWhen(v: View) = mkForm(restoreWhen setText dp.human, none, baseBuilder(null, dp.pure), dialog_ok, dialog_cancel)
  def recWallet(v: View) = hideKeys(doRecoverWallet)

  def doRecoverWallet =
    app.kit = new app.WalletKit {
      setVis(View.GONE, View.VISIBLE)
      startAsync

      def startUp = {
        val stamp = dp.cal.getTimeInMillis / 1000
        val seed = new DeterministicSeed(getMnemonicText, null, "", stamp)
        wallet = Wallet.fromSeed(app.params, seed, true)
        prepare(this, password.getText)
      }
    }
}