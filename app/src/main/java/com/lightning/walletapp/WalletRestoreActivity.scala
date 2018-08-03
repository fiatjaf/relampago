package com.lightning.walletapp

import android.widget._
import android.widget.DatePicker._
import com.lightning.walletapp.R.string._
import com.hootsuite.nachos.terminator.ChipTerminatorHandler._
import com.lightning.walletapp.ln.Tools.{none, runAnd, wrap}
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import android.view.{View, ViewGroup}

import android.R.layout.simple_list_item_1
import com.hootsuite.nachos.NachoTextView
import com.lightning.walletapp.Utils.app
import org.bitcoinj.crypto.MnemonicCode
import java.util.Calendar
import android.os.Bundle


class WhenPicker(host: TimerActivity, start: Long)
  extends DatePicker(host) with OnDateChangedListener { me =>
  def refresh = runAnd(me)(try getParent.asInstanceOf[ViewGroup] removeView me catch none)
  def humanTime = java.text.DateFormat getDateInstance java.text.DateFormat.MEDIUM format cal.getTime
  def onDateChanged(view: DatePicker, year: Int, mon: Int, dt: Int) = cal.set(year, mon, dt)
  init(cal get Calendar.YEAR, cal get Calendar.MONTH, cal get Calendar.DATE, me)

  lazy val cal = {
    val calendar = Calendar.getInstance
    calendar setTimeInMillis start
    me setMinDate start
    calendar
  }
}

class WalletRestoreActivity extends TimerActivity with ViewSwitch with FirstActivity { me =>
  lazy val views = findViewById(R.id.restoreInfo) :: findViewById(R.id.restoreProgress) :: Nil
  lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[NachoTextView]
  lazy val restoreWallet = findViewById(R.id.restoreWallet).asInstanceOf[Button]
  lazy val restoreWhen = findViewById(R.id.restoreWhen).asInstanceOf[Button]
  lazy val dp = new WhenPicker(me, 1526817600 * 1000L)

  def INIT(state: Bundle) = {
    setContentView(R.layout.activity_restore)
    restoreCode addTextChangedListener new TextChangedWatcher {
      override def onTextChanged(cs: CharSequence, x: Int, y: Int, z: Int) = {
        val mnemonicPhraseIsCorrect: Boolean = getMnemo.split("\\s+").length > 11
        if (mnemonicPhraseIsCorrect) restoreWallet setText wallet_restore
        else restoreWallet setText restore_mnemonic_wrong
        restoreWallet setEnabled mnemonicPhraseIsCorrect
      }
    }

    restoreWhen setText dp.humanTime
    restoreCode.addChipTerminator(' ', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator(',', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator('\n', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode setDropDownBackgroundResource R.color.button_material_dark
    restoreCode setAdapter new ArrayAdapter(me, simple_list_item_1,
      MnemonicCode.INSTANCE.getWordList)
  }

  def recWallet(button: View) = hideKeys(doRecoverWallet)
  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  def getMnemo = restoreCode.getText.toString.trim.toLowerCase.replaceAll("[^a-zA-Z0-9']+", " ")
  def setWhen(button: View) = mkCheckForm(alert => rm(alert)(restoreWhen setText dp.humanTime),
    none, baseBuilder(null, dp.refresh), dialog_ok, dialog_cancel)

  def doRecoverWallet =
    app.kit = new app.WalletKit {
      setVis(View.GONE, View.VISIBLE)
      startAsync

      def startUp = {
        // Make a seed from user provided mnemonic code and restore a wallet using it
        val seed = new DeterministicSeed(getMnemo, null, "", dp.cal.getTimeInMillis / 1000)
        wallet = Wallet.fromSeed(app.params, seed)
        prepareFreshWallet(this)
      }
    }
}