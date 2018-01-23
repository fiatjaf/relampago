package com.lightning.wallet

import R.string._
import android.widget._
import com.lightning.wallet.Utils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.Tools.{wrap, none, runAnd}
import org.bitcoinj.core.{BlockChain, PeerGroup}
import scala.util.{Failure, Success, Try}
import R.id.{typePIN, typePass}

import android.widget.RadioGroup.OnCheckedChangeListener
import android.text.method.PasswordTransformationMethod
import org.ndeftools.util.activity.NfcReaderActivity
import info.hoang8f.android.segmented.SegmentedGroup
import concurrent.ExecutionContext.Implicits.global
import org.bitcoinj.wallet.WalletProtobufSerializer
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.helper.AES
import fr.acinq.bitcoin.Crypto.sha256
import com.lightning.wallet.ln.Tools
import fr.acinq.bitcoin.BinaryData
import scala.concurrent.Future
import java.io.FileInputStream
import android.text.InputType
import android.content.Intent
import org.ndeftools.Message
import android.os.Bundle
import android.view.View


trait ViewSwitch {
  val views: List[View]
  def setVis(ms: Int*) = views zip ms foreach {
    case (view, state) => view setVisibility state
  }
}

object MainActivity {
  var proceed: Runnable = _
  lazy val prepareKit = Future {
    val stream = new FileInputStream(app.walletFile)
    val proto = WalletProtobufSerializer parseToProto stream

    app.kit = new app.WalletKit {
      def startUp = runAnd(setupAndStartDownload)(proceed.run)
      wallet = (new WalletProtobufSerializer).readWallet(app.params, null, proto)
      store = new org.bitcoinj.store.SPVBlockStore(app.params, app.chainFile)
      blockChain = new BlockChain(app.params, wallet, store)
      peerGroup = new PeerGroup(app.params, blockChain)
    }
  }
}

class MainActivity extends NfcReaderActivity with TimerActivity with ViewSwitch { me =>
  lazy val mnemonicOptions = getResources getStringArray R.array.restore_mnemonic_options
  lazy val mainPassKeysType = findViewById(R.id.mainPassKeysType).asInstanceOf[SegmentedGroup]
  lazy val mainPassCheck = findViewById(R.id.mainPassCheck).asInstanceOf[Button]
  lazy val mainPassData = findViewById(R.id.mainPassData).asInstanceOf[EditText]

  lazy val views =
    findViewById(R.id.mainChoice) ::
      findViewById(R.id.mainPassForm) ::
      findViewById(R.id.mainProgress) :: Nil

  def INIT(state: Bundle) = {
    wrap(me initNfc state)(me setContentView R.layout.activity_main)
    Utils clickableTextField findViewById(R.id.mainGreetings)

    mainPassKeysType setOnCheckedChangeListener new OnCheckedChangeListener {
      def onCheckedChanged(fancyRadioGroupView: RadioGroup, newInputKeyType: Int) = {
        app.prefs.edit.putBoolean(AbstractKit.PASS_INPUT, newInputKeyType == typePass).commit
        updateInputType
      }
    }

    MainActivity.proceed = anyToRunnable {
      val landingIsLN = app.prefs.getBoolean(AbstractKit.LANDING_LN, true)
      val landing = if (landingIsLN) classOf[LNActivity] else classOf[BtcActivity]
      me exitTo landing
    }

    // When activity is re-accessed after initial
    // NFC check we need to call next manually
    if (intentProcessed) onNoNfcIntentFound
  }

  // NFC AND SHARE

  override def onNoNfcIntentFound = {
    // Filter out failures and nulls, try to set value, proceed if successful and inform if not
    val attempts = Try(getIntent.getDataString) :: Try(getIntent getStringExtra Intent.EXTRA_TEXT) :: Nil
    val valid = attempts collectFirst { case res @ Success(nonNull: String) => res map app.TransData.recordValue }
    if (valid.isEmpty) next else valid foreach { case Failure(err) => app.TransData.onFail(inform)(err) case _ => next }
  }

  def readNdefMessage(msg: Message) = try {
    val asText = readFirstTextNdefMessage(msg)
    app.TransData recordValue asText
    next

  } catch { case err: Throwable =>
    // Could not process a message
    me inform nfc_error
  }

  def onNfcStateEnabled = none
  def onNfcStateDisabled = none
  def onNfcFeatureNotFound = none
  def onNfcStateChange(ok: Boolean) = none
  def readNonNdefMessage = me inform nfc_error
  def readEmptyNdefMessage = me inform nfc_error

  // STARTUP LOGIC

  def next: Unit = (app.walletFile.exists, app.isAlive) match {
    // Find out what exactly should be done once user opens an app
    // depends on both wallet app file existence and runtime objects
    case (false, _) => setVis(View.VISIBLE, View.GONE, View.GONE)
    case (true, true) => MainActivity.proceed.run

    case (true, false) =>
      // Launch of a previously closed app
      setVis(View.GONE, View.VISIBLE, View.GONE)
      <<(MainActivity.prepareKit, throw _)(none)
      updateInputType

      mainPassCheck setOnClickListener onButtonTap {
        // Lazy Future has already been initialized so check a pass after it's done
        <<(MainActivity.prepareKit map setup, wrongPass)(_ => app.kit.startAsync)
        setVis(View.GONE, View.GONE, View.VISIBLE)
      }

    // Just should not ever happen
    // and when it does we just exit
    case _ => System exit 0
  }

  // MISC

  private def updateInputType = {
    val (isPassword, trans) = (app.prefs.getBoolean(AbstractKit.PASS_INPUT, true), new PasswordTransformationMethod)
    val (inputType, id) = if (isPassword) (passNoSuggest, typePass) else (InputType.TYPE_CLASS_NUMBER, typePIN)
    mainPassData setTransformationMethod trans
    mainPassData setInputType inputType
    mainPassKeysType check id
  }

  private def setup(some: Any) = {
    val password = mainPassData.getText.toString
    LNParams setup app.kit.decryptSeed(password).getSeedBytes
  }

  private def wrongPass(err: Throwable) = {
    setVis(View.GONE, View.VISIBLE, View.GONE)
    Tools log err.getMessage
  }

  private def inform(messageCode: Int): Unit = {
    val dlg = mkChoiceDialog(next, finish, dialog_ok, dialog_cancel)
    showForm(alertDialog = dlg.setMessage(messageCode).create)
  }

  def goRestoreWallet(view: View) = {
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = mkForm(me negBld dialog_cancel, me getString restore_hint, lst)

    // Offer user a choice between entering a raw or encrypted mnemonic code
    lst setOnItemClickListener onTap { pos => if (pos == 1) rm(alert)(exitRestoreWallet) else proceed }
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, mnemonicOptions)
    lst setDividerHeight 0
    lst setDivider null

    def proceed = rm(alert) {
      val form = getLayoutInflater.inflate(R.layout.frag_encrypted_mnemonic, null)
      val encryptedMnemonic = form.findViewById(R.id.encryptedMnemonic).asInstanceOf[EditText]
      val oldWalletPassword = form.findViewById(R.id.oldWalletPassword).asInstanceOf[EditText]
      lazy val dialog = mkChoiceDialog(attempt, none, dialog_ok, dialog_cancel)
      lazy val alert1 = mkForm(dialog, me getString wallet_restore, form)
      alert1

      def attempt: Unit = rm(alert1) {
        // Catch wrong password in Future
        <(decryptAndImport, onFail)(none)
      }

      def decryptAndImport = {
        val packed = BinaryData(encryptedMnemonic.getText.toString)
        val hash = sha256(oldWalletPassword.getText.toString.binary)
        val plain = AES.decode(hash.toArray)(packed.toArray)
        require(isMnemonicCorrect(plain), "Wrong password")
        app.TransData.value = plain
        exitRestoreWallet
      }
    }
  }

  def exitRestoreWallet = me exitTo classOf[WalletRestoreActivity]
  def goCreateWallet(view: View) = me exitTo classOf[WalletCreateActivity]
}