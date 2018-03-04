package com.lightning.wallet

import R.string._
import android.widget._
import com.lightning.wallet.Utils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import co.infinum.goldfinger.{Goldfinger, Warning}
import org.bitcoinj.core.{BlockChain, PeerGroup}
import com.google.common.io.{ByteStreams, Files}
import co.infinum.goldfinger.{Error => GFError}
import scala.util.{Failure, Success, Try}
import java.io.{File, FileInputStream}

import ln.wire.LightningMessageCodecs.walletZygoteCodec
import org.ndeftools.util.activity.NfcReaderActivity
import concurrent.ExecutionContext.Implicits.global
import org.bitcoinj.wallet.WalletProtobufSerializer
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.helper.AES
import fr.acinq.bitcoin.Crypto
import scala.concurrent.Future
import android.content.Intent
import org.ndeftools.Message
import scodec.bits.BitVector
import android.app.Activity
import android.os.Bundle
import android.view.View


trait ViewSwitch {
  val views: List[View]
  def setVis(ms: Int*) = views zip ms foreach {
    case (view, state) => view setVisibility state
  }
}

object FingerPassCode {
  def exists = app.prefs.contains(AbstractKit.ENCRYPTED_PASSCODE)
  def erase = app.prefs.edit.remove(AbstractKit.ENCRYPTED_PASSCODE).commit
  def record(base64: String) = app.prefs.edit.putString(AbstractKit.ENCRYPTED_PASSCODE, base64).commit
  def get = app.prefs.getString(AbstractKit.ENCRYPTED_PASSCODE, "No encrypted passcode exists here")

  def informUser(w: Warning) = w match {
    case Warning.DIRTY => app toast fp_err_dirty
    case Warning.FAILURE => app toast fp_err_try_again
    case Warning.INSUFFICIENT => app toast fp_err_try_again
    case Warning.TOO_SLOW => app toast fp_err_try_again
    case Warning.TOO_FAST => app toast fp_err_try_again
    case Warning.PARTIAL => app toast fp_err_try_again
    case otherwise =>
  }

  def informUser(e: GFError) = e match {
    case GFError.CRYPTO_OBJECT_INIT => runAnd(FingerPassCode.erase)(app toast fp_err_disabled)
    case GFError.DECRYPTION_FAILED => runAnd(FingerPassCode.erase)(app toast fp_err_failure)
    case GFError.ENCRYPTION_FAILED => runAnd(FingerPassCode.erase)(app toast fp_err_failure)
    case GFError.TIMEOUT => app toast fp_err_timeout
    case otherwise => app toast fp_err_failure
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
  lazy val mainFingerprint = findViewById(R.id.mainFingerprint).asInstanceOf[ImageView]
  lazy val mainPassCheck = findViewById(R.id.mainPassCheck).asInstanceOf[Button]
  lazy val mainPassData = findViewById(R.id.mainPassData).asInstanceOf[EditText]
  lazy val gf = new Goldfinger.Builder(me).build
  private[this] val FILE_CODE = 101

  lazy val views =
    findViewById(R.id.mainChoice) ::
      findViewById(R.id.mainPassForm) ::
      findViewById(R.id.mainProgress) :: Nil

  def INIT(state: Bundle) = {
    wrap(me initNfc state)(me setContentView R.layout.activity_main)
    Utils clickableTextField findViewById(R.id.mainGreetings)

    MainActivity.proceed = UITask {
      // Unconditionally go to wallet
      me exitTo classOf[WalletActivity]
    }
  }

  // NFC AND SHARE

  override def onActivityResult(requestCode: Int, resultCode: Int, resultData: Intent) =
    if (requestCode == FILE_CODE & resultCode == Activity.RESULT_OK) restoreFromZygote(resultData)

  override def onNoNfcIntentFound = {
    // Filter out failures and nulls, try to set value, proceed if successful and inform if not
    val attempts = Try(getIntent.getDataString) :: Try(getIntent getStringExtra Intent.EXTRA_TEXT) :: Nil
    val valid = attempts collectFirst { case res @ Success(nonNull: String) => res map app.TransData.recordValue }
    if (valid.isEmpty) next else valid foreach { case Failure(err) => app.TransData.onFail(popup)(err) case _ => next }
  }

  def readNdefMessage(msg: Message) = try {
    val asText = readFirstTextNdefMessage(msg)
    app.TransData recordValue asText
    next

  } catch { case err: Throwable =>
    // Could not process a message
    me popup nfc_error
  }

  def onNfcStateEnabled = none
  def onNfcStateDisabled = none
  def onNfcFeatureNotFound = none
  def onNfcStateChange(ok: Boolean) = none
  def readNonNdefMessage = me popup nfc_error
  def readEmptyNdefMessage = me popup nfc_error

  // STARTUP LOGIC

  def next: Unit = (app.walletFile.exists, app.isAlive) match {
    // Find out what exactly should be done once user opens an app
    // depends on both wallet app file existence and runtime objects
    case (false, _) => setVis(View.VISIBLE, View.GONE, View.GONE)
    case (true, true) => MainActivity.proceed.run

    case (true, false) =>
      // Load Future in a background
      setVis(View.GONE, View.VISIBLE, View.GONE)
      <<(MainActivity.prepareKit, throw _)(none)

      mainPassCheck setOnClickListener onButtonTap(startLogin)
      if (gf.hasEnrolledFingerprint && FingerPassCode.exists) {
        // This device hase fingerprint support, prints registered
        // and user has saved an encrypted passcode in app prefs

        val callback = new Goldfinger.Callback {
          def onWarning(warn: Warning) = FingerPassCode informUser warn
          def onSuccess(plainPasscode: String) = runAnd(mainPassData setText plainPasscode)(startLogin)
          def onError(err: GFError) = wrap(FingerPassCode informUser err)(mainFingerprint setVisibility View.GONE)
        }

        mainFingerprint setVisibility View.VISIBLE
        gf.decrypt(fileName, FingerPassCode.get, callback)
      }

    // Just should not ever happen
    // and when it does we just exit
    case _ => System exit 0
  }

  // MISC

  def startLogin = {
    // Lazy Future has already been initialized so check a pass after it's done
    <<(MainActivity.prepareKit map decrypt, wrongPass)(_ => app.kit.startAsync)
    setVis(View.GONE, View.GONE, View.VISIBLE)
    gf.cancel
  }

  def decrypt(some: Any) = {
    val password = mainPassData.getText.toString
    LNParams setup app.kit.decryptSeed(password).getSeedBytes
  }

  def wrongPass(authError: Throwable) = {
    setVis(View.GONE, View.VISIBLE, View.GONE)
    app toast authError.getMessage
  }

  def popup(messageCode: Int): Unit = {
    val dlg = mkChoiceDialog(next, finish, dialog_ok, dialog_cancel)
    showForm(alertDialog = dlg.setMessage(messageCode).create)
  }

  def goRestoreWallet(view: View) = {
    val mnemonicOptions = getResources getStringArray R.array.restore_mnemonic_options
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, mnemonicOptions)
    val alert = mkForm(me negBld dialog_cancel, null, lst)

    lst setDivider null
    lst setDividerHeight 0
    lst setOnItemClickListener onTap {
      case 0 => proceedWithEncryptedMnemonic
      case 1 => rm(alert)(exitRestoreWallet)
      case 2 => proceedWithMigrationFile
    }

    def proceedWithEncryptedMnemonic = rm(alert) {
      val form = getLayoutInflater.inflate(R.layout.frag_encrypted_mnemonic, null)
      val encryptedMnemonic = form.findViewById(R.id.encryptedMnemonic).asInstanceOf[EditText]
      val oldWalletPassword = form.findViewById(R.id.oldWalletPassword).asInstanceOf[EditText]
      lazy val dialog = mkChoiceDialog(attempt, none, dialog_ok, dialog_cancel)
      lazy val alert1 = mkForm(dialog, getString(wallet_restore), form)
      alert1

      def attempt: Unit = rm(alert1) {
        // Catch wrong password in Future
        <(decryptAndImport, onFail)(none)
      }

      def decryptAndImport = {
        val hash = Crypto sha256 oldWalletPassword.getText.toString.binary
        val plain = AES.decode(encryptedMnemonic.getText.toString, hash)
        require(isMnemonicCorrect(plain), "Wrong password")
        app.TransData.value = plain
        exitRestoreWallet
      }
    }

    def proceedWithMigrationFile = rm(alert) {
      val intent = new Intent(Intent.ACTION_OPEN_DOCUMENT) setType "text/plain"
      startActivityForResult(intent addCategory Intent.CATEGORY_OPENABLE, FILE_CODE)
    }
  }

  def restoreFromZygote(intent: Intent) = {
    val databaseFile = new File(app.getDatabasePath(dbFileName).getPath)
    val inputStream = getContentResolver.openInputStream(intent.getData)
    val bitVector = BitVector(ByteStreams toByteArray inputStream)
    val zygote = walletZygoteCodec.decode(bitVector).require.value
    if (!databaseFile.exists) databaseFile.getParentFile.mkdirs
    Files.write(zygote.wallet, app.walletFile)
    Files.write(zygote.chain, app.chainFile)
    Files.write(zygote.db, databaseFile)
    next
  }

  def exitRestoreWallet = me exitTo classOf[WalletRestoreActivity]
  def exitCreateWallet(view: View) = me exitTo classOf[WalletCreateActivity]
}