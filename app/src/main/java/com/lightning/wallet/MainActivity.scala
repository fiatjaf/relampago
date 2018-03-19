package com.lightning.wallet

import R.string._
import android.widget._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import co.infinum.goldfinger.{Goldfinger, Warning}
import org.bitcoinj.core.{BlockChain, PeerGroup}
import com.google.common.io.{ByteStreams, Files}
import co.infinum.goldfinger.{Error => GFError}
import scala.util.{Failure, Success, Try}
import java.io.{File, FileInputStream}

import ln.wire.LightningMessageCodecs.walletZygoteCodec
import org.ndeftools.util.activity.NfcReaderActivity
import org.bitcoinj.wallet.WalletProtobufSerializer
import com.lightning.wallet.ln.LNParams
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
  def exists = app.prefs.contains(AbstractKit.ENC_PASSCODE)
  def erase = app.prefs.edit.remove(AbstractKit.ENC_PASSCODE).commit
  def record(base64: String) = app.prefs.edit.putString(AbstractKit.ENC_PASSCODE, base64).commit
  def get = app.prefs.getString(AbstractKit.ENC_PASSCODE, "No encrypted passcode exists here")

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

  lazy val prepareKit = {
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
  lazy val views = findViewById(R.id.mainChoice) :: findViewById(R.id.mainPassForm) :: Nil
  lazy val mainFingerprint = findViewById(R.id.mainFingerprint).asInstanceOf[ImageView]
  lazy val mainPassCheck = findViewById(R.id.mainPassCheck).asInstanceOf[Button]
  lazy val mainPassData = findViewById(R.id.mainPassData).asInstanceOf[EditText]
  lazy val gf = new Goldfinger.Builder(me).build
  private[this] val FILE_CODE = 101

  def INIT(state: Bundle) = {
    runAnd(me setContentView R.layout.activity_main)(me initNfc state)
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

  def popup(code: Int) = runAnd(app toast code)(next)
  def next: Unit = (app.walletFile.exists, app.isAlive) match {
    // Find out what exactly should be done once user opens an app
    // depends on both wallet app file existence and runtime objects
    case (false, _) => setVis(View.VISIBLE, View.GONE)
    case (true, true) => MainActivity.proceed.run

    case (true, false) =>
      MainActivity.prepareKit
      if (app.kit.wallet.isEncrypted) whenEncrypted else {
        // Wallet is not encrypted so just proceed wuth setup
        val seed = app.kit.wallet.getKeyChainSeed.getSeedBytes
        runAnd(LNParams setup seed)(app.kit.startAsync)
      }

    // Just should not ever happen
    // and when it does we just exit
    case _ => System exit 0
  }

  // MISC

  def whenEncrypted = {
    setVis(View.GONE, View.VISIBLE)
    mainPassCheck setOnClickListener onButtonTap(doCheck)
    if (gf.hasEnrolledFingerprint && FingerPassCode.exists) {
      // This device hase fingerprint support with prints registered

      val callback = new Goldfinger.Callback {
        def onWarning(nonFatalWarning: Warning) = FingerPassCode informUser nonFatalWarning
        def onError(err: GFError) = wrap(FingerPassCode informUser err)(mainFingerprint setVisibility View.GONE)
        def onSuccess(plainPasscode: String) = runAnd(mainPassData setText plainPasscode)(doCheck)
      }

      mainFingerprint setVisibility View.VISIBLE
      gf.decrypt(fileName, FingerPassCode.get, callback)
    }

    def doCheck = {
      def carryOutDecryption = {
        val passcode = mainPassData.getText.toString
        LNParams setup app.kit.decryptSeed(passcode).getSeedBytes
        // Decryption may throw thus preventing further execution
        app.kit.startAsync
        gf.cancel
      }

      def wrong(authThrowable: Throwable) = {
        // Passcode has failed, show form again
        app toast authThrowable.getMessage
        setVis(View.GONE, View.VISIBLE)
      }

      // Reveals background image again
      <(carryOutDecryption, wrong)(none)
      setVis(View.GONE, View.GONE)
    }
  }

  def goRestoreWallet(view: View) = {
    val restoreOptions = getResources getStringArray R.array.restore_options
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, restoreOptions)
    val alert = showForm(negBuilder(dialog_cancel, null, lst).create)

    lst setDivider null
    lst setDividerHeight 0
    lst setOnItemClickListener onTap {
      case 0 => rm(alert)(exitRestoreWallet)
      case 1 => proceedWithMigrationFile
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