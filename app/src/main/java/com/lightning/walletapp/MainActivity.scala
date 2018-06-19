package com.lightning.walletapp

import R.string._
import android.widget._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.Tools.{none, runAnd}
import org.bitcoinj.core.{BlockChain, PeerGroup}
import com.google.common.io.{ByteStreams, Files}
import java.io.{File, FileInputStream}

import ln.wire.LightningMessageCodecs.walletZygoteCodec
import org.ndeftools.util.activity.NfcReaderActivity
import org.bitcoinj.wallet.WalletProtobufSerializer
import com.lightning.walletapp.ln.LNParams
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

class MainActivity extends NfcReaderActivity with TimerActivity { me =>
  override def onActivityResult(requestCode: Int, resultCode: Int, resultData: Intent) =
    if (requestCode == 101 & resultCode == Activity.RESULT_OK) restoreFromZygote(resultData)

  def INIT(state: Bundle) = {
    runAnd(me setContentView R.layout.activity_main)(me initNfc state)
    Utils clickableTextField findViewById(R.id.mainGreetings)

    MainActivity.proceed = UITask {
      // Unconditionally go to wallet
      me exitTo classOf[WalletActivity]
    }
  }

  // NFC AND SHARE

  private[this] def readFail(readingError: Throwable) = runAnd(app toast err_no_data)(next)
  def readNdefMessage(m: Message) = <(app.TransData recordValue ndefMessageString(m), readFail)(ok => next)

  override def onNoNfcIntentFound = {
    val datas = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT)
    <(datas.find(txt => null != txt) foreach app.TransData.recordValue, readFail)(ok => next)
  }

  def onNfcStateEnabled = none
  def onNfcStateDisabled = none
  def onNfcFeatureNotFound = none
  def onNfcStateChange(ok: Boolean) = none
  def readNonNdefMessage = me readFail null
  def readEmptyNdefMessage = me readFail null

  // STARTUP LOGIC

  def next: Unit = (app.walletFile.exists, app.isAlive) match {
    // Find out what exactly should be done once user opens an app
    // depends on both wallet app file existence and runtime objects presence
    case (false, _) => findViewById(R.id.mainChoice) setVisibility View.VISIBLE
    case (true, true) => MainActivity.proceed.run

    case (true, false) =>
      MainActivity.prepareKit
      val seed = app.kit.wallet.getKeyChainSeed.getSeedBytes
      runAnd(LNParams setup seed)(app.kit.startAsync)

    // Just should not ever happen
    // and when it does we just exit
    case _ => System exit 0
  }

  // MISC

  def goRestoreWallet(view: View) = {
    val restoreOptions = getResources getStringArray R.array.restore_options
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.titleTip, restoreOptions)
    val alert = showForm(negBuilder(dialog_cancel, null, lst).create)

    lst setDivider null
    lst setDividerHeight 0
    lst setOnItemClickListener onTap {
      case 0 => rm(alert)(exitRestoreWallet)
      case 1 => proceedWithMigrationFile
    }

    def proceedWithMigrationFile = rm(alert) {
      val intent = new Intent(Intent.ACTION_OPEN_DOCUMENT) setType "text/plain"
      startActivityForResult(intent addCategory Intent.CATEGORY_OPENABLE, 101)
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