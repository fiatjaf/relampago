package com.lightning.wallet

import R.string._
import com.lightning.wallet.lnutils.olympus._
import org.bitcoinj.core.{BlockChain, PeerGroup}
import com.lightning.wallet.ln.Tools.wrap
import org.bitcoinj.store.SPVBlockStore
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.Utils.app
import org.bitcoinj.wallet.Wallet
import android.view.View
import android.os.Bundle


trait FirstActivity { me: TimerActivity =>
  def launch(wallet: Wallet) = if (app.isAlive) {
    val emptyData = CloudData(None, Vector.empty, Vector.empty)
    val mainServer = OlympusServer("main-dev-server", "http://213.133.99.89:9002", emptyData, 1, 0)
    val fallbackServer = OlympusServer("fallback-dev-server", "http://10.0.2.2:9002", emptyData, 0, 0)

    // Preload developer servers
    OlympusWrap.addServer(mainServer, order = 0)
    OlympusWrap.addServer(fallbackServer, order = 1)

    app.kit.setupAndStartDownload
    wallet saveToFile app.walletFile
    me exitTo classOf[WalletActivity]
  }
}

class WalletCreateActivity extends TimerActivity with ViewSwitch with FirstActivity { me =>
  lazy val createPass = findViewById(R.id.createPass).asInstanceOf[android.widget.EditText]
  lazy val createWallet = findViewById(R.id.createWallet).asInstanceOf[android.widget.Button]
  lazy val views = findViewById(R.id.createInfo) :: findViewById(R.id.createProgress) :: Nil

  def INIT(state: Bundle) = {
    setContentView(R.layout.activity_create)
    createPass addTextChangedListener new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, st: Int, n: Int, af: Int) =
        if (s.length >= 6) wrap(createWallet setEnabled true)(createWallet setText wallet_create)
        else wrap(createWallet setEnabled false)(createWallet setText secret_too_short)
    }
  }

  def makeNewWallet =
    app.kit = new app.WalletKit {
      setVis(View.GONE, View.VISIBLE)
      startAsync

      override def startUp = {
        // Get seed before encryption
        wallet = new Wallet(app.params, true)
        store = new SPVBlockStore(app.params, app.chainFile)
        LNParams.setup(wallet.getKeyChainSeed.getSeedBytes)
        useCheckPoints(wallet.getEarliestKeyCreationTime)
        app.encryptWallet(wallet, createPass.getText)

        // These should be initialized after checkpoints
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)
        launch(wallet)
      }
    }

  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  def newWallet(view: View) = hideKeys(makeNewWallet)
}