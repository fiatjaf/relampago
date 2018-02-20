package com.lightning.wallet

import R.string._
import org.bitcoinj.core.{BlockChain, PeerGroup}
import com.lightning.wallet.ln.Tools.wrap
import org.bitcoinj.store.SPVBlockStore
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.Utils.app
import org.bitcoinj.wallet.Wallet
import android.view.View
import android.os.Bundle


class WalletCreateActivity extends TimerActivity with ViewSwitch { me =>
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

        // Enable checkpoints optimization and encrypt wallet
        LNParams.setup(wallet.getKeyChainSeed.getSeedBytes)
        useCheckPoints(wallet.getEarliestKeyCreationTime)
        app.encryptWallet(wallet, createPass.getText)

        // These should be initialized after checkpoints
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)

        if (app.isAlive) {
          setupAndStartDownload
          wallet saveToFile app.walletFile
          me exitTo classOf[WalletActivity]
        }
      }
    }

  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  def newWallet(view: View) = hideKeys(makeNewWallet)
}