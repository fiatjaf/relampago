package com.lightning.walletapp

import org.bitcoinj.core.{BlockChain, PeerGroup}
import com.lightning.walletapp.ln.Tools.wrap
import com.lightning.walletapp.ln.LNParams
import com.lightning.walletapp.Utils.app
import org.bitcoinj.store.SPVBlockStore
import org.bitcoinj.wallet.Wallet
import android.os.Bundle


trait FirstActivity { me: TimerActivity =>
  def prepareFreshWallet(kit: app.WalletKit) = {
    kit.store = new SPVBlockStore(app.params, app.chainFile)
    kit.useCheckPoints(kit.wallet.getEarliestKeyCreationTime)
    LNParams.setup(kit.wallet.getKeyChainSeed.getSeedBytes)

    // Complete initialization
    kit.blockChain = new BlockChain(app.params, kit.wallet, kit.store)
    kit.peerGroup = new PeerGroup(app.params, kit.blockChain)

    // Make sure keys are rendered and save to disk
    kit.wallet.currentAddress(org.bitcoinj.wallet.KeyChain.KeyPurpose.RECEIVE_FUNDS)
    kit.wallet.currentAddress(org.bitcoinj.wallet.KeyChain.KeyPurpose.CHANGE)
    kit.wallet.saveToFile(app.walletFile)

    kit.setupAndStartDownload
    me exitTo classOf[WalletActivity]
  }
}

class WalletCreateActivity extends TimerActivity with FirstActivity {
  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)

  def INIT(state: Bundle) = {
    app.kit = new app.WalletKit {
      override def startUp: Unit = {
        wallet = new Wallet(app.params)
        prepareFreshWallet(this)
      }
    }

    // Fill view and immediately create a wallet
    setContentView(R.layout.activity_create)
    app.kit.startAsync
  }
}