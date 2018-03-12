package com.lightning.wallet

import org.bitcoinj.core.{BlockChain, PeerGroup}
import com.lightning.wallet.ln.Tools.wrap
import org.bitcoinj.store.SPVBlockStore
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.Utils.app
import org.bitcoinj.wallet.Wallet
import android.view.View
import android.os.Bundle


trait FirstActivity { me: TimerActivity =>
  def prepare(kit: app.WalletKit, pass: CharSequence) = {
    kit.store = new SPVBlockStore(app.params, app.chainFile)
    kit.useCheckPoints(kit.wallet.getEarliestKeyCreationTime)

    // Get seed and set everything up before encryption
    LNParams.setup(kit.wallet.getKeyChainSeed.getSeedBytes)
    if (pass.length > 0) app.encryptWallet(kit.wallet, pass)

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

class WalletCreateActivity extends TimerActivity with ViewSwitch with FirstActivity { me =>
  lazy val createPass = findViewById(R.id.createPass).asInstanceOf[android.widget.EditText]
  lazy val createWallet = findViewById(R.id.createWallet).asInstanceOf[android.widget.Button]
  lazy val views = findViewById(R.id.createInfo) :: findViewById(R.id.createProgress) :: Nil
  def INIT(state: Bundle) = setContentView(R.layout.activity_create)

  def makeNewWallet =
    app.kit = new app.WalletKit {
      setVis(View.GONE, View.VISIBLE)
      startAsync

      override def startUp = {
        wallet = new Wallet(app.params, true)
        prepare(this, createPass.getText)
      }
    }

  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  def newWallet(view: View) = hideKeys(makeNewWallet)
}