package com.lightning.wallet

import R.string._
import scala.util.{Success, Try}
import com.lightning.wallet.AbstractKit.ERROR_REPORT
import org.bitcoinj.wallet.WalletProtobufSerializer
import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.Utils.app
import java.io.FileInputStream
import android.os.Bundle
import android.view.View


class EmergencyActivity extends TimerActivity { me =>
  def viewReport(v: View) = Try(getIntent getStringExtra ERROR_REPORT) match {
    case Success(report) => showForm(negBld(dialog_ok).setMessage(report).create)
    case _ => app toast err_general
  }

  def INIT(savedInstanceState: Bundle) = {
    me setContentView R.layout.activity_emergency
    <(prepareWallet, onFail)(none)
  }

  def prepareWallet =
    app.kit = new app.WalletKit {
      val stream = new FileInputStream(app.walletFile)
      val proto = WalletProtobufSerializer parseToProto stream
      wallet = (new WalletProtobufSerializer).readWallet(app.params, null, proto)

      def startUp = none
      blockChain = null
      peerGroup = null
      store = null
    }
}