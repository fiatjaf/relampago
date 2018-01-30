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


class EmergencyActivity
extends TimerActivity { me =>

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

  def viewReport(view: View) =
    Try(getIntent getStringExtra ERROR_REPORT) match {
      case Success(report: String) => showDetails(report)
      case _ => showDetails(me getString err_general)
    }

  def showDetails(report: String) = {
    val (view, field) = str2Tuple(report)
    mkForm(me negBld dialog_ok, null, view)
    field setTextIsSelectable true
  }
}