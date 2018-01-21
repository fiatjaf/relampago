package com.lightning.wallet

import R.string._
import scala.util.{Success, Try}
import com.lightning.wallet.ln.Tools.{none, wrap}
import com.lightning.wallet.AbstractKit.ERROR_REPORT
import org.bitcoinj.wallet.WalletProtobufSerializer
import com.lightning.wallet.Utils.app
import java.io.FileInputStream
import android.os.Bundle
import android.view.View


class EmergencyActivity extends ToolbarActivity { me =>
  override def notifySubTitle(subtitleText: String, infoType: Int): Unit = none
  def showMnemonic(view: View) = passWrap(me getString sets_mnemonic) apply checkPassNotify(doViewMnemonic)

  def INIT(state: Bundle) = {
    <(prepareWallet, _ => app toast err_general)(none)
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_emergency)
    add(me getString emerge_subtitle, Informer.EMERGENCY).flash.run
    setTitle(me getString emerge_title)
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

  def showDetails(report: String) = {
    val (view, field) = str2Tuple(report)
    mkForm(me negBld dialog_ok, null, view)
    field setTextIsSelectable true
  }

  def showReport(view: View) =
    Try(getIntent getStringExtra ERROR_REPORT) match {
      case Success(report: String) => showDetails(report)
      case _ => showDetails(me getString err_general)
    }
}