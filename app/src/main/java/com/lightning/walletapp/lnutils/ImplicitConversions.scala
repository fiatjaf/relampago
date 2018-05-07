package com.lightning.walletapp.lnutils

import com.lightning.walletapp.Utils.app
import language.implicitConversions
import fr.acinq.bitcoin.BinaryData
import org.bitcoinj.core.Utils.HEX
import android.text.Html


object ImplicitConversions {
  implicit def string2Ops(raw: String): StringOps = new StringOps(raw)
  implicit def bitcoinLibScript2bitcoinjScript(pubKeyScript: BinaryData): org.bitcoinj.script.Script =
    new org.bitcoinj.script.Script(pubKeyScript, 1519862400L) // Mar 1 2018

  implicit def bitcoinjTx2bitcoinLibTx(bitcoinjTx: org.bitcoinj.core.Transaction): fr.acinq.bitcoin.Transaction =
    fr.acinq.bitcoin.Transaction.read(bitcoinjTx.unsafeBitcoinSerialize)

  implicit def bitcoinLibTx2bitcoinjTx(bitcoinLibTx: fr.acinq.bitcoin.Transaction): org.bitcoinj.core.Transaction =
    new org.bitcoinj.core.Transaction(app.params, fr.acinq.bitcoin.Transaction write bitcoinLibTx)
}

class StringOps(source: String) {
  def html = Html.fromHtml(source, Html.FROM_HTML_MODE_LEGACY)
  def binary = BinaryData(source getBytes "UTF-8")
  def hex = HEX.encode(source getBytes "UTF-8")
  def noSpaces = source.replace(" ", "")
}