package com.lightning.wallet.lnutils

import com.lightning.wallet.Utils.app
import language.implicitConversions
import fr.acinq.bitcoin.BinaryData
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.TxWrap
import android.text.Html


object ImplicitConversions {
  implicit def string2Ops(raw: String): StringOps = new StringOps(raw)
  implicit def bitcoinjTx2Wrap(bitcoinjTx: org.bitcoinj.core.Transaction): TxWrap = new TxWrap(bitcoinjTx)

  implicit def bitcoinLibScript2bitcoinjScript(pubKeyScript: BinaryData): org.bitcoinj.script.Script =
    new org.bitcoinj.script.Script(pubKeyScript, 1501538400L) // 2017-09-01

  implicit def bitcoinjTx2bitcoinLibTx(bitcoinjTx: org.bitcoinj.core.Transaction): fr.acinq.bitcoin.Transaction =
    fr.acinq.bitcoin.Transaction.read(bitcoinjTx.unsafeBitcoinSerialize)

  implicit def bitcoinLibTx2bitcoinjTx(bitcoinLibTx: fr.acinq.bitcoin.Transaction): org.bitcoinj.core.Transaction =
    new org.bitcoinj.core.Transaction(app.params, fr.acinq.bitcoin.Transaction write bitcoinLibTx)
}

class StringOps(source: String) {
  def binary = BinaryData(source getBytes "UTF-8")
  def hex = HEX.encode(source getBytes "UTF-8")
  def noCommas = source.replace(",", "")
  def html = Html fromHtml source
}