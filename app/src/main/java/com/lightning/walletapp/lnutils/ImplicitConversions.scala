package com.lightning.walletapp.lnutils

import com.lightning.walletapp.Utils.app
import language.implicitConversions
import fr.acinq.bitcoin.BinaryData
import org.bitcoinj.core.Utils.HEX
import android.text.Html


object ImplicitConversions {
  implicit def string2Ops(raw: String): StringOps = new StringOps(raw)
  implicit def bitcoinLibScript2bitcoinjScript(pubKeyScript: BinaryData): org.bitcoinj.script.Script =
    new org.bitcoinj.script.Script(pubKeyScript, System.currentTimeMillis / 1000L - 3600 * 24)

  implicit def bitcoinjTx2bitcoinLibTx(bitcoinjTx: org.bitcoinj.core.Transaction): fr.acinq.bitcoin.Transaction =
    fr.acinq.bitcoin.Transaction.read(bitcoinjTx.unsafeBitcoinSerialize)

  implicit def bitcoinLibTx2bitcoinjTx(bitcoinLibTx: fr.acinq.bitcoin.Transaction): org.bitcoinj.core.Transaction =
    new org.bitcoinj.core.Transaction(app.params, fr.acinq.bitcoin.Transaction write bitcoinLibTx)
}

object IconGetter extends Html.ImageGetter {
  private val metrics = app.getResources.getDisplayMetrics
  val scrWidth = metrics.widthPixels.toDouble / metrics.density
  val maxDialog = metrics.densityDpi * 2.1

  import android.provider.Settings.{System => FontSystem}
  val bigFont = FontSystem.getFloat(app.getContentResolver, FontSystem.FONT_SCALE, 1) > 1
  private val noneDrawable = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_none, null)
  private val btcDrawable = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_btc_shape, null)
  private val lnDrawable = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_bolt_shape, null)
  def getDrawable(s: String) = s match { case "ln" => lnDrawable case "btc" => btcDrawable case "none" => noneDrawable }

  private val fontAdjusted = if (bigFont) 22 else 19
  private val screenMetricsAdjusted = (fontAdjusted * metrics.density).toInt
  noneDrawable.setBounds(0, 0, screenMetricsAdjusted, screenMetricsAdjusted)
  btcDrawable.setBounds(0, 0, screenMetricsAdjusted, screenMetricsAdjusted)
  lnDrawable.setBounds(0, 0, screenMetricsAdjusted, screenMetricsAdjusted)
}

class StringOps(source: String) {
  def html = Html.fromHtml(source, IconGetter, null)
  def binary = BinaryData(source getBytes "UTF-8")
  def hex = HEX.encode(source getBytes "UTF-8")
  def noSpaces = source.replace(" ", "")
}