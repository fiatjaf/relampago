package com.lightning.walletapp.lnutils

import android.graphics.drawable.BitmapDrawable
import com.lightning.walletapp.Utils.app
import language.implicitConversions
import fr.acinq.bitcoin.BinaryData
import android.view.Gravity
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
  import android.provider.Settings.System.{getFloat, FONT_SCALE}
  val bigFont = getFloat(app.getContentResolver, FONT_SCALE, 1) > 1
  private val fontAdjusted = if (bigFont) 2 else 0

  private val metrics = app.getResources.getDisplayMetrics
  val scrWidth = metrics.widthPixels.toDouble / metrics.densityDpi
  val maxDialog = metrics.densityDpi * 2.1
  val isTablet = scrWidth > 3.5

  val btcDrawableTitle = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_btc_shape, null).asInstanceOf[BitmapDrawable]
  val lnDrawableTitle = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_bolt_shape, null).asInstanceOf[BitmapDrawable]
  val btcDrawable = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_btc_shape, null).asInstanceOf[BitmapDrawable]
  val lnDrawable = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_bolt_shape, null).asInstanceOf[BitmapDrawable]

  def getDrawable(s: String) = s match {
    case "btcbig" => btcDrawableTitle
    case "lnbig" => lnDrawableTitle
    case "btc" => btcDrawable
    case "ln" => lnDrawable
  }

  btcDrawableTitle.setBounds(0, 0, btcDrawable.getIntrinsicWidth, btcDrawable.getIntrinsicHeight + 12 + fontAdjusted * 2)
  lnDrawableTitle.setBounds(0, 0, lnDrawable.getIntrinsicWidth, lnDrawable.getIntrinsicHeight + 18 + fontAdjusted * 2)
  btcDrawable.setBounds(0, 0, btcDrawable.getIntrinsicWidth, btcDrawable.getIntrinsicHeight + 9 + fontAdjusted)
  lnDrawable.setBounds(0, 0, lnDrawable.getIntrinsicWidth, lnDrawable.getIntrinsicHeight + 9 + fontAdjusted)

  btcDrawableTitle.setGravity(Gravity.TOP)
  lnDrawableTitle.setGravity(Gravity.TOP)
  btcDrawable.setGravity(Gravity.TOP)
  lnDrawable.setGravity(Gravity.TOP)
}

class StringOps(source: String) {
  def html = Html.fromHtml(source, IconGetter, null)
  def hex = BinaryData(source getBytes "UTF-8").toString
  def noSpaces = source.replace(" ", "")
}