package com.lightning.walletapp.lnutils

import android.graphics.drawable.BitmapDrawable
import com.lightning.walletapp.Utils.app
import language.implicitConversions
import scodec.bits.ByteVector
import android.view.Gravity
import android.text.Html


object ImplicitConversions {
  implicit def string2Ops(raw: String): StringOps = new StringOps(raw)

  implicit def bitcoinLibScript2bitcoinjScript(pubKeyScript: ByteVector): org.bitcoinj.script.Script =
    new org.bitcoinj.script.Script(pubKeyScript.toArray, System.currentTimeMillis / 1000L - 3600 * 24)

  implicit def bitcoinjTx2bitcoinLibTx(bitcoinjTx: org.bitcoinj.core.Transaction): fr.acinq.bitcoin.Transaction =
    fr.acinq.bitcoin.Transaction.read(bitcoinjTx.unsafeBitcoinSerialize)

  implicit def bitcoinLibTx2bitcoinjTx(bitcoinLibTx: fr.acinq.bitcoin.Transaction): org.bitcoinj.core.Transaction =
    new org.bitcoinj.core.Transaction(app.params, bitcoinLibTx.bin.toArray)
}

object IconGetter extends Html.ImageGetter {
  private[this] val metrics = app.getResources.getDisplayMetrics
  val scrWidth = metrics.widthPixels.toDouble / metrics.densityDpi
  val scrHeight = metrics.heightPixels.toDouble / metrics.densityDpi
  val maxDialog = metrics.densityDpi * 2.1
  val isTablet = scrWidth > 3.5

  private[this] val btcDrawableTitle = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_btc_shape, null).asInstanceOf[BitmapDrawable]
  private[this] val lnDrawableTitle = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_bolt_shape, null).asInstanceOf[BitmapDrawable]
  private[this] val btcDrawable = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_btc_shape, null).asInstanceOf[BitmapDrawable]
  private[this] val lnDrawable = app.getResources.getDrawable(com.lightning.walletapp.R.drawable.icon_bolt_shape, null).asInstanceOf[BitmapDrawable]

  def getDrawable(s: String) = s match {
    case "btcbig" => btcDrawableTitle
    case "lnbig" => lnDrawableTitle
    case "btc" => btcDrawable
    case "ln" => lnDrawable
  }

  import android.provider.Settings.System.{getFloat, FONT_SCALE}
  val bigFont = getFloat(app.getContentResolver, FONT_SCALE, 1) > 1

  private[this] val btcTitleCorrection = if (bigFont) metrics.scaledDensity * 4.8 else metrics.scaledDensity * 4.2
  private[this] val lnTitleCorrection = if (bigFont) metrics.scaledDensity * 6.6 else metrics.scaledDensity * 5.6
  private[this] val btcCorrection = if (bigFont) metrics.scaledDensity * 3.6 else metrics.scaledDensity * 2.6
  private[this] val lnCorrection = if (bigFont) metrics.scaledDensity * 3.0 else metrics.scaledDensity * 2.3

  btcDrawableTitle.setBounds(0, 0, btcDrawable.getIntrinsicWidth, btcTitleCorrection.toInt + btcDrawableTitle.getIntrinsicHeight)
  lnDrawableTitle.setBounds(0, 0, lnDrawable.getIntrinsicWidth, lnTitleCorrection.toInt + lnDrawableTitle.getIntrinsicHeight)
  btcDrawable.setBounds(0, 0, btcDrawable.getIntrinsicWidth, btcCorrection.toInt + btcDrawable.getIntrinsicHeight)
  lnDrawable.setBounds(0, 0, lnDrawable.getIntrinsicWidth, lnCorrection.toInt + lnDrawable.getIntrinsicHeight)

  btcDrawableTitle.setGravity(Gravity.TOP)
  lnDrawableTitle.setGravity(Gravity.TOP)
  btcDrawable.setGravity(Gravity.TOP)
  lnDrawable.setGravity(Gravity.TOP)
}

class StringOps(source: String) {
  def html = Html.fromHtml(source, IconGetter, null)
  def s2hex = ByteVector.view(source getBytes "UTF-8").toHex
  def noSpaces = source.replace(" ", "").replace("\u00A0", "")
}