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
    new org.bitcoinj.core.Transaction(app.params, fr.acinq.bitcoin.Transaction.write(bitcoinLibTx).toArray)
}

object IconGetter extends Html.ImageGetter {
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

  val bigFont = android.provider.Settings.System.getFloat(app.getContentResolver, android.provider.Settings.System.FONT_SCALE, 1) > 1
  btcDrawableTitle.setBounds(0, 0, btcDrawable.getIntrinsicWidth, { if (bigFont) 16 else 11 } + btcDrawableTitle.getIntrinsicHeight)
  lnDrawableTitle.setBounds(0, 0, lnDrawable.getIntrinsicWidth, { if (bigFont) 22 else 16 } + lnDrawableTitle.getIntrinsicHeight)
  btcDrawable.setBounds(0, 0, btcDrawable.getIntrinsicWidth, { if (bigFont) 10 else 6 } + btcDrawable.getIntrinsicHeight)
  lnDrawable.setBounds(0, 0, lnDrawable.getIntrinsicWidth, { if (bigFont) 11 else 5 } + lnDrawable.getIntrinsicHeight)

  btcDrawableTitle.setGravity(Gravity.TOP)
  lnDrawableTitle.setGravity(Gravity.TOP)
  btcDrawable.setGravity(Gravity.TOP)
  lnDrawable.setGravity(Gravity.TOP)
}

class StringOps(source: String) {
  def html = Html.fromHtml(source, IconGetter, null)
  def hex = ByteVector.view(source getBytes "UTF-8").toHex
  def noSpaces = source.replace(" ", "").replace("\u00A0", "")
}