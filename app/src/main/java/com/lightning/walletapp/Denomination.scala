package com.lightning.walletapp

import R.string._
import java.text._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.Utils.app
import fr.acinq.bitcoin.MilliSatoshi
import language.implicitConversions
import org.bitcoinj.core.Coin
import language.postfixOps


object Denomination {
  val locale = new java.util.Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)
  symbols.setGroupingSeparator(' ')
  symbols.setDecimalSeparator('.')

  val formatFiat = new DecimalFormat("#,###,###.##")
  formatFiat setDecimalFormatSymbols symbols

  def btcBigDecimal2MSat(btc: BigDecimal) = MilliSatoshi(btc * BtcDenomination.factor toLong)
  implicit def mSat2Coin(msat: MilliSatoshi): Coin = Coin.valueOf(msat.amount / 1000L)
  implicit def coin2MSat(cn: Coin): MilliSatoshi = MilliSatoshi(cn.value * 1000L)
}

trait Denomination {
  def rawString2MSat(raw: String) = MilliSatoshi(BigDecimal(raw) * factor toLong)
  def asString(msat: MilliSatoshi) = fmt format BigDecimal(msat.amount) / factor
  def formattedWithSign(msat: MilliSatoshi) = formatted(msat) + sign
  def formatted(msat: MilliSatoshi): String

  def coloredP2WSH(msat: MilliSatoshi, suffix: String) = s"<font color=#0099FE><tt>+</tt>${this formatted msat}</font>$suffix"
  def coloredOut(msat: MilliSatoshi, suffix: String) = s"<font color=#E31300><tt>-</tt>${this formatted msat}</font>$suffix"
  def coloredIn(msat: MilliSatoshi, suffix: String) = s"<font color=#6AAB38><tt>+</tt>${this formatted msat}</font>$suffix"

  val amountInTxt: String
  val fmt: DecimalFormat
  val factor: Long
  val sign: String
}

object SatDenomination extends Denomination {
  val fmt = new DecimalFormat("###,###,###.###")
  val amountInTxt = app getString amount_hint_sat
  val sign = "\u00A0sat"
  val factor = 1000L

  fmt setDecimalFormatSymbols symbols
  def formatted(msat: MilliSatoshi) = {
    val basicFormattedSum = asString(msat)
    val dotIndex = basicFormattedSum indexOf "."
    val whole \ decimal = basicFormattedSum splitAt dotIndex
    if (decimal == basicFormattedSum) basicFormattedSum
    else s"$whole<small>$decimal</small>"
  }
}

object BtcDenomination extends Denomination {
  val fmt = new DecimalFormat("##0.00000000###")
  val amountInTxt = app getString amount_hint_btc
  val factor = 100000000000L
  val sign = "\u00A0btc"

  fmt setDecimalFormatSymbols symbols
  def formatted(msat: MilliSatoshi) =
    asString(msat) take 10
}