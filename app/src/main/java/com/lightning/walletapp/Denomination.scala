package com.lightning.walletapp

import R.string._
import java.text._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.Utils.app
import fr.acinq.bitcoin.MilliSatoshi
import language.implicitConversions
import org.bitcoinj.core.Coin


object Denomination {
  val locale = new java.util.Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)
  symbols.setGroupingSeparator(' ')
  symbols.setDecimalSeparator('.')

  val formatFiat = new DecimalFormat("#,###,###.##")
  formatFiat setDecimalFormatSymbols symbols

  implicit def mSat2Coin(msat: MilliSatoshi): Coin = Coin.valueOf(msat.amount / 1000L)
  implicit def coin2MSat(cn: Coin): MilliSatoshi = MilliSatoshi(cn.value * 1000L)

  def btcBigDecimal2MSat(btc: BigDecimal) = {
    val btcAsMsat = btc * BtcDenomination.factor
    MilliSatoshi(btcAsMsat.toLong)
  }
}

trait Denomination {
  def rawString2MSat(raw: String) = {
    val factored = BigDecimal(raw) * factor
    MilliSatoshi(factored.toLong)
  }

  def asString(msat: MilliSatoshi): String = {
    val factored = BigDecimal(msat.amount) / factor
    fmt format factored
  }

  def formatted(msat: MilliSatoshi): String
  def withSign(msat: MilliSatoshi): String
  val amountInTxt: String
  val fmt: DecimalFormat
  val factor: Long
}

object SatDenomination extends Denomination {
  val fmt = new DecimalFormat("###,###,###.###")
  val amountInTxt = app getString amount_hint_sat
  val factor = 1000L

  fmt setDecimalFormatSymbols symbols
  def withSign(msat: MilliSatoshi) =
    formatted(msat) + "\u00A0sat"

  def formatted(msat: MilliSatoshi) = {
    val basicFormattedSum = asString(msat)
    val whole \ decimal = basicFormattedSum.splitAt(basicFormattedSum indexOf ".")
    if (decimal == basicFormattedSum) basicFormattedSum else s"$whole<small>$decimal</small>"
  }
}

object FinDenomination extends Denomination {
  val fmt = new DecimalFormat("###,##0.0000###")
  val amountInTxt = app getString amount_hint_fin
  val factor = 10000000L

  fmt setDecimalFormatSymbols symbols
  def withSign(msat: MilliSatoshi) =
    formatted(msat) + "\u00A0fin"

  def formatted(msat: MilliSatoshi) = {
    val basicFormattedSum = asString(msat)
    val dotIndex = basicFormattedSum indexOf "."
    val whole \ decimal = basicFormattedSum splitAt dotIndex
    val satDecimalPart \ milliSatDecimalPart = decimal splitAt 5

    if (decimal == basicFormattedSum) basicFormattedSum
    else if (decimal == milliSatDecimalPart) s"$whole<small>$decimal</small>"
    else s"$whole$satDecimalPart<small>$milliSatDecimalPart</small>"
  }
}

object BtcDenomination extends Denomination {
  val fmt = new DecimalFormat("##0.000########")
  val amountInTxt = app getString amount_hint_btc
  val factor = 100000000000L

  fmt setDecimalFormatSymbols symbols
  def withSign(msat: MilliSatoshi) =
    formatted(msat) + "\u00A0btc"

  def formatted(msat: MilliSatoshi) = {
    val basicFormattedSum = asString(msat)
    val dotIndex = basicFormattedSum indexOf "."
    val whole \ decimal = basicFormattedSum splitAt dotIndex
    val satDecimalPart \ milliSatDecimalPart = decimal splitAt 9

    if (decimal == basicFormattedSum) basicFormattedSum
    else if (decimal == milliSatDecimalPart) s"$whole<small>$decimal</small>"
    else s"$whole$satDecimalPart<small>$milliSatDecimalPart</small>"
  }
}