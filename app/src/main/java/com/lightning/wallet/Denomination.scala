package com.lightning.wallet

import R.string._
import java.text._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.Utils.app
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

  def formatted(msat: MilliSatoshi) =
    fmt format BigDecimal(msat.amount) / factor

  def withSign(msat: MilliSatoshi): String
  val fmt: DecimalFormat
  val factor: Long
  val txt: String
}

object SatDenomination extends Denomination {
  val fmt = new DecimalFormat("###,###,###.###")
  val txt = app getString amount_hint_sat
  val factor = 1000L

  fmt setDecimalFormatSymbols symbols
  def withSign(msat: MilliSatoshi) =
    formatted(msat) + "\u00A0SAT"
}

object FinDenomination extends Denomination {
  val fmt = new DecimalFormat("###,###.#######")
  val txt = app getString amount_hint_fin
  val factor = 10000000L

  fmt setDecimalFormatSymbols symbols
  def withSign(msat: MilliSatoshi) =
    formatted(msat) + "\u00A0FIN"
}

object BtcDenomination extends Denomination {
  val fmt = new DecimalFormat("##0.000########")
  val txt = app getString amount_hint_btc
  val factor = 100000000000L

  fmt setDecimalFormatSymbols symbols
  def withSign(msat: MilliSatoshi) =
    formatted(msat) + "\u00A0BTC"
}