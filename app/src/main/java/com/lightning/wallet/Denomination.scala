package com.lightning.wallet

import R.string._
import java.text._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.MilliSatoshi
import language.implicitConversions
import org.bitcoinj.core.Coin
import language.postfixOps


object Denomination {
  val sat2msatFactor = 1000L
  val btc2msatFactor = 100000000000L
  val locale = new java.util.Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)
  val formatFiat = new DecimalFormat("#,###,###.##")
  formatFiat setDecimalFormatSymbols symbols

  def btcBigDecimal2MSat(btc: BigDecimal) = MilliSatoshi(btc * btc2msatFactor toLong)
  implicit def mSat2Coin(msat: MilliSatoshi): Coin = Coin.valueOf(msat.amount / sat2msatFactor)
  implicit def coin2MSat(cn: Coin): MilliSatoshi = MilliSatoshi(cn.value * sat2msatFactor)
}

trait Denomination {
  def rawString2MSat(raw: String) = MilliSatoshi(BigDecimal(raw) * factor toLong)
  def formatted(msat: MilliSatoshi) = fmt format BigDecimal(msat.amount) / factor
  def withSign(msat: MilliSatoshi): String
  val fmt: DecimalFormat
  val factor: Long
  val txt: String
}

object SatDenomination extends Denomination {
  val fmt = new DecimalFormat("###,###,###.###")
  val txt = app getString amount_hint_sat
  val factor = sat2msatFactor

  fmt setDecimalFormatSymbols symbols
  def withSign(msat: MilliSatoshi) =
    "ⓢ\u00A0" + formatted(msat)
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
  val factor = btc2msatFactor

  fmt setDecimalFormatSymbols symbols
  def withSign(msat: MilliSatoshi) =
    formatted(msat) + "\u00A0BTC"
}