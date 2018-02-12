package com.lightning.wallet.lnutils

import com.lightning.wallet.ln.Tools.{random, Bytes}
import org.bitcoinj.core.ECKey.CURVE.{getN, getG}
import org.spongycastle.math.ec.ECPoint
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.ECKey
import java.math.BigInteger


case class BlindMemo(params: List[BlindParam], clears: List[BigInteger], sesPubKeyHex: String) {
  def makeBlindTokens = params zip clears map { case (param, token) => param.blind(token).toString }
  def makeClearSigs(blind: BigInteger*) = params zip blind map { case (param, sig) => param unblind sig }

  def pack(clearSigs: BigInteger*) = {
    val clearSigStrings = for (clearSig <- clearSigs) yield clearSig.toString
    val clearTokenStrings = for (clearToken <- clears) yield clearToken.toString
    val blindPoints = for (param <- params) yield HEX encode param.point
    (blindPoints, clearTokenStrings, clearSigStrings).zipped.toList
  }
}

// As seen on http://arxiv.org/pdf/1304.2094.pdf
class ECBlind(signerQ: ECPoint, signerR: ECPoint) {
  def params(number: Int) = List.fill(number)(makeParams)
  def tokens(number: Int) = List.fill(number)(oneToken)
  def oneToken = new BigInteger(1, random getBytes 64)

  def makeParams: BlindParam = {
    val a = new ECKey(random).getPrivKey
    val b = new ECKey(random).getPrivKey
    val c = new ECKey(random).getPrivKey

    val bInv: BigInteger = b modInverse getN
    val abInvQ: ECPoint = signerQ.multiply(a.multiply(bInv) mod getN)
    val blindF: ECPoint = signerR.multiply(bInv).add(abInvQ).add(getG multiply c).normalize
    if (blindF.getAffineXCoord.isZero | blindF.getAffineYCoord.isZero) makeParams
    else BlindParam(blindF getEncoded true, a, b, c, bInv)
  }
}

// We blind a token but unblind it's signature
case class BlindParam(point: Bytes, a: BigInteger, b: BigInteger, c: BigInteger, bInv: BigInteger) {
  def blind(msg: BigInteger): BigInteger = b.multiply(keyBigInt mod getN).multiply(msg).add(a) mod getN
  def keyBigInt: BigInteger = ECKey.CURVE.getCurve.decodePoint(point).getAffineXCoord.toBigInteger
  def unblind(sigHat: BigInteger): BigInteger = bInv.multiply(sigHat).add(c) mod getN
}