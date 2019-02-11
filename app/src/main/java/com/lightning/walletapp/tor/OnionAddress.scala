package com.lightning.walletapp.tor

import java.net.InetSocketAddress
import com.lightning.walletapp.ln.Tools.Bytes
import org.apache.commons.codec.binary.Base32


sealed trait OnionAddress {
  def getHostString = s"$onionService.onion"
  def toInetSocketAddress = new InetSocketAddress(getHostString, port)
  val onionService: String
  val port: Int
}

case class OnionAddressV2(onionService: String, port: Int) extends OnionAddress
case class OnionAddressV3(onionService: String, port: Int) extends OnionAddress

object OnionAddress {
  def base32encode(addr: Bytes) = (new Base32).encodeAsString(addr).toLowerCase
  def fromParts(encodedHost: Bytes, port: Int) = base32encode(encodedHost) match {
    case onionAddress if onionAddress.length == V2Len => OnionAddressV2(onionAddress, port)
    case onionAddress if onionAddress.length == V3Len => OnionAddressV3(onionAddress, port)
    case onionAddress => throw new RuntimeException(s"Invalid Tor address $onionAddress")
  }

  val V2Len = 16
  val V3Len = 56
}