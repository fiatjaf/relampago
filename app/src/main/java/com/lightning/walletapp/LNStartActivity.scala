package com.lightning.walletapp

import spray.json._
import android.view._
import android.widget._
import android.support.v4.app._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.PayRequest._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import fr.acinq.bitcoin.{Bech32, Crypto, MilliSatoshi}

import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRouteVec
import com.lightning.walletapp.Utils.app.TransData.nodeLink
import com.lightning.walletapp.lnutils.JsonHttpUtils.to
import com.lightning.walletapp.helper.ThrottledWork
import com.github.kevinsawicki.http.HttpRequest
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.uri.BitcoinURI
import scodec.bits.ByteVector
import android.os.Bundle
import scala.util.Try


class LNStartActivity extends ScanActivity { me =>
  lazy val slidingFragmentAdapter = new FragmentStatePagerAdapter(getSupportFragmentManager) {
    def getItem(currentFragmentPos: Int) = if (0 == currentFragmentPos) new FragLNStart else new FragScan
    def getCount = 2
  }

  override def onBackPressed = {
    val isScannerOpen = 1 == walletPager.getCurrentItem
    if (isScannerOpen) walletPager.setCurrentItem(0, true)
    else super.onBackPressed
  }

  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionScan) walletPager.setCurrentItem(1, true)
  }

  override def onResume = wrap(super.onResume)(me returnToBase null)
  override def onCreateOptionsMenu(menu: Menu) = runAnd(true) {
    // Called after FragLNStart sets its toolbar as actionbar
    getMenuInflater.inflate(R.menu.lnstart, menu)
    FragLNStart.fragment.setupSearch(menu)
  }

  def INIT(s: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_double_pager
    walletPager setAdapter slidingFragmentAdapter
  } else me exitTo classOf[MainActivity]

  def checkTransData =
    app.TransData checkAndMaybeErase {
      case _: LNUrl => me exitTo MainActivity.wallet
      case _: BitcoinURI => me exitTo MainActivity.wallet
      case _: PaymentRequest => me exitTo MainActivity.wallet
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case _ => me returnToBase null
    }
}

object FragLNStart {
  var fragment: FragLNStart = _
}

class FragLNStart extends Fragment with SearchBar with HumanTimeDisplay { me =>
  override def onCreateView(inf: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inf.inflate(R.layout.frag_ln_start, vg, false)

  lazy val host = me.getActivity.asInstanceOf[LNStartActivity]
  private[this] var nodes = Vector.empty[StartNodeView]
  FragLNStart.fragment = me

  val worker = new ThrottledWork[String, AnnounceChansNumVec] {
    def work(nodeSearchAsk: String) = app.olympus findNodes nodeSearchAsk
    def error(nodeSearchError: Throwable) = host onFail nodeSearchError

    def process(userQuery: String, results: AnnounceChansNumVec) = {
      nodes = for (result <- results) yield RemoteNodeView(result)
      host.UITask(adapter.notifyDataSetChanged).run
    }
  }

  val adapter = new BaseAdapter {
    def getView(pos: Int, savedView: View, par: ViewGroup) = {
      val slot = host.getLayoutInflater.inflate(R.layout.frag_single_line, null)
      val textLine = slot.findViewById(R.id.textLine).asInstanceOf[TextView]
      val txt = getItem(pos).asString(app getString ln_ops_start_node_view)
      textLine setText txt.html
      slot
    }

    def getItem(position: Int) = nodes(position)
    def getItemId(position: Int) = position
    def getCount = nodes.size
  }

  def react = worker addWork lastQuery
  def onNodeSelected(pos: Int): Unit = {
    app.TransData.value = adapter getItem pos
    host goTo classOf[LNStartFundActivity]
  }

  override def onViewCreated(view: View, state: Bundle) = if (app.isAlive) {
    val lnStartNodesList = view.findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
    me initToolbar view.findViewById(R.id.toolbar).asInstanceOf[android.support.v7.widget.Toolbar]
    wrap(host.getSupportActionBar setTitle action_ln_open)(host.getSupportActionBar setSubtitle ln_status_peer)
    lnStartNodesList setOnItemClickListener host.onTap(onNodeSelected)
    lnStartNodesList setAdapter adapter
    host.checkTransData
    react
  }
}

// DISPLAYING NODES ON UI

sealed trait StartNodeView {
  def asString(base: String): String
}

case class IncomingChannelParams(nodeView: HardcodedNodeView, open: OpenChannel)
case class HardcodedNodeView(ann: NodeAnnouncement, tip: String) extends StartNodeView {
  // App suggests a bunch of hardcoded and separately fetched nodes with a good liquidity
  def asString(base: String) = base.format(ann.alias, tip, ann.pretty)
}

case class RemoteNodeView(acn: AnnounceChansNum) extends StartNodeView {
  def asString(base: String) = base.format(ca.alias, app.plur1OrZero(chansNumber, num), ca.pretty)
  lazy val chansNumber = app.getResources getStringArray R.array.ln_ops_start_node_channels
  val ca \ num = acn
}

// LNURL response types

object LNUrl {
  def fromBech32(bech32url: String) = {
    val _ \ data = Bech32.decode(bech32url)
    val request = Bech32.five2eight(data)
    LNUrl(Tools bin2readable request)
  }
}

case class LNUrl(request: String) {
  val uri = android.net.Uri.parse(request)
  require(uri.toString contains "https://", "First level uri is not an HTTPS endpoint")
  lazy val isLogin: Boolean = Try(uri getQueryParameter "tag" equals "login").getOrElse(false)
  lazy val isPay: Boolean = Try(uri getQueryParameter "tag" equals "pay").getOrElse(false)
  lazy val k1: Try[String] = Try(uri getQueryParameter "k1")
}

object LNUrlData {
  type PayReqVec = Vector[PaymentRequest]
  def guardResponse(raw: String): String = {
    val validJson = Try(raw.parseJson.asJsObject.fields)
    val hasError = validJson.map(_ apply "reason").map(json2String)
    if (validJson.isFailure) throw new Exception(s"Invalid response $raw")
    if (hasError.isSuccess) throw new Exception(hasError.get)
    raw
  }
}

trait LNUrlData {
  def unsafe(request: String): HttpRequest =
    get(request, true).trustAllCerts.trustAllHosts
}

case class WithdrawRequest(callback: String, k1: String,
                           maxWithdrawable: Long, defaultDescription: String,
                           minWithdrawable: Option[Long] = None) extends LNUrlData {

  require(callback contains "https://", "Callback does not have HTTPS prefix")
  val minCanReceive = MilliSatoshi(minWithdrawable getOrElse 1L)
  require(minCanReceive.amount <= maxWithdrawable)
  require(minCanReceive.amount >= 1L)

  def requestWithdraw(lnUrl: LNUrl, pr: PaymentRequest) = {
    val privateKey = LNParams.getLinkingKey(lnUrl.uri.getHost)

    val request =
      android.net.Uri.parse(callback).buildUpon
        .appendQueryParameter("pr", PaymentRequest write pr)
        .appendQueryParameter("k1", k1)

    val req1Try = for {
      dataToSign <- Try(ByteVector fromValidHex k1)
      signature = Tools.sign(dataToSign, privateKey).toHex
    } yield request.appendQueryParameter("sig", signature)
    unsafe(req1Try.getOrElse(request).build.toString)
  }
}

case class IncomingChannelRequest(uri: String, callback: String, k1: String) extends LNUrlData {
  // Recreate node announcement from supplied data and call a second level callback once connected
  require(callback contains "https://", "Callback does not have HTTPS prefix")
  val nodeLink(key, host, port) = uri

  def resolveNodeAnnouncement = {
    val nodeId = PublicKey(ByteVector fromValidHex key)
    val nodeAddress = NodeAddress.fromParts(host, port.toInt)
    app.mkNodeAnnouncement(nodeId, nodeAddress, host)
  }

  def requestChannel =
    unsafe(android.net.Uri.parse(callback).buildUpon
      .appendQueryParameter("remoteid", LNParams.nodePublicKey.toString)
      .appendQueryParameter("private", "1")
      .appendQueryParameter("k1", k1)
      .build.toString)
}

object PayRequest {
  type TagAndContent = Vector[String]
  type PayMetaData = Vector[TagAndContent]
  type KeyAndUpdate = (PublicKey, ChannelUpdate)
  type Route = Vector[KeyAndUpdate]
}

case class PayRequest(routes: Vector[Route], maxSendable: Long, minSendable: Long, metadata: String, pr: String) extends LNUrlData {
  val extraPaymentRoutes: PaymentRouteVec = for (route <- routes) yield route map { case nodeId \ chanUpdate => chanUpdate toHop nodeId }
  val decodedMetadata = ByteVector.fromValidBase64(metadata)
  val paymentRequest = PaymentRequest.read(pr)

  val textMetaData = to[PayMetaData](Tools bin2readable decodedMetadata.toArray).collectFirst { case Vector("text/plain", content) => content }.get
  for (route <- routes) for (nodeId \ chanUpdate <- route) require(Announcements.checkSig(chanUpdate, nodeId), "Extra route contains an invalid update")
  require(ByteVector.fromValidHex(paymentRequest.description) == Crypto.sha256(decodedMetadata), "Invoice hash does not match metadata")
}