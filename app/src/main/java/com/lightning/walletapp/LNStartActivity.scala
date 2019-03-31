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
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.Utils.app.TransData.nodeLink
import com.lightning.walletapp.helper.ThrottledWork
import com.lightning.walletapp.LNUrlData.PayReqVec
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
  override def onCreateView(inf: LayoutInflater, vg: ViewGroup, bn: Bundle) = inf.inflate(R.layout.frag_ln_start, vg, false)
  val bitrefillKey = PublicKey.fromValidHex("030c3f19d742ca294a55c00376b3b355c3c90d61c6b6b39554dbc7ac19b141c14f")
  val liteGoKey = PublicKey.fromValidHex("02c12b5459cf107ee0440cae41902f1189db50fa003a077f3a6fbe6b5760218695")
  val acinqKey = PublicKey.fromValidHex("03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f")

  val bitrefillNa = app.mkNodeAnnouncement(bitrefillKey, NodeAddress.fromParts("52.50.244.44", 9735), "Bitrefill")
  val liteGoNa = app.mkNodeAnnouncement(liteGoKey, NodeAddress.fromParts("141.101.8.36", 9735), "LiteGo")
  val acinqNa = app.mkNodeAnnouncement(acinqKey, NodeAddress.fromParts("34.239.230.56", 9735), "ACINQ")

  val bitrefill = HardcodedNodeView(bitrefillNa, "<i>bitrefill.com</i>")
  val acinq = HardcodedNodeView(acinqNa, "<i>strike.acinq.co</i>")
  val liteGo = HardcodedNodeView(liteGoNa, "<i>litego.io</i>")
  val hardcodedNodes = Vector(acinq, bitrefill, liteGo)

  lazy val host = me.getActivity.asInstanceOf[LNStartActivity]
  private[this] var nodes = Vector.empty[StartNodeView]
  FragLNStart.fragment = me

  val worker = new ThrottledWork[String, AnnounceChansNumVec] {
    def work(nodeSearchAsk: String) = app.olympus findNodes nodeSearchAsk
    def error(nodeSearchError: Throwable) = host onFail nodeSearchError

    def process(userQuery: String, results: AnnounceChansNumVec) = {
      val remoteNodeViewWraps = for (nodeInfo <- results) yield RemoteNodeView(nodeInfo)
      nodes = if (userQuery.isEmpty) hardcodedNodes ++ remoteNodeViewWraps else remoteNodeViewWraps
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

sealed trait StartNodeView { def asString(base: String): String }
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

object LNUrlData {
  type PayReqVec = Vector[PaymentRequest]
  def guardResponse(raw: String): Unit = {
    val validJson = Try(raw.parseJson.asJsObject.fields)
    val hasError = validJson.map(_ apply "reason").map(json2String)
    if (validJson.isFailure) throw new Exception(s"Invalid response $raw")
    if (hasError.isSuccess) throw new Exception(hasError.get)
  }
}

sealed trait LNUrlData { def unsafe(request: String) = get(request, true).trustAllCerts.trustAllHosts.body }
case class IncomingChannelRequest(uri: String, callback: String, k1: String, capacity: Long, push: Long) extends LNUrlData {
  def resolveAnnounce = app.mkNodeAnnouncement(PublicKey(ByteVector fromValidHex key), NodeAddress.fromParts(host, port.toInt), host)
  def requestChannel = unsafe(s"$callback?k1=$k1&remoteid=${LNParams.nodePublicKey.toString}&private=1")
  require(callback contains "https://", "Not an HTTPS callback")
  val nodeLink(key, host, port) = uri
}

case class WithdrawRequest(callback: String, k1: String, maxWithdrawable: Long, defaultDescription: String) extends LNUrlData {
  def requestWithdraw(paymentRequest: PaymentRequest) = unsafe(s"$callback?k1=$k1&pr=${PaymentRequest write paymentRequest}")
  require(callback contains "https://", "Not an HTTPS callback")
}

case class MultipartPayment(requests: StringVec, paymentId: String) extends LNUrlData {
  val parsedPaymentRequests: PayReqVec = requests.take(5).map(raw => PaymentRequest read raw).filter(_.amount.isEmpty)
  require(parsedPaymentRequests.map(_.paymentHash).distinct.size == parsedPaymentRequests.size, "Same invoices contain the same hash")
  require(parsedPaymentRequests.forall(_.description contains paymentId), s"Some invoice descriptions do not contain $paymentId")
  require(parsedPaymentRequests.forall(_.lnUrlOpt.isEmpty), "Some invoices contain nested LNUrls which are not allowed")
  require(parsedPaymentRequests.size > 1, "Not enough additional invoices were found")
}