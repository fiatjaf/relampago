package com.lightning.walletapp

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
import com.lightning.walletapp.Utils.app.TransData.nodeLink
import com.lightning.walletapp.helper.ThrottledWork
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import java.net.InetSocketAddress
import org.bitcoinj.core.Batch
import android.os.Bundle


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

      case batch: Batch =>
        // TransData value will be erased here
        FragLNStart.fragment.setBatchMode(batch)
        me returnToBase null

      case _ =>
        // TransData value will be erased here
        FragLNStart.fragment.setNormalMode.run
        me returnToBase null
    }
}

object FragLNStart {
  var fragment: FragLNStart = _
  var batchOpt = Option.empty[Batch]
}

class FragLNStart extends Fragment with SearchBar with HumanTimeDisplay { me =>
  override def onCreateView(inf: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inf.inflate(R.layout.frag_ln_start, vg, false)

  override def onDestroy = {
    FragLNStart.batchOpt = None
    super.onDestroy
  }

  var setBatchMode: Batch => Unit = none
  var setNormalMode = new Runnable { def run = none }
  private[this] var nodes = Vector.empty[StartNodeView]
  lazy val host = me.getActivity.asInstanceOf[LNStartActivity]
  lazy val worker = new ThrottledWork[String, AnnounceChansNumVec] {
    private[this] val acinqKey = PublicKey("03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f")
    private[this] val acinqAnnounce = app.mkNodeAnnouncement(acinqKey, new InetSocketAddress("34.239.230.56", 9735), "ACINQ")
    private[this] val acinq = HardcodedNodeView(acinqAnnounce, "<i>Recommended node</i>")

    def error(err: Throwable) = host onFail err
    def work(userQuery: String) = app.olympus findNodes userQuery
    def process(userQuery: String, results: AnnounceChansNumVec) = {
      val remoteNodeViewWraps = for (result <- results) yield RemoteNodeView(result)
      nodes = if (userQuery.isEmpty) acinq +: remoteNodeViewWraps else remoteNodeViewWraps
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
    val batchPresentInfo = view.findViewById(R.id.batchPresentInfo).asInstanceOf[TextView]
    val batchPresentCancel = view.findViewById(R.id.batchPresentCancel).asInstanceOf[Button]
    val batchPresentWrap = view.findViewById(R.id.batchPresentWrap).asInstanceOf[LinearLayout]
    val lnStartNodesList = view.findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]

    setNormalMode = host UITask {
      // Hide a batch funding notification
      batchPresentWrap setVisibility View.GONE
      FragLNStart.batchOpt = None
    }

    setBatchMode = batch => {
      FragLNStart.batchOpt = Some(batch)
      val info = app getString ln_open_batch_inform
      batchPresentWrap setVisibility View.VISIBLE
      batchPresentInfo setText info.html
    }

    // Init
    FragLNStart.fragment = me
    me initToolbar view.findViewById(R.id.toolbar).asInstanceOf[android.support.v7.widget.Toolbar]
    wrap(host.getSupportActionBar setTitle action_ln_open)(host.getSupportActionBar setSubtitle ln_status_peer)
    batchPresentCancel setOnClickListener host.onButtonTap(setNormalMode.run)
    lnStartNodesList setOnItemClickListener host.onTap(onNodeSelected)
    lnStartNodesList.setAdapter(adapter)
    runAnd(host.checkTransData)(react)
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
  def asString(base: String) = base.format(ca.alias, app.plurOrZero(chansNumber, num), ca.pretty)
  lazy val chansNumber = app.getResources getStringArray R.array.ln_ops_start_node_channels
  val ca \ num = acn
}

// LNURL response types

sealed trait LNUrlData { def unsafe(request: String) = get(request, true).trustAllCerts.trustAllHosts.body }
case class IncomingChannelRequest(uri: String, callback: String, k1: String, capacity: Long, push: Long, cltvExpiryDelta: Int,
                                  htlcMinimumMsat: Long, feeBaseMsat: Long, feeProportionalMillionths: Long) extends LNUrlData {

  val nodeLink(key, host, port) = uri
  def getAnnounce = app.mkNodeAnnouncement(PublicKey(key), new InetSocketAddress(host, port.toInt), host)
  def requestChannel = unsafe(s"$callback?k1=$k1&remoteid=${LNParams.nodePublicKey.toString}&private=1")
  require(callback contains "https://", "Not an HTTPS callback")
}

case class WithdrawRequest(callback: String, k1: String, maxAmount: MilliSatoshi, defaultDescription: String) extends LNUrlData {
  def requestWithdraw(paymentRequest: PaymentRequest) = unsafe(s"$callback?k1=$k1&pr=${PaymentRequest write paymentRequest}")
  require(callback contains "https://", "Not an HTTPS callback")
}