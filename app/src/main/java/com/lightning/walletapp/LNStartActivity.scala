package com.lightning.walletapp

import android.view._
import android.widget._
import android.support.v4.app._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.StartNodeView._
import com.lightning.walletapp.ln.wire.FundMsg._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.Utils.app.TransData.nodeLink
import com.lightning.walletapp.helper.ThrottledWork
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.core.Address
import org.bitcoinj.core.Batch
import fr.acinq.bitcoin.Bech32
import android.os.Bundle
import java.util.Date


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
      case _: Address => me exitTo MainActivity.wallet
      case _: BitcoinURI => me exitTo MainActivity.wallet
      case _: PaymentRequest => me exitTo MainActivity.wallet
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]

      case started: Started =>
        // TransData value will be erased here
        FragLNStart.fragment.setExternalFunder(started)
        me returnToBase null

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
  def rmCurrentRemoteFunder = for (currentWSWrap <- ExternalFunder.worker) ExternalFunder eliminateWSWrap currentWSWrap
  override def onCreateView(inf: LayoutInflater, vg: ViewGroup, bn: Bundle) = inf.inflate(R.layout.frag_ln_start, vg, false)

  override def onDestroy = {
    FragLNStart.batchOpt = None
    rmCurrentRemoteFunder
    super.onDestroy
  }

  var setBatchMode: Batch => Unit = none
  var setExternalFunder: Started => Unit = none
  var setNormalMode = new Runnable { def run = none }
  private[this] var nodes = Vector.empty[StartNodeView]
  lazy val host = me.getActivity.asInstanceOf[LNStartActivity]

  val acinqKey = PublicKey("03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f")
  val acinqAnnouncement = app.mkNodeAnnouncement(nodeId = acinqKey, host = "34.239.230.56", 9735)
  val acinq = HardcodedNodeView(acinqAnnouncement, "<strong>Recommended ACINQ node</strong>")

  val worker = new ThrottledWork[String, AnnounceChansNumVec] {
    def error(searchError: Throwable) = Tools errlog searchError
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
      textLine setText getItem(pos).asString(nodeView, "\u0020").html
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

  override def onViewCreated(view: View, state: Bundle) = {
    val externalFundInfo = view.findViewById(R.id.externalFundInfo).asInstanceOf[TextView]
    val externalFundCancel = view.findViewById(R.id.externalFundCancel).asInstanceOf[Button]
    val externalFundWrap = view.findViewById(R.id.externalFundWrap).asInstanceOf[LinearLayout]

    val batchPresentInfo = view.findViewById(R.id.batchPresentInfo).asInstanceOf[TextView]
    val batchPresentCancel = view.findViewById(R.id.batchPresentCancel).asInstanceOf[Button]
    val batchPresentWrap = view.findViewById(R.id.batchPresentWrap).asInstanceOf[LinearLayout]
    val lnStartNodesList = view.findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]

    def funderInfo(wrk: WSWrap, color: Int, text: Int) = host UITask {
      val humanAmountSum = denom withSign wrk.params.start.fundingAmount
      val humanExpiry = me time new Date(wrk.params.expiry)
      val humanFeeSum = denom withSign wrk.params.fee

      externalFundWrap setVisibility View.VISIBLE
      externalFundWrap setBackgroundColor getResources.getColor(color, null)
      externalFundInfo setText host.getString(text).format(wrk.params.start.host,
        humanExpiry, humanAmountSum, humanFeeSum).html
    }

    setExternalFunder = started => {
      val freshWSWrap = WSWrap(started)

      val err2String =
        Map(FAIL_VERIFY_ERROR -> err_fund_verify_error)
          .updated(FAIL_NOT_VERIFIED_YET, err_fund_not_verified_yet)
          .updated(FAIL_INTERNAL_ERROR, err_fund_internal_error)
          .updated(FAIL_RESERVE_FAILED, err_fund_reserve_failed)
          .updated(FAIL_RESERVE_EXPIRED, err_fund_reserve_expired)
          .updated(FAIL_FUNDING_IS_TRIED, err_fund_funding_pending)
          .updated(FAIL_FUNDING_EXISTS, err_fund_funding_exists)
          .updated(FAIL_PUBLISH_ERROR, err_publish_error)

      freshWSWrap.listeners += new ExternalFunderListener {
        override def onRejection = host.UITask(externalFundWrap setVisibility View.GONE).run
        override def onOffline = funderInfo(freshWSWrap, R.color.material_blue_grey_800, ex_fund_connecting).run
        override def onMsg(message: FundMsg) = funderInfo(freshWSWrap, R.color.ln, ex_fund_connected).run
      }

      freshWSWrap.listeners += new ExternalFunderListener {
        override def onMsg(message: FundMsg) = message match {
          case _: Fail => ExternalFunder eliminateWSWrap freshWSWrap
          case _ => Tools log s"Websocket got $message"
        }

        override def onRejection = freshWSWrap.lastMessage match {
          case Fail(code, _, _) if err2String contains code => host onFail getString(err2String apply code)
          case _: Fail => host onFail getString(err2String apply FAIL_INTERNAL_ERROR)
          case _ => host.UITask(app toast err_fund_disconnect).run
        }
      }

      // Try to connect a Funder and remove any Batch info if it was present before
      funderInfo(freshWSWrap, R.color.material_blue_grey_800, ex_fund_connecting).run
      ExternalFunder setWSWrap freshWSWrap
      setNormalMode.run
    }

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

    // Wire up all tappable elements
    batchPresentCancel setOnClickListener host.onButtonTap(setNormalMode.run)
    externalFundCancel setOnClickListener host.onButtonTap(rmCurrentRemoteFunder)
    lnStartNodesList setOnItemClickListener host.onTap(onNodeSelected)

    // Init
    FragLNStart.fragment = me
    me initToolbar view.findViewById(R.id.toolbar).asInstanceOf[android.support.v7.widget.Toolbar]
    wrap(host.getSupportActionBar setTitle action_ln_open)(host.getSupportActionBar setSubtitle ln_status_peer)
    lnStartNodesList.setAdapter(adapter)
    runAnd(host.checkTransData)(react)
  }
}

// DISPLAYING NODES ON UI

object StartNodeView {
  lazy val nodeView = app getString ln_ops_start_node_view
  lazy val nodeFundView = app getString ln_ops_start_fund_node_view
  lazy val incomingChannel = app getString ln_ops_start_fund_incoming_channel
  lazy val chansNumber = app.getResources getStringArray R.array.ln_ops_start_node_channels
}

sealed trait StartNodeView {
  def asString(base: String, separator: String): String
}

case class HardcodedNodeView(ann: NodeAnnouncement, tip: String) extends StartNodeView {
  // App suggests a bunch of hardcoded and separately fetched nodes with a good liquidity

  def asString(base: String, separator: String) = {
    val key = humanNode(ann.nodeId.toString, separator)
    base.format(ann.alias, tip, key)
  }
}

case class RemoteNodeView(acn: AnnounceChansNum) extends StartNodeView {
  // User may search for every currently available node on Olympus server

  def asString(base: String, separator: String) = {
    val channelAnnouncement \ channelConnections = acn
    val humanConnects = app.plurOrZero(chansNumber, channelConnections)
    val key = humanNode(channelAnnouncement.nodeId.toString, separator)
    base.format(channelAnnouncement.alias, humanConnects, key)
  }
}

// GETTING INCOMING CHANNEL

sealed trait LNUrlData
case class IncomingChannelParams(nodeView: HardcodedNodeView, open: OpenChannel)
case class IncomingChannelRequest(uri: String, callback: String, k1: String, capacity: Long, push: Long,
                                  cltvExpiryDelta: Int, htlcMinimumMsat: Long, feeBaseMsat: Long,
                                  feeProportionalMillionths: Long) extends LNUrlData {

  val nodeLink(key, host, port) = uri
  val request = s"$callback?k1=$k1&remoteid=${LNParams.nodePublicKey.toString}&private=1"
  def getAnnounce = app.mkNodeAnnouncement(PublicKey(key), host, port.toInt)
  def requestChannel = get(request, true).trustAllCerts.trustAllHosts.body
}

// LNURL HANDLER

case class LNUrl(bech32url: String) {
  private[this] val _ \ decoded = Bech32 decode bech32url
  private[this] val finalDecoded = Bech32 five2eight decoded
  private def fetch = get(bin2readable(finalDecoded.toArray), true).trustAllCerts.trustAllHosts
  def resolve = obsOnIO.map(_ => fetch.connectTimeout(7500).body) map to[IncomingChannelRequest]
}