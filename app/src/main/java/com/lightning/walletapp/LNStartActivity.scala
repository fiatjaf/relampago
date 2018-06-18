package com.lightning.walletapp

import android.view._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.StartNodeView._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._

import android.support.v4.app.{Fragment, FragmentStatePagerAdapter}
import android.widget.{BaseAdapter, ListView, TextView}
import com.lightning.walletapp.ln.wire.NodeAnnouncement
import com.lightning.walletapp.helper.ThrottledWork
import com.lightning.walletapp.ln.Tools.runAnd
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.core.Address
import android.os.Bundle


class LNStartActivity extends ScanActivity { me =>
  lazy val slidingFragmentAdapter = new FragmentStatePagerAdapter(getSupportFragmentManager) {
    def getItem(currentFragmentPos: Int) = if (0 == currentFragmentPos) new FragLNStart else new FragScan
    def getCount = 2
  }

  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionScan) walletPager.setCurrentItem(1, true)
  }

  override def onCreateOptionsMenu(menu: Menu) = runAnd(true) {
    // Called after FragLNStart sets its toolbar as actionbar
    getMenuInflater.inflate(R.menu.ln_start, menu)
    FragLNStart.fragment.setupSearch(menu)
  }

  def INIT(s: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_double_pager
    walletPager setAdapter slidingFragmentAdapter
  } else me exitTo classOf[MainActivity]

  def checkTransData = {
    returnToBase(view = null)
    app.TransData.value match {
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case _: PaymentRequest => me exitTo classOf[WalletActivity]
      case _: BitcoinURI => me exitTo classOf[WalletActivity]
      case _: Address => me exitTo classOf[WalletActivity]
      case _ => app.TransData.value = null
    }
  }
}

object FragLNStart {
  var fragment: FragLNStart = _
}

class FragLNStart extends Fragment with SearchBar { me =>
  lazy val host: LNStartActivity = getActivity.asInstanceOf[LNStartActivity]
  override def onCreateView(inf: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inf.inflate(R.layout.frag_ln_start, vg, false)

  override def onViewCreated(view: View, state: Bundle) = {
    val lnStartNodesList = view.findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
    val toolbar = view.findViewById(R.id.toolbar).asInstanceOf[android.support.v7.widget.Toolbar]
    var nodes = Vector.empty[StartNodeView]

    val acinqKey = PublicKey("03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f")
    val acinqAnnouncement = app.mkNodeAnnouncement(nodeId = acinqKey, host = "34.239.230.56", 9735)
    val acinq = HardcodedNodeView(acinqAnnouncement, "<strong>Recommended ACINQ node</strong>")

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

    def onSelect(pos: Int) = {
      app.TransData.value = adapter getItem pos
      host goTo classOf[LNStartFundActivity]
    }

    new ThrottledWork[String, AnnounceChansNumVec] {
      def work(userQuery: String) = findNodes(userQuery)
      def error(error: Throwable) = Tools errlog error
      me.react = addWork

      def process(ask: String, res: AnnounceChansNumVec) = {
        val remoteNodeViewWraps = for (acn <- res) yield RemoteNodeView(acn)
        nodes = if (ask.isEmpty) acinq +: remoteNodeViewWraps else remoteNodeViewWraps
        host.UITask(adapter.notifyDataSetChanged).run
      }
    }

    FragLNStart.fragment = me
    host.setSupportActionBar(toolbar)
    host.getSupportActionBar.setTitle(action_ln_open)
    host.getSupportActionBar.setSubtitle(ln_status_peer)
    lnStartNodesList.setOnItemClickListener(host onTap onSelect)
    lnStartNodesList.setAdapter(adapter)
    react(new String)
  }
}

// DISPLAYING NODES ON UI

object StartNodeView {
  lazy val nodeView = app getString ln_ops_start_node_view
  lazy val nodeFundView = app getString ln_ops_start_fund_node_view
  lazy val chansNumber = app.getResources getStringArray R.array.ln_ops_start_node_channels
}

sealed trait StartNodeView { def asString(base: String, separator: String): String }
// This invariant comes from dev recommended nodes but also when a node QR is scanned
case class HardcodedNodeView(ann: NodeAnnouncement, tip: String) extends StartNodeView {

  def asString(base: String, separator: String) = {
    val key = humanNode(ann.nodeId.toString, separator)
    base.format(ann.alias, tip, key)
  }
}

// This invariant comes as a search result from Olympus server queries
case class RemoteNodeView(acn: AnnounceChansNum) extends StartNodeView {

  def asString(base: String, separator: String) = {
    val channelAnnouncement \ channelConnections = acn
    val humanConnects = app.plurOrZero(chansNumber, channelConnections)
    val key = humanNode(channelAnnouncement.nodeId.toString, separator)
    base.format(channelAnnouncement.alias, humanConnects, key)
  }
}