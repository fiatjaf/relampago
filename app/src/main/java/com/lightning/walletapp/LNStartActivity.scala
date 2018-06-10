package com.lightning.walletapp

import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.StartNodeView._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.ln.wire.NodeAnnouncement
import com.lightning.walletapp.helper.ThrottledWork
import com.lightning.walletapp.ln.Tools.runAnd
import com.lightning.walletapp.ln.Tools.wrap
import fr.acinq.bitcoin.Crypto.PublicKey
import android.os.Bundle

import android.widget.{BaseAdapter, ListView, TextView}
import android.view.{Menu, View, ViewGroup}


class LNStartActivity extends TimerActivity with SearchBar { me =>
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[android.support.v7.widget.Toolbar]
  private var nodes = Vector.empty[StartNodeView]

  private val acinqKey = PublicKey("03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f")
  private val acinqAnnouncement = app.mkNodeAnnouncement(nodeId = acinqKey, host = "34.239.230.56", 9735)
  private val acinq = HardcodedNodeView(acinqAnnouncement, "<strong>Recommended ACINQ node</strong>")

  val adapter = new BaseAdapter {
    def getView(pos: Int, cv: View, par: ViewGroup) = {
      val view = getLayoutInflater.inflate(R.layout.frag_single_line, null)
      val textLine = view.findViewById(R.id.textLine).asInstanceOf[TextView]
      textLine setText getItem(pos).asString(nodeView, "\u0020").html
      view
    }

    def getItem(position: Int) = nodes(position)
    def getItemId(position: Int) = position
    def getCount = nodes.size
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    new ThrottledWork[String, AnnounceChansNumVec] {
      def work(userQuery: String) = findNodes(userQuery)
      def error(error: Throwable) = Tools errlog error
      me.react = addWork

      def process(ask: String, res: AnnounceChansNumVec) = {
        val remoteNodeViewWraps = for (acn <- res) yield RemoteNodeView(acn)
        nodes = if (ask.isEmpty) acinq +: remoteNodeViewWraps else remoteNodeViewWraps
        UITask(adapter.notifyDataSetChanged).run
      }
    }

    // Set action bar, content view, title and subtitle text, wire up listeners
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_ln_start)
    wrap(getSupportActionBar setTitle action_ln_open)(getSupportActionBar setSubtitle ln_status_peer)
    lnStartNodesList setOnItemClickListener onTap(onPeerSelected)
    lnStartNodesList setAdapter adapter
    react(new String)

    // Or back if resources are freed
  } else me exitTo classOf[MainActivity]

  override def onCreateOptionsMenu(menu: Menu) = runAnd(true) {
    // Can search nodes by their aliases and use a QR node scanner
    getMenuInflater.inflate(R.menu.ln_start, menu)
    setupSearch(menu)
  }

  private def onPeerSelected(pos: Int) = {
    app.TransData.value = adapter getItem pos
    me goTo classOf[LNStartFundActivity]
  }
}

object StartNodeView {
  lazy val nodeView = app getString ln_ops_start_node_view
  lazy val nodeFundView = app getString ln_ops_start_fund_node_view
  lazy val chansNumber = app.getResources getStringArray R.array.ln_ops_start_node_channels
}

sealed trait StartNodeView { def asString(base: String, separator: String): String }
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