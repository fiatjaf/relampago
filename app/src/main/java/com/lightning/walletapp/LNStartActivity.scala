package com.lightning.walletapp

import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import android.view.{Menu, View, ViewGroup}
import android.widget.{BaseAdapter, ListView, TextView}
import com.lightning.walletapp.ln.wire.NodeAnnouncement
import com.lightning.walletapp.helper.ThrottledWork
import com.lightning.walletapp.ln.Tools.runAnd
import com.lightning.walletapp.ln.Tools.wrap
import android.support.v7.widget.Toolbar
import fr.acinq.bitcoin.Crypto.PublicKey
import android.os.Bundle


class LNStartActivity extends TimerActivity with SearchBar { me =>
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val nodeView = getString(ln_ops_start_node_view)
  private var nodes = Vector.empty[StartNodeView]

  private val ohHiMarkKey = PublicKey("03dc39d7f43720c2c0f86778dfd2a77049fa4a44b4f0a8afb62f3921567de41375")
  private val enduranceKey = PublicKey("03933884aaf1d6b108397e5efe5c86bcf2d8ca8d2f700eda99db9214fc2712b134")
  private val endurance = HardcodedNodeView(app.mkNodeAnnouncement(enduranceKey, "34.250.234.192", 9735), "<i>ACINQ node</i>")
  private val ohHiMark = HardcodedNodeView(app.mkNodeAnnouncement(ohHiMarkKey, "192.210.203.16", 9735), "<i>Bitcoin + Lightning node</i>")

  val adapter = new BaseAdapter {
    def getView(pos: Int, cv: View, parent: ViewGroup) = {
      val view = getLayoutInflater.inflate(R.layout.frag_single_line, null)
      val textLine = view.findViewById(R.id.textLine).asInstanceOf[TextView]

      getItem(pos) match {
        case RemoteNodeView(acn) =>
          val announce \ connections = acn
          val humanConnects = app.plurOrZero(chansNumber, connections)
          val theirNodeKey = humanNode(announce.nodeId.toString, "\u0020")
          val txt = nodeView.format(announce.alias, humanConnects, theirNodeKey)
          textLine setText txt.html

        case HardcodedNodeView(announce, tip) =>
          val theirNodeKey = humanNode(announce.nodeId.toString, "\u0020")
          val txt = nodeView.format(announce.alias, tip, theirNodeKey)
          view setBackgroundColor 0xFFEEFFEE
          textLine setText txt.html
      }

      view
    }

    def getItem(position: Int) = nodes(position)
    def getItemId(position: Int) = position
    def getCount = nodes.size
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    new ThrottledWork[String, AnnounceChansNumVec] {
      def work(userQuery: String) = findNodes(userQuery)
      def error(err: Throwable) = Tools errlog err
      me.react = addWork

      def process(ask: String, res: AnnounceChansNumVec) = {
        val remoteNodeViewWraps = res map RemoteNodeView.apply
        val augmentedViews = endurance +: ohHiMark +: remoteNodeViewWraps
        nodes = if (ask.isEmpty) augmentedViews else remoteNodeViewWraps
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

sealed trait StartNodeView
case class RemoteNodeView(acn: AnnounceChansNum) extends StartNodeView
case class HardcodedNodeView(ann: NodeAnnouncement, tip: String) extends StartNodeView