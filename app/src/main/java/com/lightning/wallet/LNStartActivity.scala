package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.lnutils.olympus.OlympusWrap._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import android.widget.{BaseAdapter, ListView, TextView}
import android.view.{Menu, View, ViewGroup}

import com.lightning.wallet.helper.ThrottledWork
import com.lightning.wallet.ln.Tools.runAnd
import com.lightning.wallet.ln.Tools.wrap
import android.support.v7.widget.Toolbar
import android.os.Bundle


class LNStartActivity extends TimerActivity with SearchBar { me =>
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val nodeView = getString(ln_ops_start_node_view)
  private var nodes = Vector.empty[AnnounceChansNum]

  val adapter = new BaseAdapter {
    def getView(pos: Int, cv: View, parent: ViewGroup) = {
      val view = getLayoutInflater.inflate(R.layout.frag_single_line, null)
      val textLine = view.findViewById(R.id.textLine).asInstanceOf[TextView]

      val announce \ connections = getItem(pos)
      val humanConnects = app.plurOrZero(chansNumber, connections)
      val theirNodeKey = humanNode(announce.nodeId.toString, "\u0020")
      val txt = nodeView.format(announce.alias, humanConnects, theirNodeKey)
      textLine setText txt.html
      view
    }

    def getItem(position: Int) = nodes(position)
    def getItemId(position: Int) = position
    def getCount = nodes.size
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    new ThrottledWork[String, AnnounceChansNumVec] {
      def updateView = UITask(adapter.notifyDataSetChanged).run
      def work(queryAliasOrNodeKey: String) = findNodes(queryAliasOrNodeKey)
      def process(res: AnnounceChansNumVec) = wrap(updateView) { nodes = res }
      def error(err: Throwable) = Tools errlog err
      me.react = addWork
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