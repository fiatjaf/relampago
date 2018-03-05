package com.lightning.wallet

import android.net.Uri
import android.os.Bundle
import android.support.v7.widget._
import android.support.v7.widget.helper.ItemTouchHelper
import com.lightning.wallet.lnutils.olympus.OlympusWrap._
import android.view.{Menu, MenuItem, ViewGroup}
import android.widget.{CheckBox, EditText, TextView}
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.Utils.app
import com.lightning.wallet.lnutils.olympus.Cloud

import scala.collection.JavaConverters._
import com.thesurix.gesturerecycler._


class OlympusActivity extends TimerActivity { me =>
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  lazy val serverList = findViewById(R.id.serverList).asInstanceOf[RecyclerView]
  lazy val tokensLeft = getResources getStringArray R.array.olympus_tokens_left

  val adapter = new GestureAdapter[Cloud, GestureViewHolder] {
    override def onCreateViewHolder(parent: ViewGroup, viewType: Int) = {
      val view = getLayoutInflater.inflate(R.layout.frag_olympus_line, parent, false)
      new GestureViewHolder(view) {
        override def canDrag: Boolean = true
        override def canSwipe: Boolean = false
      }
    }

    override def onBindViewHolder(holder: GestureViewHolder, pos: Int) = {
      val olympusAddress = holder.itemView.findViewById(R.id.olympusAddress).asInstanceOf[TextView]
      val olympusTokens = holder.itemView.findViewById(R.id.olympusTokens).asInstanceOf[TextView]

      val cloud = getItem(pos)
      val address = Uri.parse(cloud.connector.url)
      olympusTokens setText app.plurOrZero(tokensLeft, cloud.data.tokens.size)
      olympusAddress setText address.getHost + ":" + address.getPort
    }
  }

  def INIT(savedInstanceState: Bundle) = {
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_olympus)
    getSupportActionBar setTitle sets_manage_olympus

    serverList setLayoutManager new LinearLayoutManager(me)
    serverList setHasFixedSize true
    adapter.setData(clouds.asJava)
    serverList setAdapter adapter

    serverList addOnItemTouchListener new RecyclerItemTouchListener(new DefaultItemClickListener[Cloud] {
      override def onItemClick(item: Cloud, position: Int) = {
        println(item)
        false
      }

      override def onItemLongPress(item: Cloud, position: Int) = none

      override def onDoubleTap(item: Cloud, position: Int) = false
    })

    adapter setDataChangeListener new GestureAdapter.OnDataChangeListener[Cloud] {
      override def onItemRemoved(item: Cloud, position: Int) {
      }

      override def onItemReorder(item: Cloud, fromPos: Int, toPos: Int): Unit = {
        println(adapter.getData.asScala.map(_.connector.url))
      }
    }
    new GestureManager.Builder(serverList)
      .setDragFlags(ItemTouchHelper.UP | ItemTouchHelper.DOWN)
      .setLongPressDragEnabled(true)
      .build
  }

  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionAddOlympus) new FormManager
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.olympus, menu)
    true
  }

  class FormManager {
    val content = getLayoutInflater.inflate(R.layout.frag_olympus_details, null, false)
    val serverHostPort = content.findViewById(R.id.serverHostPort).asInstanceOf[EditText]
    val serverBackup = content.findViewById(R.id.serverBackup).asInstanceOf[CheckBox]
    mkForm(me negBld dialog_ok, getString(olympus_add), content)
  }
}
