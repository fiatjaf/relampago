package com.lightning.walletapp

import android.support.v7.widget._
import com.lightning.walletapp.ln._
import com.thesurix.gesturerecycler._
import scala.collection.JavaConverters._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.lnutils.olympus._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import android.support.v7.widget.helper.ItemTouchHelper
import com.lightning.walletapp.ln.LNParams
import com.lightning.walletapp.Utils.app
import org.bitcoinj.core.Utils.HEX
import android.app.AlertDialog
import android.os.Bundle
import android.net.Uri

import android.widget.{CheckBox, EditText, TextView}
import android.view.{Menu, MenuItem, ViewGroup}


class OlympusActivity extends TimerActivity { me =>
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  lazy val serverList = findViewById(R.id.serverList).asInstanceOf[RecyclerView]
  lazy val tokensLeft = getResources getStringArray R.array.olympus_tokens_left

  val adapter = new GestureAdapter[Cloud, GestureViewHolder] {
    override def onCreateViewHolder(parent: ViewGroup, viewType: Int) = {
      val view = getLayoutInflater.inflate(R.layout.frag_olympus_line, parent, false)
      new GestureViewHolder(view)
    }

    override def onBindViewHolder(holder: GestureViewHolder, pos: Int) = {
      val olympusAddress = holder.itemView.findViewById(R.id.olympusAddress).asInstanceOf[TextView]
      val olympusTokens = holder.itemView.findViewById(R.id.olympusTokens).asInstanceOf[TextView]

      val cloud = getItem(pos)
      val serverAddress = Uri.parse(cloud.connector.url)
      val tokensLeftHuman = app.plurOrZero(tokensLeft, cloud.data.tokens.size)
      val finalTokensLeft = if (cloud.isAuthEnabled) tokensLeftHuman else tokensLeft.last

      olympusAddress setText serverAddress.getHost
      olympusTokens setText finalTokensLeft.html
      holder.swipable = cloud.removable == 1
    }
  }

  val onClick = new DefaultItemClickListener[Cloud] {
    override def onItemClick(item: Cloud, position: Int) = {
      new FormManager(updateCloud(item), olympus_edit) set item
      false
    }
  }

  def INIT(savedInstanceState: Bundle) = {
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_olympus)
    wrap(getSupportActionBar setTitle sets_manage_olympus)(getSupportActionBar setSubtitle olympus_actions)
    Utils clickableTextField findViewById(R.id.serverWhat)

    adapter setData clouds.asJava
    adapter setDataChangeListener new GestureAdapter.OnDataChangeListener[Cloud] {
      override def onItemReorder(item: Cloud, fromPos: Int, targetPos: Int) = onUpdate
      override def onItemRemoved(item: Cloud, position: Int) = onUpdate
    }

    serverList setAdapter adapter
    serverList setHasFixedSize true
    serverList setLayoutManager new LinearLayoutManager(me)
    serverList addOnItemTouchListener new RecyclerItemTouchListener(onClick)

    new GestureManager.Builder(serverList)
      .setSwipeEnabled(true).setLongPressDragEnabled(true)
      .setDragFlags(ItemTouchHelper.UP | ItemTouchHelper.DOWN)
      .setSwipeFlags(ItemTouchHelper.LEFT).build
  }

  def onUpdate = LNParams.db txWrap {
    val updated = adapter.getData.asScala.toVector
    for (removed <- clouds diff updated) remove(removed.identifier)
    for (cloud \ order <- updated.zipWithIndex) addServer(cloud, order)
    for (cloud \ order <- updated.zipWithIndex) updMeta(cloud, order)
    adapter.notifyDataSetChanged
    clouds = updated
  }

  def addNewCloud(url: String, auth: Int) = {
    val randomIdentity = HEX.encode(random getBytes 16)
    val emptyData = CloudData(info = None, tokens = Vector.empty, acts = Vector.empty)
    val cd = new Cloud(randomIdentity, new Connector(url), auth, 1) { data = emptyData }
    if (adapter add cd) onUpdate
  }

  def updateCloud(cloud: Cloud)(url: String, auth: Int) = {
    // Just update mutable fields and insert them into database
    // won't be re-addded because of INSERT IGNORE sql
    cloud.connector = new Connector(url)
    cloud.auth = auth
    onUpdate
  }

  override def onOptionsItemSelected(m: MenuItem) = {
    if (m.getItemId == R.id.actionAddEntity) new FormManager(addNewCloud, olympus_add)
    true
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.add_entity, menu)
    true
  }

  class FormManager(next: (String, Int) => Unit, title: Int) {
    val content = getLayoutInflater.inflate(R.layout.frag_olympus_details, null, false)
    val serverHostPort = content.findViewById(R.id.serverHostPort).asInstanceOf[EditText]
    val serverBackup = content.findViewById(R.id.serverBackup).asInstanceOf[CheckBox]

    mkCheckForm(addAttempt, none, baseBuilder(getString(title), content), dialog_ok, dialog_cancel)
    def set(c: Cloud) = wrap(serverHostPort setText c.connector.url)(serverBackup setChecked c.isAuthEnabled)

    def addAttempt(alert: AlertDialog): Unit = {
      val uriChecker = Uri parse serverHostPort.getText.toString
      if (uriChecker.getHost == null || uriChecker.getPort < 80) return
      next(uriChecker.toString, if (serverBackup.isChecked) 1 else 0)
      alert.dismiss
    }
  }
}