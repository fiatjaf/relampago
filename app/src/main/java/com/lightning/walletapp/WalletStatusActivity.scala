package com.lightning.walletapp

import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.lnutils.JointNode._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import android.view.{Menu, MenuItem, View, ViewGroup}
import android.widget.{BaseAdapter, LinearLayout, ListView, TextView}
import com.lightning.walletapp.WalletStatusActivity.allItems
import com.lightning.walletapp.lnutils.ChannelBalances
import com.lightning.walletapp.ln.Tools.wrap
import android.support.v7.widget.Toolbar
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.MilliSatoshi
import android.content.Intent
import android.os.Bundle
import android.net.Uri


object WalletStatusActivity { me =>
  def updateItems(cbs: ChannelBalances): Unit = {
    // Only show what can be sent via Joint -> payee if do not have a phone -> Joint channel because `math.min` below
    val me2JointMaxSendable = relayPeerReports.map(_.softReserveCanSend).reduceOption(_ max _) getOrElse Long.MaxValue
    val perPeerMaxSendable = cbs.localBalances.sortBy(- _.withoutMaxFee).groupBy(_.peerNodeId).mapValues(_.head)

    val updatedMap = for {
      jointPeersNodeKey \ title <- mapping
      cbi <- perPeerMaxSendable get jointPeersNodeKey
      min = math.min(me2JointMaxSendable, cbi.withoutMaxFee)
    } yield title -> MilliSatoshi(min)
    allItems = updatedMap.toList
  }

  val mapping = Map (
    PublicKey("0282734a2ca9bdca61859c71efdc43234ea664ecba9a785f780c6eec8c80243b45") -> "buda.com",
    PublicKey("03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f") -> "strike.acinq.co",
    PublicKey("0260fab633066ed7b1d9b9b8a0fac87e1579d1709e874d28a0d171a1f5c43bb877") -> "southxchange.com"
  )

  type TitleAndSendable = (String, MilliSatoshi)
  var allItems = List.empty[TitleAndSendable]
}

class WalletStatusActivity extends TimerActivity with HumanTimeDisplay { me =>
  lazy val jointNodeInfo = findViewById(R.id.jointNodeInfo).asInstanceOf[LinearLayout]
  lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]
  lazy val host = me

  val adapter = new BaseAdapter {
    def getItem(position: Int) = allItems(position)
    def getItemId(position: Int) = position
    def getCount = allItems.size

    def getView(itemPosition: Int, savedView: View, parent: ViewGroup) = {
      val view = host.getLayoutInflater.inflate(R.layout.frag_line_double, null)
      val maxSendField = view.findViewById(R.id.rightSideLine).asInstanceOf[TextView]
      val nameField = view.findViewById(R.id.leftSideLine).asInstanceOf[TextView]
      val nameString \ maxSendMsat = getItem(itemPosition)

      view setOnClickListener onButtonTap {
        val uri = Uri parse s"https://$nameString"
        val intent = new Intent(Intent.ACTION_VIEW, uri)
        startActivity(intent)
      }

      // Set listener on per-item basis because XML properties
      maxSendField setText denom.parsedWithSign(maxSendMsat).html
      nameField setText nameString
      view
    }
  }

  val killWS = makeWebSocket(defaultJointAnn) { raw =>
    WalletStatusActivity updateItems to[ChannelBalances](raw)
    UITask(adapter.notifyDataSetChanged).run
  }

  override def onDestroy = wrap(super.onDestroy)(killWS.run)

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.status, menu)
    true
  }

  override def onOptionsItemSelected(m: MenuItem) = {
    val arbitrageApi = Uri parse "http://lightning-wallet.com/joint/arbitrage-api"
    me startActivity new Intent(Intent.ACTION_VIEW, arbitrageApi)
    true
  }

  def openJointChannel(top: View) = {
    app.TransData.value = defaultJointAnn
    me exitTo classOf[LNStartFundActivity]
  }

  def INIT(s: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_wallet_status
    Utils clickableTextField findViewById(R.id.jointNodeHint)
    jointNodeInfo setVisibility viewMap(relayPeerReports.isEmpty)
    me initToolbar findViewById(R.id.toolbar).asInstanceOf[Toolbar]
    getSupportActionBar setTitle joint_title
    // Update because items may be cached
    itemsList setAdapter adapter
    adapter.notifyDataSetChanged

    // Or back if resources are freed
  } else me exitTo classOf[MainActivity]
}