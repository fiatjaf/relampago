package com.lightning.walletapp.helper

import android.app.{PendingIntent, Service}
import android.support.v4.app.NotificationCompat
import com.lightning.walletapp.MainActivity
import com.lightning.walletapp.R
import android.content.Intent


object AwaitService {
  val classof = classOf[AwaitService]
  val CHANNEL_ID = "awaitChannelId"
  val SHOW_AMOUNT = "showAmount"
  val CANCEL = "awaitCancel"
}

class AwaitService extends Service { me =>
  override def onBind(intent: Intent) = null
  override def onStartCommand(intent: Intent, flags: Int, id: Int) = {
    if (intent.getAction == AwaitService.CANCEL) stop else start(intent)
    Service.START_NOT_STICKY
  }

  def start(intent: Intent) = {
    val awaitedPaymentSum = intent getStringExtra AwaitService.SHOW_AMOUNT
    val pendingActivity = PendingIntent.getActivity(me, 0, new Intent(me, MainActivity.wallet), 0)
    val cancelIntent = PendingIntent.getService(me, 0, new Intent(me, AwaitService.classof).setAction(AwaitService.CANCEL), 0)

    startForeground(1, new NotificationCompat.Builder(me, AwaitService.CHANNEL_ID).setContentIntent(pendingActivity)
      .addAction(android.R.drawable.ic_menu_close_clear_cancel, getResources getString R.string.dialog_cancel, cancelIntent)
      .setSmallIcon(R.drawable.ic_info_outline_white_18dp).setContentTitle(getResources getString R.string.notify_title)
      .setContentText(getResources getString R.string.notify_body format awaitedPaymentSum).build)
  }

  def stop = {
    me stopForeground true
    stopSelf
  }
}