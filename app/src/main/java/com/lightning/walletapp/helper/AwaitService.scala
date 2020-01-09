package com.lightning.walletapp.helper

import android.content.{Context, Intent}
import android.app.{NotificationManager, PendingIntent, Service}
import android.support.v4.app.NotificationCompat
import com.lightning.walletapp.ln.Tools.runAnd
import com.lightning.walletapp.MainActivity
import com.lightning.walletapp.R


object AwaitService {
  val classof = classOf[AwaitService]
  val CHANNEL_ID = "awaitChannelId"
  val SHOW_AMOUNT = "showAmount"
  val CANCEL = "awaitCancel"
  val NOTIFICATION_ID = 14
}

class AwaitService extends Service { me =>
  override def onBind(intent: Intent) = null

  override def onDestroy = runAnd(super.onDestroy){
    val service = getSystemService(Context.NOTIFICATION_SERVICE)
    service.asInstanceOf[NotificationManager] cancel AwaitService.NOTIFICATION_ID
  }

  override def onStartCommand(serviceIntent: Intent, flags: Int, id: Int) = {
    if (serviceIntent.getAction != AwaitService.CANCEL) start(serviceIntent)
    else runAnd(me stopForeground true)(stopSelf)
    Service.START_NOT_STICKY
  }

  def start(intent: Intent) = {
    val awaitedPaymentSum = intent getStringExtra AwaitService.SHOW_AMOUNT
    val pendingActivity = PendingIntent.getActivity(me, 0, new Intent(me, MainActivity.wallet), 0)
    val cancelIntent = PendingIntent.getService(me, 0, new Intent(me, AwaitService.classof).setAction(AwaitService.CANCEL), 0)
    startForeground(AwaitService.NOTIFICATION_ID, new NotificationCompat.Builder(me, AwaitService.CHANNEL_ID).setContentIntent(pendingActivity)
      .addAction(android.R.drawable.ic_menu_close_clear_cancel, getResources getString R.string.dialog_cancel, cancelIntent)
      .setSmallIcon(R.drawable.ic_info_outline_white_18dp).setContentTitle(getResources getString R.string.notify_title)
      .setContentText(getResources getString R.string.notify_body format awaitedPaymentSum).build)
  }
}