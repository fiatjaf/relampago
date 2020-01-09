package com.lightning.walletapp.helper

import fr.acinq.bitcoin.MilliSatoshi
import com.lightning.walletapp.MainActivity
import com.lightning.walletapp.ln.Tools.none
import android.support.v4.app.NotificationCompat
import android.app.ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND
import android.app.{ActivityManager, AlarmManager, NotificationManager, PendingIntent}
import android.content.{BroadcastReceiver, Context, Intent}
import com.lightning.walletapp.Utils.{app, denom}
import scala.collection.JavaConverters._


object Notificator {
  val EXTRA_CONTENT = "extraContent"
  def paymentReceived(sum: MilliSatoshi) = try {
    val parametersIntent = new Intent(app, Notificator.getClass).putExtra(EXTRA_CONTENT, denom asString sum)
    val alarmManager = app.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]
    val pendingIntent = PendingIntent.getBroadcast(app, 0, parametersIntent, 0)
    alarmManager.set(AlarmManager.RTC_WAKEUP, 0, pendingIntent)
  } catch none
}

class Notificator extends BroadcastReceiver { me =>
  def onReceive(context: Context, intent: Intent) = try {
    val content = intent.getExtras.getString(Notificator.EXTRA_CONTENT)
    val targetIntent = PendingIntent.getActivity(context, 0, new Intent(context, MainActivity.getClass), PendingIntent.FLAG_UPDATE_CURRENT)
    val notification = new NotificationCompat.Builder(context).setContentIntent(targetIntent).setAutoCancel(true).setContentText(content).build
    if (me isAppInBackground context) context.getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager].notify(1, notification)
  } catch none

  def isAppInBackground(context: Context) = {
    val services = context.getSystemService(Context.ACTIVITY_SERVICE).asInstanceOf[ActivityManager].getRunningAppProcesses.asScala
    !services.exists(srv => srv.processName.equalsIgnoreCase(context.getPackageName) && srv.importance == IMPORTANCE_FOREGROUND)
  }
}