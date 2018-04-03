package com.lightning.wallet

import java.io.{PrintWriter, StringWriter}
import java.lang.Thread.UncaughtExceptionHandler
import com.lightning.wallet.AbstractKit.ERROR_REPORT
import android.content.Intent
import android.app.Activity


object UncaughtHandler {
  def toText(exc: Throwable) = {
    val stackTraceWriter = new StringWriter
    exc printStackTrace new PrintWriter(stackTraceWriter)
    stackTraceWriter.toString
  }
}

class UncaughtHandler(ctxt: Activity)
extends UncaughtExceptionHandler {

  def uncaughtException(thread: Thread, exc: Throwable): Unit = {
    val emerge: Class[EmergencyActivity] = classOf[EmergencyActivity]
    val content = UncaughtHandler toText exc
    val intent = new Intent(ctxt, emerge)

    ctxt startActivity intent.putExtra(ERROR_REPORT, content)
    android.os.Process killProcess android.os.Process.myPid
    System exit 10
  }
}
