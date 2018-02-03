package com.lightning.wallet

import R.string._
import com.lightning.wallet.Utils._
import com.journeyapps.barcodescanner._
import com.lightning.wallet.ln.Tools.{none, wrap}
import android.content.DialogInterface.BUTTON_NEGATIVE
import android.os.Bundle


class ScanActivity extends TimerActivity with BarcodeCallback { me =>
  lazy val reader = findViewById(R.id.reader).asInstanceOf[BarcodeView]
  type Points = java.util.List[com.google.zxing.ResultPoint]
  private[this] var lastAttempt = System.currentTimeMillis

  def INIT(savedInstance: Bundle) = {
    setContentView(R.layout.activity_scan)
    reader decodeContinuous me
  }

  def tryParseQR(scannedText: String) = try {
    // This may throw, but this is fine here
    lastAttempt = System.currentTimeMillis
    app.TransData recordValue scannedText
    me exitTo classOf[WalletActivity]

  } catch app.TransData.onFail { code =>
    val alert = showForm(negBld(dialog_ok).setMessage(code).create)
    alert getButton BUTTON_NEGATIVE setOnClickListener onButtonTap(proceed)
    def proceed = rm(alert)(reader.resume)
    app toast scannedText

    // Pause anyway
  } finally reader.pause

  // Only try to decode result if 3 seconds elapsed
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 2500) tryParseQR(rawText)
  }

  override def possibleResultPoints(pts: Points) = none
  override def onResume = wrap(super.onResume)(reader.resume)
  override def onPause = wrap(super.onPause)(reader.pause)
}