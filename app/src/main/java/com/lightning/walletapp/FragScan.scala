package com.lightning.walletapp

import android.view._
import com.journeyapps.barcodescanner._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.R.string._

import android.support.v4.app.{ActivityCompat, Fragment}
import android.support.v4.content.ContextCompat
import android.content.pm.PackageManager
import com.lightning.walletapp.Utils.app
import android.support.v4.view.ViewPager
import android.os.Bundle


trait ScanActivity extends TimerActivity {
  lazy val walletPager = findViewById(R.id.walletPager).asInstanceOf[ViewPager]
  def returnToBase(view: View) = walletPager.setCurrentItem(0, true)
  def checkTransData: Unit
}

class FragScan extends Fragment with BarcodeCallback { me =>
  type Points = java.util.List[com.google.zxing.ResultPoint]
  lazy val host = getActivity.asInstanceOf[ScanActivity]
  var lastAttempt = System.currentTimeMillis
  var barcodeReader: BarcodeView = _
  import host._

  override def onCreateView(inflator: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inflator.inflate(R.layout.frag_view_pager_scan, vg, false)

  override def onViewCreated(view: View, savedInstanceState: Bundle) = {
    val camDenied = ContextCompat.checkSelfPermission(host, android.Manifest.permission.CAMERA) != PackageManager.PERMISSION_GRANTED
    if (camDenied) ActivityCompat.requestPermissions(host, Array(android.Manifest.permission.CAMERA), 102)
    barcodeReader = view.findViewById(R.id.reader).asInstanceOf[BarcodeView]
  }

  override def setUserVisibleHint(isVisibleToUser: Boolean) = {
    if (isAdded && isVisibleToUser) runAnd(barcodeReader decodeContinuous me)(barcodeReader.resume)
    else if (isAdded) runAnd(getFragmentManager.beginTransaction.detach(me).attach(me).commit)(barcodeReader.pause)
    super.setUserVisibleHint(isVisibleToUser)
  }

  // Only try to decode result after some time
  override def possibleResultPoints(points: Points) = none
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 3000) tryParseQR(rawText)
  }

  def tryParseQR(text: String) = {
    def fail(err: Throwable) = runAnd(app toast err_no_data)(barcodeReader.resume)
    <(app.TransData recordValue text, fail)(ok => host.checkTransData)
    lastAttempt = System.currentTimeMillis
    barcodeReader.pause
  }
}