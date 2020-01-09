package com.lightning.walletapp

import android.view._
import android.support.v4.app._
import com.journeyapps.barcodescanner._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.R.string._
import android.support.v4.content.ContextCompat
import android.Manifest.permission.CAMERA
import android.content.pm.PackageManager
import com.lightning.walletapp.Utils.app
import android.support.v4.view.ViewPager
import android.widget.ImageButton
import android.os.Bundle


trait ScanActivity extends TimerActivity {
  lazy val walletPager = findViewById(R.id.walletPager).asInstanceOf[ViewPager]
  val slidingFragmentAdapter: FragmentStatePagerAdapter
  var lastAttempt = System.currentTimeMillis
  var barcodeReader: BarcodeView = _

  def checkTransData: Unit
  def returnToBase(view: View) = {
    // Hack to fix a strange behavior
    walletPager.setCurrentItem(1, false)
    walletPager.setCurrentItem(0, false)
  }

  def toggleTorch(view: View) = {
    val image = view.asInstanceOf[ImageButton]
    val currentTag = image.getTag.asInstanceOf[Int]

    if (currentTag != R.drawable.baseline_brightness_high_white_36) {
      image.setImageResource(R.drawable.baseline_brightness_high_white_36)
      image.setTag(R.drawable.baseline_brightness_high_white_36)
      barcodeReader.setTorch(true)
    } else {
      image.setImageResource(R.drawable.baseline_brightness_low_white_36)
      image.setTag(R.drawable.baseline_brightness_low_white_36)
      barcodeReader.setTorch(false)
    }
  }
}

class FragScan extends Fragment with BarcodeCallback { me =>
  type Points = java.util.List[com.google.zxing.ResultPoint]
  lazy val host = getActivity.asInstanceOf[ScanActivity]
  import host._

  override def onCreateView(inflator: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inflator.inflate(R.layout.frag_view_pager_scan, vg, false)

  override def onViewCreated(view: View, savedInstanceState: Bundle) = if (app.isAlive) {
    val cameraAllowed = ContextCompat.checkSelfPermission(host, CAMERA) == PackageManager.PERMISSION_GRANTED
    if (!cameraAllowed) ActivityCompat.requestPermissions(host, Array(CAMERA), 104)
    barcodeReader = view.findViewById(R.id.reader).asInstanceOf[BarcodeView]
  }

  override def setUserVisibleHint(isVisibleToUser: Boolean) = {
    if (isAdded && isVisibleToUser) runAnd(barcodeReader decodeContinuous me)(barcodeReader.resume)
    else if (isAdded) runAnd(getFragmentManager.beginTransaction.detach(me).attach(me).commit)(pauseReader)
    super.setUserVisibleHint(isVisibleToUser)
  }

  // Only try to decode result after some time
  override def possibleResultPoints(points: Points) = none
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 2000) tryParseQR(rawText)
  }

  def tryParseQR(text: String) = {
    def fail(err: Throwable) = runAnd(app quickToast err_nothing_useful)(barcodeReader.resume)
    <(app.TransData recordValue text, fail)(parseSuccess => host.checkTransData)
    lastAttempt = System.currentTimeMillis
    pauseReader
  }

  def pauseReader = {
    barcodeReader.setTorch(false)
    barcodeReader.pause
  }
}