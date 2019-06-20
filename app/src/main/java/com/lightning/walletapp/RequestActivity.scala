package com.lightning.walletapp

import android.graphics._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import android.text.Layout.Alignment.ALIGN_NORMAL
import android.graphics.Bitmap.Config.ARGB_8888
import android.support.v4.content.FileProvider
import com.google.zxing.qrcode.QRCodeWriter
import android.graphics.Bitmap.createBitmap
import android.transition.TransitionManager
import org.bitcoinj.core.Address
import android.content.Intent
import scodec.bits.ByteVector
import android.view.View
import android.os.Bundle

import android.widget.{ImageButton, ImageView, LinearLayout}
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import com.lightning.walletapp.ln.Tools.{none, wrap}
import android.text.{StaticLayout, TextPaint}
import java.io.{File, FileOutputStream}


object QRGen {
  val writer = new QRCodeWriter
  val hints = new java.util.Hashtable[EncodeHintType, Any]
  hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.M)
  hints.put(EncodeHintType.MARGIN, 1)

  def get(data: String, size: Int): Bitmap = {
    val bitMatrix = writer.encode(data, BarcodeFormat.QR_CODE, size, size, hints)
    val wid \ height = Tuple2(bitMatrix.getWidth, bitMatrix.getHeight)
    val pixels = new Array[Int](wid * height)

    for (y <- 0 until height) for (x <- 0 until wid)
      pixels(y * wid + x) = bitMatrix.get(x, y) match {
        case true => Color.BLACK case false => Color.WHITE
      }

    val qrBitmap = createBitmap(wid, height, ARGB_8888)
    qrBitmap.setPixels(pixels, 0, wid, 0, 0, wid, height)
    qrBitmap
  }
}

class RequestActivity extends TimerActivity { me =>
  lazy val reqContainer = findViewById(R.id.reqContainer).asInstanceOf[LinearLayout]
  lazy val reqFulfilled = findViewById(R.id.reqFulfilled).asInstanceOf[LinearLayout]
  lazy val reqOptions = findViewById(R.id.reqOptions).asInstanceOf[LinearLayout]
  lazy val shareText = findViewById(R.id.shareText).asInstanceOf[ImageButton]
  lazy val shareQR = findViewById(R.id.shareQR).asInstanceOf[ImageButton]
  lazy val reqCode = findViewById(R.id.reqCode).asInstanceOf[ImageView]

  lazy val textBounds = getResources getDimensionPixelSize R.dimen.bitmap_text_bounds
  lazy val bottomSize = getResources getDimensionPixelSize R.dimen.bitmap_bottom_size
  lazy val topSize = getResources getDimensionPixelSize R.dimen.bitmap_top_size
  lazy val qrSize = getResources getDimensionPixelSize R.dimen.bitmap_qr_size

  var whenDestroy: Runnable = new Runnable { def run = none }
  override def onDestroy = wrap(super.onDestroy)(whenDestroy.run)

  def INIT(state: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_qr_request)
    // Snapshot hash here, data will be erased soon
    val targetHash = app.TransData.value match {
      case pr: PaymentRequest => pr.paymentHash
      case _ => ByteVector.empty
    }

    val receivedListener = new ChannelListener {
      override def onSettled(cs: Commitments) = for {
        updateAddHtlc <- cs.localSpec.fulfilledIncoming
        if updateAddHtlc.paymentHash == targetHash
      } showPaid.run
    }

    app.TransData checkAndMaybeErase {
      case pr: PaymentRequest => showInfo(drawAll(denom.asString(pr.amount.get), getString(ln_qr_disposable).html), PaymentRequest.write(pr).toUpperCase)
      case onChainAddress: Address => showInfo(drawBottom(Utils humanSix onChainAddress.toString), onChainAddress.toString)
      case _ => finish
    }

    whenDestroy = UITask(ChannelManager detachListener receivedListener)
    ChannelManager attachListener receivedListener
  } else me exitTo classOf[MainActivity]

  def showPaid = UITask {
    TransitionManager beginDelayedTransition reqContainer
    reqFulfilled setVisibility View.VISIBLE
    reqOptions setVisibility View.GONE
  }

  def showInfo(renderBitmap: Bitmap => Bitmap, data: String) = {
    <(QRGen.get(data, qrSize), onFail)(renderBitmap andThen setView)
    reqCode setOnClickListener onButtonTap(app setBuffer data)
    shareText setOnClickListener onButtonTap(me share data)
  }

  def setView(image: Bitmap) = {
    def share = <(me shareBitmap image, onFail)(none)
    shareQR setOnClickListener onButtonTap(share)
    reqCode setImageBitmap image
    shareQR setEnabled true
  }

  // Low level draw utilites
  def drawAll(top: CharSequence, bot: CharSequence)(qrBitmap: Bitmap) = {
    val bitmap = createBitmap(qrSize, topSize + qrSize + bottomSize, ARGB_8888)
    val ypos = topSize + qrSize + bottomSize / 2
    val canvas = new Canvas(bitmap)
    val transRect = new Rect

    canvas drawColor 0xFFEEEEEE
    transRect.set(0, topSize, qrSize, topSize + qrSize)
    canvas.drawBitmap(qrBitmap, null, transRect, null)
    text(canvas, top, qrSize / 2, topSize / 2)
    text(canvas, bot, qrSize / 2, ypos)
    bitmap
  }

  def drawBottom(bot: CharSequence)(qrBitmap: Bitmap) = {
    val bitmap = createBitmap(qrSize, qrSize + bottomSize, ARGB_8888)
    val canvas = new Canvas(bitmap)
    val transRect = new Rect

    canvas drawColor 0xFFEEEEEE
    transRect.set(0, 0, qrSize, qrSize)
    canvas.drawBitmap(qrBitmap, null, transRect, null)
    text(canvas, bot, qrSize / 2, qrSize + bottomSize / 2)
    bitmap
  }

  def text(canvas: Canvas, text: CharSequence, x: Float, baseY: Float) = {
    val layout = new StaticLayout(text, paint, textBounds, ALIGN_NORMAL, 1f, 0f, false)
    val y = baseY - layout.getHeight / 2f

    canvas.save
    canvas.translate(x, y)
    layout draw canvas
    canvas.restore
  }

  def paint = {
    val newPaint = new TextPaint(Paint.ANTI_ALIAS_FLAG)
    newPaint setTextSize getResources.getDimensionPixelSize(R.dimen.text_small)
    newPaint setTypeface Typeface.create("Droid Sans", Typeface.NORMAL)
    newPaint setTextAlign Paint.Align.CENTER
    newPaint setStyle Paint.Style.FILL
    newPaint setColor Color.BLACK
    newPaint
  }

  def shareBitmap(bitmap: Bitmap) = {
    val paymentRequestFilePath = new File(getCacheDir, "images")
    if (!paymentRequestFilePath.isFile) paymentRequestFilePath.mkdirs
    val out = new FileOutputStream(s"$paymentRequestFilePath/qr.png")
    bitmap.compress(Bitmap.CompressFormat.PNG, 90, out)
    out.close

    val savedFile = new File(paymentRequestFilePath, "qr.png")
    val fileURI = FileProvider.getUriForFile(me, "com.lightning.wallet", savedFile)
    val share = new Intent setAction Intent.ACTION_SEND addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
    share.putExtra(Intent.EXTRA_STREAM, fileURI).setDataAndType(fileURI, getContentResolver getType fileURI)
    me startActivity Intent.createChooser(share, "Choose an app")
  }
}