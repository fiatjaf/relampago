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
import scodec.bits.ByteVector
import android.view.View
import android.os.Bundle

import android.widget.{ImageButton, ImageView, LinearLayout}
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import com.lightning.walletapp.ln.Tools.{none, wrap}
import android.text.{StaticLayout, TextPaint}
import android.content.{ClipData, Intent}
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
  lazy val reqCode = findViewById(R.id.reqCode).asInstanceOf[ImageView]
  lazy val share = findViewById(R.id.share).asInstanceOf[ImageButton]

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
      case pr: PaymentRequest => showInfo(drawAll(denom.asString(pr.amount.get), getString(ln_qr_disposable).html), PaymentRequest write pr)
      case onChainAddress: Address => showInfo(drawBottom(Utils humanSix onChainAddress.toString), onChainAddress.toString)
      case _ => finish
    }

    whenDestroy = UITask(ChannelManager detachListener receivedListener)
    ChannelManager attachListener receivedListener
  } else me exitTo classOf[MainActivity]

  def showPaid = UITask {
    TransitionManager beginDelayedTransition reqContainer
    reqCode  setOnClickListener onButtonTap(none)
    reqFulfilled setVisibility View.VISIBLE
    share setVisibility View.GONE
  }

  def showInfo(render: Bitmap => Bitmap, bech32: String) =
    <(QRGen.get(bech32.toUpperCase, qrSize), onFail) { bitmap =>

      val finalBitmap = render(bitmap)
      share setOnClickListener onButtonTap {
        <(shareData(finalBitmap, bech32), onFail)(none)
      }

      reqCode setImageBitmap finalBitmap
      reqCode setOnClickListener onButtonTap {
        val bufferContent = ClipData.newPlainText("wallet", bech32)
        app.clipboardManager.setPrimaryClip(bufferContent)
        app quickToast copied_to_clipboard
      }
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

  def shareData(bitmap: Bitmap, bech32: String) = {
    val paymentRequestFilePath = new File(getCacheDir, "images")
    if (!paymentRequestFilePath.isFile) paymentRequestFilePath.mkdirs
    val out = new FileOutputStream(s"$paymentRequestFilePath/qr.png")
    bitmap.compress(Bitmap.CompressFormat.PNG, 85, out)
    out.close

    val savedFile = new File(paymentRequestFilePath, "qr.png")
    val fileURI = FileProvider.getUriForFile(me, "com.lightning.wallet", savedFile)
    val share = new Intent setAction Intent.ACTION_SEND setType "text/plain" addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
    share.putExtra(Intent.EXTRA_TEXT, bech32).putExtra(Intent.EXTRA_STREAM, fileURI).setDataAndType(fileURI, getContentResolver getType fileURI)
    me startActivity Intent.createChooser(share, "Choose an app")
  }
}