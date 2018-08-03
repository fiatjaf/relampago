package com.lightning.walletapp

import android.graphics._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import android.text.Layout.Alignment.ALIGN_NORMAL
import android.graphics.Bitmap.Config.ARGB_8888
import com.google.zxing.qrcode.QRCodeWriter
import android.graphics.Bitmap.createBitmap
import fr.acinq.bitcoin.BinaryData
import org.bitcoinj.core.Address
import android.content.Intent
import android.view.View
import android.net.Uri

import android.widget.{ImageButton, ImageView, LinearLayout}
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import com.lightning.walletapp.ln.Tools.{wrap, none}
import android.text.{StaticLayout, TextPaint}
import java.io.{File, FileOutputStream}
import android.os.{Bundle, Environment}


object QRGen {
  val writer = new QRCodeWriter
  val hints = new java.util.Hashtable[EncodeHintType, Any]
  hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.M)
  hints.put(EncodeHintType.MARGIN, 1)

  def get(txt: String, size: Int): Bitmap = {
    val bitMatrix = writer.encode(txt, BarcodeFormat.QR_CODE, size, size, hints)
    val (wid, height) = (bitMatrix.getWidth, bitMatrix.getHeight)
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

object FileOps {
  def shell(name: String) = {
    val path = Environment.getExternalStorageDirectory
    val dir = new File(path.getAbsolutePath)
    if (!dir.exists) dir.mkdirs
    new File(dir, name)
  }
}

class RequestActivity extends TimerActivity { me =>
  lazy val reqOptions = findViewById(R.id.reqOptions).asInstanceOf[LinearLayout]
  lazy val reqFulfilled = findViewById(R.id.reqFulfilled).asInstanceOf[LinearLayout]
  lazy val shareText = findViewById(R.id.shareText).asInstanceOf[ImageButton]
  lazy val shareQR = findViewById(R.id.shareQR).asInstanceOf[ImageButton]
  lazy val reqCode = findViewById(R.id.reqCode).asInstanceOf[ImageView]

  lazy val textBounds = getResources getDimensionPixelSize R.dimen.bitmap_text_bounds
  lazy val bottomSize = getResources getDimensionPixelSize R.dimen.bitmap_bottom_size
  lazy val topSize = getResources getDimensionPixelSize R.dimen.bitmap_top_size
  lazy val qrSize = getResources getDimensionPixelSize R.dimen.bitmap_qr_size
  lazy val disposable = getString(ln_qr_disposable)

  var whenDestroy: Runnable = new Runnable { def run = none }
  override def onDestroy = wrap(super.onDestroy)(whenDestroy.run)

  def INIT(state: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_qr_request)
    val targetPayHash = app.TransData.value match {
      case request: PaymentRequest => request.paymentHash
      case _ => BinaryData.empty
    }

    val receivedListener = new ChannelListener {
      override def settled(cs: Commitments) = for {
        Htlc(true, add) \ _ <- cs.localCommit.spec.fulfilled
        if add.paymentHash == targetPayHash
      } showPaid.run
    }

    app.TransData checkAndMaybeErase {
      case pr: PaymentRequest => showInfo(drawAll(denom asString pr.amount.get, disposable.html), PaymentRequest write pr)
      case onChainAddress: Address => showInfo(drawBottom(Utils humanSix onChainAddress.toString), onChainAddress.toString)
      case _ => finish
    }

    whenDestroy = UITask { for (c <- app.ChannelManager.all) c.listeners -= receivedListener }
    for (c <- app.ChannelManager.all) c.listeners += receivedListener
  } else me exitTo classOf[MainActivity]

  def showPaid = UITask {
    reqOptions setVisibility View.GONE
    reqFulfilled setVisibility View.VISIBLE
  }

  def showInfo(renderBitmap: Bitmap => Bitmap, data: String) = {
    <(QRGen.get(data, qrSize), onFail)(renderBitmap andThen setView)
    reqCode setOnClickListener onButtonTap(app setBuffer data)
    shareText setOnClickListener onButtonTap(me share data)
  }

  def setView(displayedImage: Bitmap) = {
    shareQR setOnClickListener onButtonTap {
      <(me saveImage displayedImage, onFail) { file =>
        val share = new Intent setAction Intent.ACTION_SEND setType "image/png"
        me startActivity share.putExtra(Intent.EXTRA_STREAM, Uri fromFile file)
      }
    }

    // Enable after QR is fully generated
    reqCode setImageBitmap displayedImage
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

  def saveImage(bits: Bitmap) = {
    val imageFile = FileOps shell "qr.png"
    val stream = new FileOutputStream(imageFile)
    bits.compress(Bitmap.CompressFormat.PNG, 80, stream)
    stream.flush
    stream.close
    imageFile
  }
}