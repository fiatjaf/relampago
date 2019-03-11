package com.lightning.walletapp.helper

import co.infinum.goldfinger.{Goldfinger, Error => GFError}
import android.content.pm.PackageManager.PERMISSION_GRANTED
import android.Manifest.permission.USE_FINGERPRINT
import android.support.v4.content.ContextCompat
import android.support.v4.app.ActivityCompat
import com.lightning.walletapp.AbstractKit
import com.lightning.walletapp.Utils.app
import android.app.Activity


object FingerPrint {
  private[this] var lastToast = 0L
  def isEnabled = app.prefs.getBoolean(AbstractKit.FINGERPRINT_ENABLED, false)
  def switch(mode: Boolean) = app.prefs.edit.putBoolean(AbstractKit.FINGERPRINT_ENABLED, mode).commit
  def askPermission(host: Activity) = ActivityCompat.requestPermissions(host, Array(USE_FINGERPRINT), 105)
  def isPermissionGranted = ContextCompat.checkSelfPermission(app, USE_FINGERPRINT) == PERMISSION_GRANTED
  def isOperational(gf: Goldfinger) = isEnabled && isPermissionGranted && gf.hasEnrolledFingerprint

  def informUser(err: GFError) =
    if (lastToast < System.currentTimeMillis - 3000L) {
      // Debounce to prevent toast stacking if user is hasty
      lastToast = System.currentTimeMillis
      app toast err.toString
    }
}