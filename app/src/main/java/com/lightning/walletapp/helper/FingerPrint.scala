package com.lightning.walletapp.helper

import com.lightning.walletapp.AbstractKit
import com.lightning.walletapp.Utils.app
import co.infinum.goldfinger.Goldfinger


object FingerPrint {
  def isEnabled = app.prefs.getBoolean(AbstractKit.FINGER_ENABLED, false)
  def switch(mode: Boolean) = app.prefs.edit.putBoolean(AbstractKit.FINGER_ENABLED, mode).commit
  def isOperational(goldfinger: Goldfinger) = isEnabled && goldfinger.hasEnrolledFingerprint
}