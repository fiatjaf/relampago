package com.lightning.wallet.test

import com.lightning.wallet.helper.ThrottledWork
import com.lightning.wallet.lnutils.JsonHttpUtils._


class ThrottledWorkSpec {
  def allTests = {

    val worker = new ThrottledWork[String, String] {
      def work(input: String) = obsOnIO.map { _ =>
        Thread.sleep(5000)
        input * 2
      }

      def process(result: String) = println(result)
      def error(err: Throwable) = err.printStackTrace
    }

    worker.addWork("t")
    worker.addWork("te")
    worker.addWork("tes")
    worker.addWork("test")
    Thread.sleep(6000)
    worker.addWork("n")
    worker.addWork("ne")
    worker.addWork("new")
  }
}
