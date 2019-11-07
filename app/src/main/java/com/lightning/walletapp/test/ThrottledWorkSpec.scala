package com.lightning.walletapp.test

import com.lightning.walletapp.helper.ThrottledWork
import com.lightning.walletapp.lnutils.JsonHttpUtils._


class ThrottledWorkSpec {
  def allTests = {

    val addWorker = new ThrottledWork[String, String] {
      def work(input: String) = queue.map { _ =>
        Thread.sleep(5000)
        input
      }

      def process(ask: String, result: String) = println(result)
      def error(err: Throwable) = err.printStackTrace
    }

    addWorker.addWork("-- t")
    addWorker.addWork("-- te")
    addWorker.addWork("-- tes")
    addWorker.addWork("-- test")

    val replaceWorker = new ThrottledWork[String, String] {
      def work(input: String) = queue.map { _ =>
        Thread.sleep(5000)
        input
      }

      def process(ask: String, result: String) = println(result)
      def error(err: Throwable) = err.printStackTrace
    }

    replaceWorker.replaceWork("-- t1")
    Thread.sleep(1000)
    replaceWorker.replaceWork("-- te1")
    Thread.sleep(2000)
    replaceWorker.replaceWork("-- tes1")
    Thread.sleep(3000)
    replaceWorker.replaceWork("-- test1")
  }
}
