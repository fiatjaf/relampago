package com.lightning.walletapp.helper

import rx.lang.scala.Subscription
import rx.lang.scala.{Observable => Obs}


abstract class ThrottledWork[T, V] {
  private var lastWork: Option[T] = None
  private var subscription: Subscription = _

  def work(input: T): Obs[V]
  def error(err: Throwable): Unit
  def process(ask: T, result: V): Unit

  def addWork(data: T): Unit =
    if (subscription == null) {
      subscription = work(input = data)
        .doOnSubscribe { lastWork = None }
        .doOnTerminate { subscription = null }
        .doAfterTerminate { lastWork foreach addWork }
        .subscribe(res => process(data, res), error)
    } else {
      // Current work has not finished yet
      // schedule new work once this one is done
      lastWork = Some(data)
    }

  def replaceWork(data: T): Unit =
    if (subscription == null) {
      subscription = work(input = data)
        .doOnTerminate { subscription = null }
        .subscribe(res => process(data, res), error)
    } else {
      // Current work has not finished yet
      // disconnect subscription and replace
      subscription.unsubscribe
      subscription = null
      replaceWork(data)
    }
}