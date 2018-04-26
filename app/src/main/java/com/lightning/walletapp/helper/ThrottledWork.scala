package com.lightning.walletapp.helper


abstract class ThrottledWork[T, V] {
  import rx.lang.scala.{Observable => Obs}
  private var lastWork: Option[T] = None
  private var isWorking: Boolean = false

  def work(input: T): Obs[V]
  def error(err: Throwable): Unit
  def process(ask: T, result: V): Unit

  private def doWork(workInput: T) =
    work(workInput).doAfterTerminate { lastWork foreach addWork }
    .doOnTerminate { isWorking = false }.doOnSubscribe { isWorking = true }
      .doOnSubscribe { lastWork = None }.subscribe(process(workInput, _), error)

  def addWork(data: T): Unit =
    // Postpone next work if not done yet
    if (isWorking) lastWork = Some(data)
    else doWork(data)
}