package doodle
package event

import java.util.concurrent.CountDownLatch

final class Cancellable[A](onCancel: () => Unit) {
  private var value: A = _
  private var latch = new CountDownLatch(1)

  def await: A = {
    latch.await()
    value
  }
  def cancel: Unit = onCancel()

  def available(value: A): Unit = {
    this.value = value
    latch.countDown()
  }
}
