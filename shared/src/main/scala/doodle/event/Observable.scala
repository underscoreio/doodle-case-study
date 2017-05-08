package doodle
package event

import java.util.concurrent._
import scala.concurrent.ExecutionContext

final class Observable[A](source: Observable.Internal[A], available: Semaphore) {
  import Observation._

  var cancelled: Boolean = false

  def runFold[B](zero: B)(f: (B,A) => B)(implicit ec: ExecutionContext): Cancellable[B] = {
    def loop(result: B): B = {
      available.acquire()
      if(!cancelled) {
        Observable.Internal.next(source) match {
          case Value(a) => loop(f(result, a))
          case Await => loop(result)
          case Complete => result
        }
      } else result
    }

    val onCancel = () => this.cancel()
    val cancellable = new Cancellable[B](onCancel)
    val runnable = new Runnable {
      def run(): Unit = {
        cancellable.available(loop(zero))
      }
    }

    ec.execute(runnable)
    cancellable
  }

  def cancel(): Unit = {
    cancelled = true
    // Put a token into the semaphore so runFold will be woken.
    available.release()
  }
}
object Observable {
  def fromStream[A](source: Stream[A]): Observable[A] = {
    import Internal._

    // Semaphore that has permits available when a source has received input
    // We use this to prevent busy waiting, which consumes a lot of CPU, in `runFold`.
    val inputAvailable = new Semaphore(0)

    def compile[A](source: Stream[A]): Internal[A] =
      source match {
        case Stream.Map(source, f) =>
          Map(compile(source), f)
        case Stream.FlatMap(source, f) =>
          // Type inference failure infers Any where it should infer A
          FlatMap(compile(source), (a: Any) => compile(f(a)))
        case Stream.ScanLeft(source, zero, f) =>
          ScanLeft(compile(source), zero, f)
        case Stream.Join(left, right) =>
          Join(compile(left), compile(right))
        case Stream.FromCallbackHandler(handler) =>
          FromCallbackHandler(handler, inputAvailable)
        case Stream.FromIterator(source) =>
          if(source.hasNext) inputAvailable.release()
          FromIterator(source, inputAvailable)
      }

    new Observable(compile(source), inputAvailable)
  }

  sealed trait Internal[A]
  object Internal {
    import Observation._

    /** Next will block indefinitely for a value. */
    def next[A](internal: Internal[A]): Observation[A] =
      internal match {
        case FromIterator(source, available) =>
          if(source.hasNext) {
            val a = source.next()
            if(source.hasNext) available.release()
            Observation.value(a)
          } else Observation.complete
        case h @ FromCallbackHandler(handler, _) =>
          Value(h.queue.take())
        case Map(source, f) =>
          next(source).map(f)
        case FlatMap(source, f) =>
          next(source).flatMap(a => next(f(a)))
        case Zip(left, right) =>
          next(left).zip(next(right))
        case s @ ScanLeft(source, z, f) =>
          next(source) match {
            case Value(a) =>
              s.zero = f(z, a)
              Observation.value(s.zero)
            case Await => Observation.await
            case Complete => Observation.complete
          }
        case j @ Join(l, r) =>
          j.leftValue = poll(l).valueOr(j.leftValue)
          j.rightValue = poll(r).valueOr(j.rightValue)
          j.leftValue.zip(j.rightValue)
      }

    /** Poll will return a value if one is immediately available. It will not block waiting. */
    def poll[A](internal: Internal[A]): Observation[A] =
      internal match {
        case FromIterator(source, available) =>
          if(source.hasNext) {
            val a = source.next()
            if(source.hasNext) available.release()
            Observation.value(a)
          } else Observation.complete
        case h @ FromCallbackHandler(handler, _) =>
          val result = h.queue.poll()
          if(result == null) Await else Value(result)
        case Map(source, f) =>
          poll(source).map(f)
        case FlatMap(source, f) =>
          poll(source).flatMap(a => poll(f(a)))
        case Zip(left, right) =>
          poll(left).zip(poll(right))
        case s @ ScanLeft(source, z, f) =>
          poll(source )match {
            case Value(a) =>
              s.zero = f(z, a)
              Observation.value(s.zero)
            case Await => Observation.await
            case Complete => Observation.complete
          }
        case j @ Join(l, r) =>
          j.leftValue = poll(l).valueOr(j.leftValue)
          j.rightValue = poll(r).valueOr(j.rightValue)
          j.leftValue.zip(j.rightValue)
      }

    final case class Map[A,B](source: Internal[A], f: A => B) extends Internal[B]
    final case class FlatMap[A,B](source: Internal[A], f: A => Internal[B]) extends Internal[B]
    final case class ScanLeft[A,B](source: Internal[A], var zero: B, f: (B,A) => B) extends Internal[B]
    final case class Zip[A,B](left: Internal[A], right: Internal[B]) extends Internal[(A,B)]
    final case class Join[A,B](left: Internal[A], right: Internal[B]) extends Internal[(A,B)] {
      var leftValue:  Observation[A] = Await
      var rightValue: Observation[B] = Await
    }
    final case class FromIterator[A](source: Iterator[A], available: Semaphore) extends Internal[A]
    final case class FromCallbackHandler[A](handler: (A => Unit) => Unit, available: Semaphore) extends Internal[A] {
      // In a production ready implement we'd provide better ways to queue input.
      // We might want, for example, a larger queue if we expect bursty traffic.
      val queue = new ArrayBlockingQueue[A](1)

      handler { a =>
        queue.put(a)
        available.release()
      }
    }
  }
}
