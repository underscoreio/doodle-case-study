package doodle
package event

import scala.concurrent.ExecutionContext

/**
  * Pull-based cancellable reactive stream system.
  *
  * This is an example of a pull-based (functional) reactive stream system.
  * It accepts input from callbacks or iterators.
  * Streams can be transformed using [[map]], [[flatMap]], [[scanLeft]], and [[join]].
  * Notably nothing happens until the [[Stream]] is run by calling [[runFold]] or [[run]].
  */
sealed trait Stream[A] {
  import Stream._

  def map[B](f: A => B): Stream[B] =
    Map(this, f)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    FlatMap(this, f)

  def scanLeft[B](seed: B)(f: (B,A) => B): Stream[B] =
    ScanLeft(this, seed, f)

  def join[B](that: Stream[B]): Stream[(A,B)] =
    Join(this, that)

  def runFold[B](seed: B)(f: (B,A) => B)(implicit ec: ExecutionContext): Cancellable[B] = {
    Observable.fromStream(this).runFold(seed)(f)
  }

  def run(f: A => Unit)(implicit ec: ExecutionContext): Unit =
    runFold(()){ (_, a) => f(a) }
}
object Stream {
  def fromCallbackHandler[A](handler: (A => Unit) => Unit) =
    FromCallbackHandler(handler)

  def fromIterator[A](source: Iterator[A]): Stream[A] =
    FromIterator(source)

  def always[A](element: A): Stream[A] =
    FromIterator(Iterator.continually(element))

  def apply[A](elements: A*): Stream[A] =
    FromIterator(Iterator(elements: _*))

  // Stream algebraic data type
  final case class Map[A,B](source: Stream[A], f: A => B) extends Stream[B]
  final case class FlatMap[A,B](source: Stream[A], f: A => Stream[B]) extends Stream[B]
  final case class ScanLeft[A,B](source: Stream[A], zero: B, f: (B,A) => B) extends Stream[B]
  final case class Join[A,B](left: Stream[A], right: Stream[B]) extends Stream[(A,B)]
  final case class FromCallbackHandler[A](handler: (A => Unit) => Unit) extends Stream[A]
  final case class FromIterator[A](source: Iterator[A]) extends Stream[A]
}
