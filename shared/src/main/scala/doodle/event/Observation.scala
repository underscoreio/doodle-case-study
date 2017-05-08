package doodle
package event

sealed trait Observation[+A] {
  import Observation._

  def flatMap[B](f: A => Observation[B]): Observation[B] =
    this match {
      case Value(a) => f(a)
      case Await => Await
      case Complete => Complete
    }

  def map[B](f: A => B): Observation[B] =
    this match {
      case Value(a) => Value(f(a))
      case Await => Await
      case Complete => Complete
    }

  def zip[B](that: Observation[B]): Observation[(A,B)] =
    this match {
      case Value(a) =>
        that match {
          case Value(b) => Value((a, b))
          case Await => Await
          case Complete => Complete
        }
      // If that is Complete the result is Complete
      case Await =>
        that match {
          case Value(b) => Await
          case Await => Await
          case Complete => Complete
        }
      case Complete => Complete
    }

  def foreach(f: A => Unit): Unit =
    this match {
      case Value(a) => f(a)
      case Await => ()
      case Complete => ()
    }

  def getOrElse[AA >: A](other: AA): AA =
    this match {
      case Value(a) => a
      case Await => other
      case Complete => other
    }

  def valueOr[AA >: A](other: Observation[AA]): Observation[AA] =
    this match {
      case Value(a) => this
      case Await => other
      case Complete => other
    }
}
object Observation {
  def value[A](a: A): Observation[A] = Value(a)
  val await: Observation[Nothing] = Await
  val complete: Observation[Nothing] = Complete

  final case class Value[A](get: A) extends Observation[A]
  final case object Await extends Observation[Nothing]
  final case object Complete extends Observation[Nothing]
}
