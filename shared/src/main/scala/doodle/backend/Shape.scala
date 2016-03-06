package doodle.backend

import doodle.core.Line.{Cap, Join}
import doodle.core.{Stroke, Color}

/**
  * This class is the base trait for all the shapes
  *
  * @author Gayathri Thiyagarajan
  */
trait Shape {
  def beside(shape:Shape) = {
    ???
  }
  def on(shape:Shape) = {
    ???
  }
  def above(shape:Shape) = {
    ???
  }
  def below(shape:Shape) = {
    ???
  }

  def draw(canvas:Canvas):Unit = {
    this match {
      case Circle(r) => {
        canvas.setFill(Color.red)
        canvas.setSize(1000, 1000)
        canvas.circle(100, 100, r)
        canvas.fill()
      }
      case Rectangle(w, h) =>  {
        canvas.setFill(Color.blue)
        canvas.setSize(1000, 1000)
        canvas.rectangle(200, 200, w, h)
        canvas.fill()
      }
    }
  }
}

final case class Circle(radius:Double) extends Shape

final case class Rectangle(width:Double, height:Double) extends Shape
