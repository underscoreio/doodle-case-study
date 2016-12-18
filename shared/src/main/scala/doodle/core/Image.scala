package doodle.core

import doodle.backend.Canvas

sealed trait Image {

  val colour : Color
  
  val x : Double
  
  val y : Double
  
  def at(coords : Vec) : Image = ???
  
  def boundingBox() : BoundingBox = this match {
    
    case On(t, b, _) => t.boundingBox on b.boundingBox//BoundingBox(left, top, t.boundingBox(left,top).right, b.boundingBox(left,top - t.boundingBox(left, top).bottom).bottom)
    case Beside(l, r, _) => BoundingBox(l.boundingBox.left, l.boundingBox.top, r.boundingBox.right, l.boundingBox.bottom)
    case Above(b, t, _) => BoundingBox(t.boundingBox.left, t.boundingBox.top, t.boundingBox.right, b.boundingBox.bottom)
    case Circle(rad, _) => BoundingBox(-rad, rad, rad,  -rad)
    case Rectangle(w, h, _) => BoundingBox(-w/2, -h/2, w/2, h/2)
    
  }
  
  def on (that : Image) : Image = On(this, that)
  
  def beside (that : Image) : Image = Beside(this, that)
  
  def above (that : Image) : Image = Above(this, that)
  
  def draw (canvas : Canvas, x : Double = 0.0, y : Double = 0.0) : Unit = this match {
    case Circle(rad, color) => {canvas.setFill(color); canvas.circle(x, y, rad); canvas.fill()}
    case Rectangle(w, h, color) => { canvas.setFill(color); canvas.rectangle(-w/2, h/2, w/2, -h/2)
; canvas.fill()}
    case On(hd, tl, _) => { 
      
      tl.draw(canvas, x, y)
      hd.draw(canvas, x, y)
    }
    case Beside(hd, tl, _) =>{ hd.draw(canvas, x, y); tl.draw(canvas, hd.boundingBox.right + (tl.boundingBox.width/2), y); }
    case Above(hd, tl, _) => { tl.draw(canvas, x, y); hd.draw(canvas, x, hd.boundingBox.top + (tl.boundingBox.height/2)); }
    
    
  }
}

case class On(top : Image, bottom : Image, colour : Color = Color.white) extends Image 

case class Beside(left : Image, right : Image, colour : Color = Color.white) extends Image 

case class Above(under : Image, over : Image, colour : Color = Color.white) extends Image 

sealed trait Shape extends Image

case class Circle(radius : Double, colour : Color = Color.red) extends Shape

case class Rectangle(height : Int, width : Int, colour : Color = Color.green) extends Shape
