package doodle.core

final case class BoundingBox(left: Double, top: Double, right: Double, bottom: Double) {
  
  val height : Double = top - bottom
  val width : Double = right - left
  
  def on(other : BoundingBox) : BoundingBox = new BoundingBox(left min other.left, top + other.height, right max other.height, bottom)
  
  def beside(other : BoundingBox) : BoundingBox = new BoundingBox(left, top max other.top, right + other.width, bottom min other.bottom)
  
  def above(other : BoundingBox) : BoundingBox = new BoundingBox(left min other.left, top, right max other.right, bottom - other.height)
  
  
}