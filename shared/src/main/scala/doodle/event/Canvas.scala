package doodle
package event

import scala.concurrent.ExecutionContext
import doodle.core.{Color, Image}

object Canvas {
  def animationFrameStream(canvas: backend.Canvas): Stream[Double] = {
    Stream.fromCallbackHandler(canvas.setAnimationFrameCallback)
  }

  def keyDownStream(canvas: backend.Canvas): Stream[backend.Key] = {
    Stream.fromCallbackHandler(canvas.setKeyDownCallback)
  }

  def animate(canvas: backend.Canvas, frames: Stream[Image])(implicit ec: ExecutionContext) =
    frames.run(frame => {
                 canvas.clear(Color.black)
                 frame.draw(canvas)
               })
}
