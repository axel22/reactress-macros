package org.reactress
package container



import scala.reflect.ClassTag



class ReactTileMap[@spec(Byte, Int, Long, Double) T](
  w0: Int,
  h0: Int,
  default0: T
) extends Reactive.Source[Mux3[Int, Int, T]] {
  private var wdt = w0
  private var hgt = h0
  private var dflt = default0

  def width = wdt
  def height = hgt
  def default = dflt

  def apply(x: Int, y: Int) = null
  def update(x: Int, y: Int) = null
  def clear() = null

}


object ReactTileMap {
}



