package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Signal[@spec T](v: T, src: Reactive) extends Reactive {
  private[reactress] val source: Reactive = src
  private[reactress] var value = v

  def apply(): T = value

  private[reactress] def detach(): Unit

  override def finalize() {
    super.finalize()
    detach()
  }

}


object Signal {
  
}