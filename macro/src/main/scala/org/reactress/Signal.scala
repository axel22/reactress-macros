package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Signal[@spec T](
  @react private[reactress] var value: T,
  private[reactress] val source: Reactive
) extends Reactive {

  def apply(): T = value

  def detach(): Unit

  override def finalize() {
    super.finalize()
    detach()
  }

}


object Signal {

}