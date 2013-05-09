package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Signal[@spec(Boolean, Char, Int, Long, Double) T](
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

  abstract class M0[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X](v: X, s: Reactive) extends Signal(v, s) with Mux0.Sink[Source]
  abstract class M1[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Boolean, Char, Int, Long, Double) T](v: X, s: Reactive) extends Signal(v, s) with Mux1.Sink[Source, T]
  abstract class M2[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q](v: X, s: Reactive) extends Signal(v, s) with Mux2.Sink[Source, P, Q]
  abstract class M3[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Boolean, Int, Long, Double) R](v: X, s: Reactive) extends Signal(v, s) with Mux3.Sink[Source, P, Q, R]
  abstract class M4[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Int) P, @spec(Int) Q, @spec(Int, Long, Double) R, @spec(Int, Long, Double) S](v: X, s: Reactive) extends Signal(v, s) with Mux4.Sink[Source, P, Q, R, S]

}