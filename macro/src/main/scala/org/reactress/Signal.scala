package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Signal[@spec(Boolean, Char, Int, Long, Double) T](
  @react private[reactress] var value: T
) extends Reactive {

  def apply(): T = value

  def detach(): Unit

  def priority: Int

  final override def flush(ctx: Ctx) {}

  override def finalize() {
    super.finalize()
    detach()
  }

}


object Signal {

  abstract class M0[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X](v: X) extends Signal(v) with Mux0.Sink[Source]
  abstract class M1[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Boolean, Char, Int, Long, Double) T](v: X) extends Signal(v) with Mux1.Sink[Source, T]
  abstract class M2[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q](v: X) extends Signal(v) with Mux2.Sink[Source, P, Q]
  abstract class M3[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Boolean, Int, Long, Double) R](v: X) extends Signal(v) with Mux3.Sink[Source, P, Q, R]
  abstract class M4[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Int) P, @spec(Int) Q, @spec(Int, Long, Double) R, @spec(Int, Long, Double) S](v: X) extends Signal(v) with Mux4.Sink[Source, P, Q, R, S]

}