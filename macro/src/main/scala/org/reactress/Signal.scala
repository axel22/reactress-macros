package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



trait Signal[@spec(Boolean, Char, Int, Long, Double) T]
extends Reactive {

  var value: T

  var value$mux: Mux0[Signal[T]]

  def asMux[M] = this.asInstanceOf[M]

  def set(v: T, ctx: Ctx) {
    value = v
    value$mux.dispatch(ctx, this)
  }

  def apply(): T = value

  def detach(): Unit

  def priority: Int

  override def finalize() {
    super.finalize()
    detach()
  }

}


object Signal {

  abstract class M0[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X]
  (@react(source[Signal[X]]) var value: X)
  extends Signal[X] with Mux0.Sink[Source]

  abstract class M1[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Boolean, Char, Int, Long, Double) T]
  (@react(source[Signal[X]]) var value: X)
  extends Signal[X] with Mux1.Sink[Source, T]

  abstract class M2[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q]
  (@react(source[Signal[X]]) var value: X)
  extends Signal[X] with Mux2.Sink[Source, P, Q]
  
  abstract class M3[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Boolean, Int, Long, Double) R]
  (@react(source[Signal[X]]) var value: X)
  extends Signal[X] with Mux3.Sink[Source, P, Q, R]
  
  abstract class M4[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) X, @spec(Int) P, @spec(Int) Q, @spec(Int, Long, Double) R, @spec(Int, Long, Double) S]
  (@react(source[Signal[X]]) var value: X)
  extends Signal[X] with Mux4.Sink[Source, P, Q, R, S]

}