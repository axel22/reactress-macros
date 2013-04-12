package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import java.lang.ref.{WeakReference => WeakRef}



trait Mux4[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q, @spec(Int, Long, Double) R, @spec(Boolean, Byte, Int, Long, Double) S] extends Serializable {

  def dispatch(source: Source, mp: P, mq: Q, mr: R, ms: S): Unit

  def add(mux: Mux4[Source, P, Q, R, S]): Mux4[Source, P, Q, R, S]

  def remove(mux: Mux4[Source, P, Q, R, S]): Mux4[Source, P, Q, R, S]

}


object Mux4 {

  def None[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q, @spec(Int, Long, Double) R, @spec(Boolean, Byte, Int, Long, Double) S] = NoneImpl.asInstanceOf[Mux4[Source, P, Q, R, S]]

  private case object NoneImpl extends Mux4[Reactive, Any, Any, Any, Any] {
    def dispatch(source: Reactive, mp: Any, mq: Any, mr: Any, ms: Any) {}
    def add(mux: Mux4[Reactive, Any, Any, Any, Any]) = new Composite(Array(new WeakRef(mux)))
    def remove(mux: Mux4[Reactive, Any, Any, Any, Any]) = this
  }

  class Composite[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q, @spec(Int, Long, Double) R, @spec(Boolean, Byte, Int, Long, Double) S](var xs: Array[WeakRef[Mux4[Source, P, Q, R, S]]]) extends Mux4[Source, P, Q, R, S] {
    def dispatch(source: Source, mp: P, mq: Q, mr: R, ms: S) {
      var i = 0
      while (i < xs.length) {
        val ref = xs(i).get
        if (ref ne null) ref.dispatch(source, mp, mq, mr, ms)
        i += 1
      }
    }
    def add(mux: Mux4[Source, P, Q, R, S]) = {
      xs = xs :+ new WeakRef(mux)
      this
    }
    def remove(mux: Mux4[Source, P, Q, R, S]) = {
      xs = xs filter {
        x => x.get != null && x.get != mux
      }
      if (xs.size == 0) None else this
    }
  }

}



