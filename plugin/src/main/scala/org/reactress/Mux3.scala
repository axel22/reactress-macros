package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import java.lang.ref.{WeakReference => WeakRef}



trait Mux3[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Boolean, Int, Long, Double) R] extends Serializable {

  def dispatch(source: Source, mp: P, mq: Q, mr: R): Unit

  def add(mux: Mux3[Source, P, Q, R]): Mux3[Source, P, Q, R]

  def remove(mux: Mux3[Source, P, Q, R]): Mux3[Source, P, Q, R]

}


object Mux3 {

  def None[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Boolean, Int, Long, Double) R] = NoneImpl.asInstanceOf[Mux3[Source, P, Q, R]]

  private case object NoneImpl extends Mux3[Reactive, Any, Any, Any] {
    def dispatch(source: Reactive, mp: Any, mq: Any, mr: Any) {}
    def add(mux: Mux3[Reactive, Any, Any, Any]) = new Composite(Array(new WeakRef(mux)))
    def remove(mux: Mux3[Reactive, Any, Any, Any]) = this
  }

  class Composite[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Boolean, Int, Long, Double) R](var ms: Array[WeakRef[Mux3[Source, P, Q, R]]]) extends Mux3[Source, P, Q, R] {
    def dispatch(source: Source, mp: P, mq: Q, mr: R) {
      var i = 0
      while (i < ms.length) {
        val ref = ms(i).get
        if (ref ne null) ref.dispatch(source, mp, mq, mr)
        i += 1
      }
    }
    def add(mux: Mux3[Source, P, Q, R]) = {
      ms = ms :+ new WeakRef(mux)
      this
    }
    def remove(mux: Mux3[Source, P, Q, R]) = {
      ms = ms filter {
        x => x.get != null && x.get != mux
      }
      if (ms.size == 0) None else this
    }
  }

  trait Sink[Source <: Reactive, P, Q, R] extends Mux3[Source, P, Q, R] {
    def add(recv: Mux3[Source, P, Q, R]) = ???
    def remove(recv: Mux3[Source, P, Q, R]) = ???
  }

}



