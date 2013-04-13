package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import java.lang.ref.{WeakReference => WeakRef}



trait Mux2[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] extends Serializable {

  def dispatch(source: Source, mp: P, mq: Q): Unit

  def add(mux: Mux2[Source, P, Q]): Mux2[Source, P, Q]

  def remove(mux: Mux2[Source, P, Q]): Mux2[Source, P, Q]

}


object Mux2 {

  def None[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] = NoneImpl.asInstanceOf[Mux2[Source, P, Q]]

  private case object NoneImpl extends Mux2[Reactive, Any, Any] {
    def dispatch(source: Reactive, mp: Any, mq: Any) {}
    def add(mux: Mux2[Reactive, Any, Any]) = new Composite(Array(new WeakRef(mux)))
    def remove(mux: Mux2[Reactive, Any, Any]) = this
  }

  class Composite[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q](var ms: Array[WeakRef[Mux2[Source, P, Q]]]) extends Mux2[Source, P, Q] {
    def dispatch(source: Source, mp: P, mq: Q) {
      var i = 0
      while (i < ms.length) {
        val ref = ms(i).get
        if (ref ne null) ref.dispatch(source, mp, mq)
        i += 1
      }
    }
    def add(mux: Mux2[Source, P, Q]) = {
      ms = ms :+ new WeakRef(mux)
      this
    }
    def remove(mux: Mux2[Source, P, Q]) = {
      ms = ms filter {
        x => x.get != null && x.get != mux
      }
      if (ms.size == 0) None else this
    }
  }

  trait Sink[Source <: Reactive, P, Q] extends Mux2[Source, P, Q] {
    def add(recv: Mux2[Source, P, Q]) = ???
    def remove(recv: Mux2[Source, P, Q]) = ???
  }

}



