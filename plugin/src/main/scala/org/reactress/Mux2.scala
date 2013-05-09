package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import java.lang.ref.{WeakReference => WeakRef}



trait Mux2[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] extends Serializable {

  def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q): Unit

  def add(mux: Mux2[Source, P, Q]): Mux2[Source, P, Q]

  def remove(mux: Mux2[Source, P, Q]): Mux2[Source, P, Q]

}


object Mux2 {

  def None[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] = NoneImpl.asInstanceOf[Mux2[Source, P, Q]]

  private case object NoneImpl extends Mux2[Reactive, Any, Any] {
    def dispatch(ctx: Ctx, source: Reactive, mp: Any, mq: Any) {}
    def add(mux: Mux2[Reactive, Any, Any]) = new Composite().add(this)
    def remove(mux: Mux2[Reactive, Any, Any]) = this
  }

  class Composite[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q]
  extends MuxHashTable[Mux2[Source, P, Q]] with Mux2[Source, P, Q] {
    def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q) {
      var i = 0
      while (i < table.length) {
        val weakref = table(i)
        if (weakref ne null) {
          val ref = weakref.get
          if (ref ne null) ref.dispatch(ctx, source, mp, mq)
        }
        i += 1
      }
    }

    def add(recv: Mux2[Source, P, Q]) = {
      addEntry(recv)
      this
    }

    def remove(recv: Mux2[Source, P, Q]) = {
      removeEntry(recv)
      if (size > 0) this else None
    }
  }

  trait Sink[Source <: Reactive, P, Q] extends Mux2[Source, P, Q] {
    def add(recv: Mux2[Source, P, Q]) = ???
    def remove(recv: Mux2[Source, P, Q]) = ???
  }

}



