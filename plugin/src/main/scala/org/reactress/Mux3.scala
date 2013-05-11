package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import java.lang.ref.{WeakReference => WeakRef}



trait Mux3[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Boolean, Int, Long, Double) R] extends Serializable {

  def dispatchOnDefault(source: Source, mp: P, mq: Q, mr: R) = {
    val ctx = Ctx.current()
    dispatch(ctx, source, mp, mq, mr)
    ctx.flush()
  }

  def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R): Unit

  def add(mux: Mux3[Source, P, Q, R]): Mux3[Source, P, Q, R]

  def remove(mux: Mux3[Source, P, Q, R]): Mux3[Source, P, Q, R]

}


object Mux3 {

  def None[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Boolean, Int, Long, Double) R] = NoneImpl.asInstanceOf[Mux3[Source, P, Q, R]]

  private case object NoneImpl extends Mux3[Reactive, Any, Any, Any] {
    override def dispatchOnDefault(source: Reactive, mp: Any, mq: Any, mr: Any) {}
    def dispatch(ctx: Ctx, source: Reactive, mp: Any, mq: Any, mr: Any) {}
    def add(mux: Mux3[Reactive, Any, Any, Any]) = new Composite().add(mux)
    def remove(mux: Mux3[Reactive, Any, Any, Any]) = this
  }

  class Composite[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Boolean, Int, Long, Double) R]
  extends MuxHashTable[Mux3[Source, P, Q, R]] with Mux3[Source, P, Q, R] {
    def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R) {
      var i = 0
      while (i < table.length) {
        val weakref = table(i)
        if (weakref ne null) {
          val ref = weakref.get
          if (ref ne null) ref.dispatch(ctx, source, mp, mq, mr)
        }
        i += 1
      }
    }

    def add(recv: Mux3[Source, P, Q, R]) = {
      addEntry(recv)
      this
    }

    def remove(recv: Mux3[Source, P, Q, R]) = {
      removeEntry(recv)
      if (size > 0) this else None
    }
  }

  trait Sink[Source <: Reactive, P, Q, R] extends Mux3[Source, P, Q, R] {
    def add(recv: Mux3[Source, P, Q, R]) = ???
    def remove(recv: Mux3[Source, P, Q, R]) = ???
  }

}



