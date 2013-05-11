package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import java.lang.ref.{WeakReference => WeakRef}



trait Mux4[Source <: Reactive, @spec(Int) P, @spec(Int) Q, @spec(Int, Long, Double) R, @spec(Int, Long, Double) S] extends Serializable {

  def dispatchOnDefault(source: Source, mp: P, mq: Q, mr: R, ms: S) = {
    val ctx = Ctx.current()
    dispatch(ctx, source, mp, mq, mr, ms)
    ctx.flush()
  }

  def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R, ms: S): Unit

  def add(mux: Mux4[Source, P, Q, R, S]): Mux4[Source, P, Q, R, S]

  def remove(mux: Mux4[Source, P, Q, R, S]): Mux4[Source, P, Q, R, S]

}


object Mux4 {

  def None[Source <: Reactive, @spec(Int) P, @spec(Int) Q, @spec(Int, Long, Double) R, @spec(Int, Long, Double) S] = NoneImpl.asInstanceOf[Mux4[Source, P, Q, R, S]]

  private case object NoneImpl extends Mux4[Reactive, Any, Any, Any, Any] {
    override def dispatchOnDefault(source: Reactive, mp: Any, mq: Any, mr: Any, ms: Any) {}
    def dispatch(ctx: Ctx, source: Reactive, mp: Any, mq: Any, mr: Any, ms: Any) {}
    def add(mux: Mux4[Reactive, Any, Any, Any, Any]) = new Composite().add(mux)
    def remove(mux: Mux4[Reactive, Any, Any, Any, Any]) = this
  }

  class Composite[Source <: Reactive, @spec(Int) P, @spec(Int) Q, @spec(Int, Long, Double) R, @spec(Int, Long, Double) S]
  extends MuxHashTable[Mux4[Source, P, Q, R, S]] with Mux4[Source, P, Q, R, S] {
    def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R, ms: S) {
      var i = 0
      while (i < table.length) {
        val weakref = table(i)
        if (weakref ne null) {
          val ref = weakref.get
          if (ref ne null) ref.dispatch(ctx, source, mp, mq, mr, ms)
        }
        i += 1
      }
    }

    def add(recv: Mux4[Source, P, Q, R, S]) = {
      addEntry(recv)
      this
    }

    def remove(recv: Mux4[Source, P, Q, R, S]) = {
      removeEntry(recv)
      if (size > 0) this else None
    }
  }

  trait Sink[Source <: Reactive, P, Q, R, S] extends Mux4[Source, P, Q, R, S] {
    def add(recv: Mux4[Source, P, Q, R, S]) = ???
    def remove(recv: Mux4[Source, P, Q, R, S]) = ???
  }

}



