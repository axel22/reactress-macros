package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import java.lang.ref.{WeakReference => WeakRef}



trait Mux1[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) T] extends Serializable {

  def dispatch(ctx: Ctx, source: Source, msg: T): Unit

  def add(mux: Mux1[Source, T]): Mux1[Source, T]

  def remove(mux: Mux1[Source, T]): Mux1[Source, T]

}


object Mux1 {

  def None[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) T] = NoneImpl.asInstanceOf[Mux1[Source, T]]

  private case object NoneImpl extends Mux1[Reactive, Any] {
    def dispatch(ctx: Ctx, source: Reactive, msg: Any) {}
    def add(mux: Mux1[Reactive, Any]) = new Composite().add(this)
    def remove(mux: Mux1[Reactive, Any]) = this
  }

  class Composite[Source <: Reactive, @spec(Boolean, Char, Int, Long, Double) T]
  extends MuxHashTable[Mux1[Source, T]] with Mux1[Source, T] {
    def dispatch(ctx: Ctx, source: Source, msg: T) {
      var i = 0
      while (i < table.length) {
        val weakref = table(i)
        if (weakref ne null) {
          val ref = weakref.get
          if (ref ne null) ref.dispatch(ctx, source, msg)
        }
        i += 1
      }
    }

    def add(recv: Mux1[Source, T]) = {
      addEntry(recv)
      this
    }

    def remove(recv: Mux1[Source, T]) = {
      removeEntry(recv)
      if (size > 0) this else None
    }
  }

  trait Sink[Source <: Reactive, T] extends Mux1[Source, T] {
    def add(recv: Mux1[Source, T]) = ???
    def remove(recv: Mux1[Source, T]) = ???
  }

}



