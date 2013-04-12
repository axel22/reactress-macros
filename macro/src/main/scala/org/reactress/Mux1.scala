package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import java.lang.ref.{WeakReference => WeakRef}



trait Mux1[Source <: Reactive, @spec(Byte, Char, Int, Long, Double) T] extends Serializable {

  def dispatch(source: Source, msg: T): Unit

  def add(mux: Mux1[Source, T]): Mux1[Source, T]

  def remove(mux: Mux1[Source, T]): Mux1[Source, T]

}


object Mux1 {

  def None[Source <: Reactive, @spec(Byte, Char, Int, Long, Double) T] = NoneImpl.asInstanceOf[Mux1[Source, T]]

  private case object NoneImpl extends Mux1[Reactive, Any] {
    def dispatch(source: Reactive, msg: Any) {}
    def add(mux: Mux1[Reactive, Any]) = new Composite(Array(new WeakRef(mux)))
    def remove(mux: Mux1[Reactive, Any]) = this
  }

  class Composite[Source <: Reactive, @spec(Byte, Char, Int, Long, Double) T](var ms: Array[WeakRef[Mux1[Source, T]]]) extends Mux1[Source, T] {
    def dispatch(source: Source, msg: T) {
      var i = 0
      while (i < ms.length) {
        val ref = ms(i).get
        if (ref ne null) ref.dispatch(source, msg)
        i += 1
      }
    }
    def add(recv: Mux1[Source, T]) = {
      ms = ms :+ new WeakRef(recv)
      this
    }
    def remove(recv: Mux1[Source, T]) = {
      ms = ms filter {
        x => x.get != null && x.get != recv
      }
      if (ms.size == 0) None else this
    }
  }

}



