package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import java.lang.ref.{WeakReference => WeakRef}



trait Mux0[Source <: Reactive] extends Serializable {

  def dispatch(source: Source): Unit

  def add(recv: Mux0[Source]): Mux0[Source]

  def remove(recv: Mux0[Source]): Mux0[Source]

}


object Mux0 {

  def None[Source <: Reactive]: Mux0[Source] = NoneImpl.asInstanceOf[Mux0[Source]]

  private case object NoneImpl extends Mux0[Reactive] {
    def dispatch(source: Reactive) {}
    def add(mux: Mux0[Reactive]) = new Composite(Array(new WeakRef(mux)))
    def remove(mux: Mux0[Reactive]) = this
  }

  case class Composite[Source <: Reactive](var ms: Array[WeakRef[Mux0[Source]]]) extends Mux0[Source] {
    def dispatch(source: Source) {
      var i = 0
      while (i < ms.length) {
        val ref = ms(i).get
        if (ref ne null) ref.dispatch(source)
        i += 1
      }
    }
    def add(recv: Mux0[Source]) = {
      ms = ms :+ new WeakRef(recv)
      this
    }
    def remove(recv: Mux0[Source]) = {
      ms = ms filter {
        x => x.get != null && x.get != recv
      }
      if (ms.size == 0) None else this
    }
  }

  trait Sink[Source <: Reactive] extends Mux0[Source] {
    def add(recv: Mux0[Source]) = ???
    def remove(recv: Mux0[Source]) = ???
  }

}



