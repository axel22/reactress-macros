package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.mutable.FlatHashTable



trait Mux0[Source <: Reactive] extends Serializable {

  def dispatch(source: Source): Unit

  def add(recv: Mux0[Source]): Mux0[Source]

  def remove(recv: Mux0[Source]): Mux0[Source]

}


object Mux0 {

  def None[Source <: Reactive]: Mux0[Source] = NoneImpl.asInstanceOf[Mux0[Source]]

  private case object NoneImpl extends Mux0[Reactive] {
    def dispatch(source: Reactive) {}
    def add(mux: Mux0[Reactive]) = new Composite().add(this)
    def remove(mux: Mux0[Reactive]) = this
  }

  case class Composite[Source <: Reactive]()
  extends MuxHashTable[Mux0[Source]] with Mux0[Source] {
    def dispatch(source: Source) {
      var i = 0
      while (i < table.length) {
        val weakref = table(i)
        if (weakref ne null) {
          val ref = weakref.get
          if (ref ne null) ref.dispatch(source)
        }
        i += 1
      }
    }

    def add(recv: Mux0[Source]) = {
      addEntry(recv)
      this
    }

    def remove(recv: Mux0[Source]) = {
      removeEntry(recv)
      if (size > 0) this else None
    }
  }

  trait Sink[Source <: Reactive] extends Mux0[Source] {
    def add(recv: Mux0[Source]) = ???
    def remove(recv: Mux0[Source]) = ???
  }

}



