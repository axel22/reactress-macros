package org.reactress



import language.experimental.macros



trait Reactive extends Serializable {

  def observe[T](field: T)(body: Mux0[this.type]): Mux0[this.type] = macro ObserveImplementations.field[this.type, T]

  def observe[M](source: Reactive.Source[M])(body: M)(implicit ismux: IsMux[M]): M = macro ObserveImplementations.source[M]
  
}


object Reactive {

  trait Struct extends Reactive

  abstract class Source[M: IsMux] extends Reactive {
    protected[reactress] var mux: M = implicitly[IsMux[M]].none

    def attach(m: M) = mux = implicitly[IsMux[M]].union(mux, m)

    def detach(m: M) = mux = implicitly[IsMux[M]].diff(mux, m)
  }

}



