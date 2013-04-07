package org.reactress



import language.experimental.macros



trait Reactive extends Serializable {

  def observe[T](field: T)(body: Mux0[this.type]): Mux0[this.type] = macro ObserveImplementations.field[this.type, T]

  def observe[T](method: () => T)(body: Mux1[this.type, T]): Mux1[this.type, T] = macro ObserveImplementations.method0[this.type, T]

  def observe[P, Q](method: P => Q)(body: Mux2[this.type, P, Q]): Mux2[this.type, P, Q] = macro ObserveImplementations.method1[this.type, P, Q]

  def observe[Source <: Reactive, P, Q, R](method: (P, Q) => R)(body: Mux3[Source, P, Q, R]): Mux3[Source, P, Q, R] = macro ObserveImplementations.method2[Source, P, Q, R]

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



