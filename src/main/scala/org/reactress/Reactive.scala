package org.reactress






trait Reactive extends Serializable {

}


object Reactive {

  trait Struct extends Reactive

  abstract class Source[M: IsMux] extends Reactive {
    protected[reactress] var mux: M = implicitly[IsMux[M]].none

    def attach(m: M) = mux = implicitly[IsMux[M]].union(mux, m)

    def detach(m: M) = mux = implicitly[IsMux[M]].diff(mux, m)
  }

}



