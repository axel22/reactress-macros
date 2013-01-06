package org.reactress






trait Mux {

  def dispatch(source: Reactive): Unit

}


object Mux {

  object None extends Mux {
    def dispatch(source: Reactive) {
    }
  }

  trait Factory {

  }

}



