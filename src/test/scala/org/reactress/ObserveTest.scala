package org.reactress



import Mux.Default._



object ObserveTest extends Reactive {

  @react var x = 1

  var y = 2

  observe(this.x) {
    println("!")
  }

}



