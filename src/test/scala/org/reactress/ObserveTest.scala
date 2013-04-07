package org.reactress



import Mux0.Factory._
import Mux1.Factory._
import Mux2.Factory._



object ObserveTest extends Reactive {
  
  @react var x = 1

  observe(this.x) {
    (ot: ObserveTest.type) => println("!")
  }

  @react def pulse() {}

  pulse()

  observe(this.pulse _) {
    (ot: ObserveTest.type, u: Unit) => println("Pulsed!")
  }

  pulse()

  @react def inc(x: Int): Int = x + 1

  observe(this.inc _) {
    (ot: ObserveTest.type, x: Int, res: Int) => println(x + " incremented: " + res)
  }

}



