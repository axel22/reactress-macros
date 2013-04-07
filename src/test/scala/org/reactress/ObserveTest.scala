package org.reactress



import Mux0.Factory._
import Mux2.Factory._



object ObserveTest extends Reactive.Struct {
  
  @react var x = 1

  var y = 2

  observe(this.x) {
    (ot: ObserveTest.type) => println("!")
  }

  observe(this.x) {
    println("x changed: " + this.x)
  }

  val table = new container.ReactMap[String, String]
  
  observe(table) {
    (t: container.ReactMap[String, String], k: String, v: String) => (println(k, v): Any)
  }

  @react def pulse(x: Int): Int = 1

}



