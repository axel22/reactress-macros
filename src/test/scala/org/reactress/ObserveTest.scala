package org.reactress



import Mux0.Factory._
import Mux2.Factory._



object ObserveTest extends Reactive.Struct {

  @react var x = 1

  var y = 2

  observe(this.x) {
    println("!")
  }

  val table = new container.ReactMap[String, String]
  
  observe(table) {
    (k: String, v: String) => println(k, v)
  }

}



