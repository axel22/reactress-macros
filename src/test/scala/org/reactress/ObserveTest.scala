package org.reactress



import org.scalatest._



class ObserverTest extends FunSuite {
 
  test("calling observe should compile") {
    implicit val mf: Multiplexer.Factory = null

    observe(null, 'field) {
      println("!")
    }
  }

}




