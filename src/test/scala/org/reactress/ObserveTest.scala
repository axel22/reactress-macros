package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



class ObserveTest extends Reactive.Struct[ObserveTest] {

  @react var x = 1

  map(x) {
    println(x)
  }

  @react def pulse() {}

  pulse()

  val pulseSignal = map(pulse _)(()) {
    u: Unit => println("pulsed!")
  }

  // observe(this.x) {
  //   (ot: ObserveTest.type) => println("!")
  // }

  // observe(this.pulse _) {
  //   (ot: ObserveTest.type, u: Unit) => println("Pulsed!")
  // }

  pulse()

  @react def inc(x: Int): Int = x + 1

  // observe(this.inc _) {
  //   (ot: ObserveTest.type, x: Int, res: Int) => println(x + " incremented: " + res)
  // }

}



