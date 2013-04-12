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

  @react def inc(x: Int): Int = x + 1

  val z = map(inc _)(0) {
    (x: Int, res: Int) => res
  }

}



