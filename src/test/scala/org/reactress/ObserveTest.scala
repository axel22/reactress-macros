package org.reactress



import api._



class ObserveTest extends Struct[ObserveTest] {

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

  val ot: ObserveTest = this

  val s2 = map(ot.inc _)(0) {
    (x: Int, res: Int) => res
  }

  val s = on(ot.inc _) {
    (x: Int, res: Int) => println("someone called it! " + x + ", " + res)
  }

  val z2 = z map { x => x - 1 }

  val zipped = zip(z, z2) { (a, b) =>
    a - b
  }

}

