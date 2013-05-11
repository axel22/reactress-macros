package org.reactress
package signal



import org.scalatest._
import org.scalatest.matchers.ShouldMatchers



class SignalSpec extends FlatSpec with ShouldMatchers {

  "A signal" should "be constant" in {
    val s = constant(0)
    assert(s.value == 0)
  }

  class ReactiveTest extends Struct[ReactiveTest] {
    @react var x = 0
  }

  it should "be mapped" in {
    val rt = new ReactiveTest
    val s = rt.map(rt.x) {
      rt.x + 1
    }
    val a = s on { x =>
      assert(x == 2)
    }

    rt.x = 1
  }

  it should "be filtered" in {
    val rt = new ReactiveTest
    val s = rt.filter(rt.x) (0) {
      rt.x % 2 == 0
    }
    val a = s on { x =>
      assert(x % 2 == 0)
    }

    rt.x = 1
    rt.x = 2
  }

  it should "be folded past" in {
    val rt = new ReactiveTest
    val s = rt.foldPast(rt.x) (List[Int]()) { (acc, x) =>
      x :: acc
    }
    val a = s on { xs =>
      assert(xs.reverse == Stream.from(1).take(xs.length))
    }

    rt.x = 1
    rt.x = 2
    rt.x = 3
    rt.x = 4
    rt.x = 5
  }

}
