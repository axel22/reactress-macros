package org.reactress
package signal



import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import api._



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

  it should "be zipped" in {
    val rt = new ReactiveTest
    val sp1 = rt.map(rt.x) {
      rt.x + 1
    }
    val sp2 = sp1 map {
      _ + 1
    }
    val sdiff = zip(sp2, sp1) {
      _ - _
    }
    val a = sdiff on { d =>
      assert(d == 1)
    }

    rt.x += 1
    rt.x += 1
  }

  it should "be either" in {
    val rt = new ReactiveTest
    val sodd = rt.filter(rt.x)(1) {
      rt.x % 2 == 1
    }
    val seven = rt.filter(rt.x)(0) {
      rt.x % 2 == 0
    }
    val seither = either(sodd, seven) {
      x => x
    } {
      x => x
    }
    val stream = seither.foldPast(List[Int]()) {
      (acc, x) => x :: acc
    }
    val a = stream on {
      xs => assert(xs.reverse == Stream.from(0).take(xs.length))
    }

    rt.x = 0
    rt.x = 1
    rt.x = 2
  }

}
