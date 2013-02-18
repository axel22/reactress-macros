package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Mux0 extends Serializable {

  def dispatch(source: Reactive): Unit

  def union(mux: Mux0): Mux0

  def diff(mux: Mux0): Mux0

}


object Mux0 {

  implicit val Mux0IsMux = new IsMux[Mux0] {
    def none = None
    def union(m1: Mux0, m2: Mux0) = m1 union m2
    def diff(m1: Mux0, m2: Mux0) = m1 diff m2
  }

  def None: Mux0 = NoneImpl

  private case object NoneImpl extends Mux0 {
    def dispatch(source: Reactive) {}
    def union(mux: Mux0) = mux
    def diff(mux: Mux0) = this
  }

  abstract class Simple extends Mux0 {
    def union(mux: Mux0) = Composite(Array(this, mux))
    def diff(mux: Mux0) = if (this eq mux) None else this
  }

  case class Composite(ms: Array[Mux0]) extends Mux0 {
    def dispatch(source: Reactive) {
      var i = 0
      while (i < ms.length) {
        ms(i).dispatch(source)
        i += 1
      }
    }
    def union(mux: Mux0) = mux match {
      case Composite(ns) => Composite(ms ++ ns)
      case _ => Composite(ms :+ mux)
    }
    def diff(mux: Mux0) = mux match {
      case Composite(ns) =>
        val nsmap = ns.toSet
        Composite(ms.filter(!nsmap(_)))
      case _ =>
        Composite(ms.filter(_ ne mux))
    }
  }

  object Factory {

    implicit def block2mux0(block: =>Any): Mux0 = macro block2mux_impl

    def block2mux_impl(c: Context)(block: c.Expr[Any]): c.Expr[Mux0] = {
      import c.universe._

      reify {
        new Mux0.Simple {
          def dispatch(source: Reactive) {
            block.splice
          }
        }
      }
    }

  }

}



