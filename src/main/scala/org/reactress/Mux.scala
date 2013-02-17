package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Mux extends Serializable {

  def dispatch(source: Reactive): Unit

  def union(mux: Mux): Mux

  def diff(mux: Mux): Mux

}


object Mux {

  case object None extends Mux {
    def dispatch(source: Reactive) {}
    def union(mux: Mux) = mux
    def diff(mux: Mux) = this
  }

  abstract class Simple extends Mux {
    def union(mux: Mux) = Composite(Array(this, mux))
    def diff(mux: Mux) = if (this eq mux) None else this
  }

  case class Composite(ms: Array[Mux]) extends Mux {
    def dispatch(source: Reactive) {
      var i = 0
      while (i < ms.length) {
        ms(i).dispatch(source)
        i += 1
      }
    }
    def union(mux: Mux) = mux match {
      case Composite(ns) => Composite(ms ++ ns)
      case _ => Composite(ms :+ mux)
    }
    def diff(mux: Mux) = mux match {
      case Composite(ns) =>
        val nsmap = ns.toSet
        Composite(ms.filter(!nsmap(_)))
      case _ =>
        Composite(ms.filter(_ ne mux))
    }
  }

  object Default {

    implicit def block2mux(block: =>Any): Mux = macro block2mux_impl

    def block2mux_impl(c: Context)(block: c.Expr[Any]): c.Expr[Mux] = {
      import c.universe._

      reify {
        new Mux.Simple {
          def dispatch(source: Reactive) {
            block.splice
          }
        }
      }
    }

  }

}



