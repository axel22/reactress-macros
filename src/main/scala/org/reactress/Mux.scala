package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



trait Mux extends Serializable {

  def dispatch(source: Reactive): Unit

}


object Mux {

  case object None extends Mux {
    def dispatch(source: Reactive) {
    }
  }

  case class Composite(ms: Array[Mux]) extends Mux {
    def dispatch(source: Reactive) {
      for (mux <- ms) mux.dispatch(source)
    }
  }

  trait Factory {
    def merge(omux: Mux, nmux: Mux): Mux
  }

  class DefaultFactory extends Factory {
    def merge(omux: Mux, nmux: Mux): Mux = omux match {
      case Mux.Composite(ms) => Mux.Composite(ms :+ nmux)
      case _ => Mux.Composite(Array(omux, nmux))
    }
  }

  object Default {

    implicit val factory = new DefaultFactory

    implicit def block2mux(block: =>Any): Mux = macro block2mux_impl

    def block2mux_impl(c: Context)(block: c.Expr[Any]): c.Expr[Mux] = {
      import c.universe._

      reify {
        new Mux {
          def dispatch(source: Reactive) {
            block.splice
          }
        }
      }
    }

  }

}



