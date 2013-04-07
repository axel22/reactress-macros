package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.ClassTag



abstract class Mux0[S <: Reactive] extends Serializable {

  def dispatch(source: S): Unit

  def union(mux: Mux0[S]): Mux0[S]

  def diff(mux: Mux0[S]): Mux0[S]

}


object Mux0 {

  implicit def Mux0IsMux[S <: Reactive] = new IsMux[Mux0[S]] {
    def none = None
    def union(m1: Mux0[S], m2: Mux0[S]) = m1 union m2
    def diff(m1: Mux0[S], m2: Mux0[S]) = m1 diff m2
  }

  def None[S <: Reactive]: Mux0[S] = NoneImpl.asInstanceOf[Mux0[S]]

  private case object NoneImpl extends Mux0[Reactive] {
    def dispatch(source: Reactive) {}
    def union(mux: Mux0[Reactive]) = mux
    def diff(mux: Mux0[Reactive]) = this
  }

  abstract class Simple[S <: Reactive] extends Mux0[S] {
    def union(mux: Mux0[S]) = Composite(Array(this, mux))
    def diff(mux: Mux0[S]) = if (this eq mux) None else this
  }

  case class Composite[S <: Reactive](ms: Array[Mux0[S]]) extends Mux0[S] {
    def dispatch(source: S) {
      var i = 0
      while (i < ms.length) {
        ms(i).dispatch(source)
        i += 1
      }
    }
    def union(mux: Mux0[S]) = mux match {
      case Composite(ns) => Composite(ms ++ ns)
      case _ => Composite(ms :+ mux)
    }
    def diff(mux: Mux0[S]) = mux match {
      case Composite(ns) =>
        val nsset = ns.toSet
        Composite(ms.filter(!nsset(_)))
      case _ =>
        Composite(ms.filter(_ ne mux))
    }
  }

  object Factory {

    def mux[S <: Reactive](block: =>Any): Mux0[S] = macro block2mux_impl[S]

    implicit def mux0[S <: Reactive](block: =>Any): Mux0[S] = macro block2mux_impl[S]

    def block2mux_impl[S <: Reactive: c.WeakTypeTag](c: Context)(block: c.Expr[Any]): c.Expr[Mux0[S]] = {
      import c.universe._

      reify {
        new SimpleMux0[S] {
          def dispatch(source: S) {
            block.splice
          }
        }
      }
    }

    def mux[S <: Reactive](block: S => Any): Mux0[S] = macro func2mux_impl[S]

    implicit def mux0[S <: Reactive](block: S => Any): Mux0[S] = macro func2mux_impl[S]

    def func2mux_impl[S <: Reactive: c.WeakTypeTag](c: Context)(block: c.Expr[S => Any]): c.Expr[Mux0[S]] = {
      import c.universe._

      val mux = reify {
        new SimpleMux0[S] {
          def dispatch(source: S) {
            block.splice(source)
          }
        }
      }

      c.inlineAndReset(mux)
    }

  }

}



