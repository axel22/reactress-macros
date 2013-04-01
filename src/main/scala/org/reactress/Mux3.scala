package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Mux3[@spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R] extends Serializable {

  def dispatch(source: Reactive, mp: P, mq: Q, mr: R): Unit

  def union(mux: Mux3[P, Q, R]): Mux3[P, Q, R]

  def diff(mux: Mux3[P, Q, R]): Mux3[P, Q, R]

}


object Mux3 {

  implicit def Mux3IsMux[@spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R] = new IsMux[Mux3[P, Q, R]] {
    def none = None[P, Q, R]
    def union(m1: Mux3[P, Q, R], m2: Mux3[P, Q, R]) = m1 union m2
    def diff(m1: Mux3[P, Q, R], m2: Mux3[P, Q, R]) = m1 diff m2
  }

  def None[@spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R] = NoneImpl.asInstanceOf[Mux3[P, Q, R]]

  private case object NoneImpl extends Mux3[Any, Any, Any] {
    def dispatch(source: Reactive, mp: Any, mq: Any, mr: Any) {}
    def union(mux: Mux3[Any, Any, Any]) = mux
    def diff(mux: Mux3[Any, Any, Any]) = this
  }

  abstract class Simple[@spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R] extends Mux3[P, Q, R] {
    def union(mux: Mux3[P, Q, R]) = Composite(Array(this, mux))
    def diff(mux: Mux3[P, Q, R]) = if (this eq mux) None[P, Q, R] else this
  }

  case class Composite[@spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R](ms: Array[Mux3[P, Q, R]]) extends Mux3[P, Q, R] {
    def dispatch(source: Reactive, mp: P, mq: Q, mr: R) {
      var i = 0
      while (i < ms.length) {
        ms(i).dispatch(source, mp, mq, mr)
        i += 1
      }
    }
    def union(mux: Mux3[P, Q, R]) = mux match {
      case Composite(ns) => Composite(ms ++ ns)
      case _ => Composite(ms :+ mux)
    }
    def diff(mux: Mux3[P, Q, R]) = mux match {
      case Composite(ns) =>
        val nsmap = ns.toSet
        Composite(ms.filter(!nsmap(_)))
      case _ =>
        Composite(ms.filter(_ ne mux))
    }
  }

  object Factory {

    def mux[P, Q, R](block: (Reactive, P, Q, R) => Any): Mux3[P, Q, R] = macro block2mux_impl[P, Q, R]

    implicit def mux3[P, Q, R](block: (Reactive, P, Q, R) => Any): Mux3[P, Q, R] = macro block2mux_impl[P, Q, R]

    def block2mux_impl[P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(block: c.Expr[(Reactive, P, Q, R) => Any]): c.Expr[Mux3[P, Q, R]] = {
      import c.universe._

      val mux = reify {
        new SimpleMux3[P, Q, R] {
          def dispatch(source: Reactive, mp: P, mq: Q, mr: R) {
            block.splice(source, mp, mq, mr)
          }
        }
      }

      c.inlineAndReset(mux)
    }

  }

}



