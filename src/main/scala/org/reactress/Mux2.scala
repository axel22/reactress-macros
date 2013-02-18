package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Mux2[@spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] extends Serializable {

  def dispatch(source: Reactive, mp: P, mq: Q): Unit

  def union(mux: Mux2[P, Q]): Mux2[P, Q]

  def diff(mux: Mux2[P, Q]): Mux2[P, Q]

}


object Mux2 {

  implicit def Mux2IsMux[@spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] = new IsMux[Mux2[P, Q]] {
    def none = None[P, Q]
    def union(m1: Mux2[P, Q], m2: Mux2[P, Q]) = m1 union m2
    def diff(m1: Mux2[P, Q], m2: Mux2[P, Q]) = m1 diff m2
  }

  def None[@spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] = NoneImpl.asInstanceOf[Mux2[P, Q]]

  private case object NoneImpl extends Mux2[Any, Any] {
    def dispatch(source: Reactive, mp: Any, mq: Any) {}
    def union(mux: Mux2[Any, Any]) = mux
    def diff(mux: Mux2[Any, Any]) = this
  }

  abstract class Simple[@spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] extends Mux2[P, Q] {
    def union(mux: Mux2[P, Q]) = Composite(Array(this, mux))
    def diff(mux: Mux2[P, Q]) = if (this eq mux) None[P, Q] else this
  }

  case class Composite[@spec(Int, Long, Double) P, @spec(Int, Long, Double) Q](ms: Array[Mux2[P, Q]]) extends Mux2[P, Q] {
    def dispatch(source: Reactive, mp: P, mq: Q) {
      var i = 0
      while (i < ms.length) {
        ms(i).dispatch(source, mp, mq)
        i += 1
      }
    }
    def union(mux: Mux2[P, Q]) = mux match {
      case Composite(ns) => Composite(ms ++ ns)
      case _ => Composite(ms :+ mux)
    }
    def diff(mux: Mux2[P, Q]) = mux match {
      case Composite(ns) =>
        val nsmap = ns.toSet
        Composite(ms.filter(!nsmap(_)))
      case _ =>
        Composite(ms.filter(_ ne mux))
    }
  }

  object Factory {

    implicit def block2mux2[P, Q](block: (P, Q) => Any): Mux2[P, Q] = macro block2mux_impl[P, Q]

    def block2mux_impl[P: c.WeakTypeTag, Q: c.WeakTypeTag](c: Context)(block: c.Expr[(P, Q) => Any]): c.Expr[Mux2[P, Q]] = {
      import c.universe._

      val mux = reify {
        new DefaultMux2[P, Q] {
          def dispatch(source: Reactive, mp: P, mq: Q) {
            block.splice(mp, mq)
          }
        }
      }

      c.inlineAndReset(mux)
    }

  }

}



