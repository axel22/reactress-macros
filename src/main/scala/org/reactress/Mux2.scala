package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Mux2[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] extends Serializable {

  def dispatch(source: Source, mp: P, mq: Q): Unit

  def union(mux: Mux2[Source, P, Q]): Mux2[Source, P, Q]

  def diff(mux: Mux2[Source, P, Q]): Mux2[Source, P, Q]

}


object Mux2 {

  implicit def Mux2IsMux[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] = new IsMux[Mux2[Source, P, Q]] {
    def none = None[Source, P, Q]
    def union(m1: Mux2[Source, P, Q], m2: Mux2[Source, P, Q]) = m1 union m2
    def diff(m1: Mux2[Source, P, Q], m2: Mux2[Source, P, Q]) = m1 diff m2
  }

  def None[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] = NoneImpl.asInstanceOf[Mux2[Source, P, Q]]

  private case object NoneImpl extends Mux2[Reactive, Any, Any] {
    def dispatch(source: Reactive, mp: Any, mq: Any) {}
    def union(mux: Mux2[Reactive, Any, Any]) = mux
    def diff(mux: Mux2[Reactive, Any, Any]) = this
  }

  abstract class Simple[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q] extends Mux2[Source, P, Q] {
    def union(mux: Mux2[Source, P, Q]) = new Composite(Array(this, mux))
    def diff(mux: Mux2[Source, P, Q]) = if (this eq mux) None[Source, P, Q] else this
  }

  class Composite[Source <: Reactive, @spec(Int, Long, Double) P, @spec(Int, Long, Double) Q](val ms: Array[Mux2[Source, P, Q]]) extends Mux2[Source, P, Q] {
    def dispatch(source: Source, mp: P, mq: Q) {
      var i = 0
      while (i < ms.length) {
        ms(i).dispatch(source, mp, mq)
        i += 1
      }
    }
    def union(mux: Mux2[Source, P, Q]) = mux match {
      case c: Composite[Source, P, Q] => new Composite(ms ++ c.ms)
      case _ => new Composite(ms :+ mux)
    }
    def diff(mux: Mux2[Source, P, Q]) = mux match {
      case c: Composite[Source, P, Q] =>
        val nsmap = c.ms.toSet
        new Composite(ms.filter(!nsmap(_)))
      case _ =>
        new Composite(ms.filter(_ ne mux))
    }
  }

  object Factory {

    def mux[Source <: Reactive, P, Q](block: (Source, P, Q) => Any): Mux2[Source, P, Q] = macro block2mux_impl[Source, P, Q]

    implicit def mux2[Source <: Reactive, P, Q](block: (Source, P, Q) => Any): Mux2[Source, P, Q] = macro block2mux_impl[Source, P, Q]

    def block2mux_impl[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag](c: Context)(block: c.Expr[(Source, P, Q) => Any]): c.Expr[Mux2[Source, P, Q]] = {
      import c.universe._

      val mux = reify {
        new SimpleMux2[Source, P, Q] {
          def dispatch(source: Source, mp: P, mq: Q) {
            block.splice(source, mp, mq)
          }
        }
      }

      c.inlineAndReset(mux)
    }

  }

}



