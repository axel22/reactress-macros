package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Mux3[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R] extends Serializable {

  def dispatch(source: Source, mp: P, mq: Q, mr: R): Unit

  def union(mux: Mux3[Source, P, Q, R]): Mux3[Source, P, Q, R]

  def diff(mux: Mux3[Source, P, Q, R]): Mux3[Source, P, Q, R]

}


object Mux3 {

  implicit def Mux3IsMux[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R] = new IsMux[Mux3[Source, P, Q, R]] {
    def none = None[Source, P, Q, R]
    def union(m1: Mux3[Source, P, Q, R], m2: Mux3[Source, P, Q, R]) = m1 union m2
    def diff(m1: Mux3[Source, P, Q, R], m2: Mux3[Source, P, Q, R]) = m1 diff m2
  }

  def None[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R] = NoneImpl.asInstanceOf[Mux3[Source, P, Q, R]]

  private case object NoneImpl extends Mux3[Reactive, Any, Any, Any] {
    def dispatch(source: Reactive, mp: Any, mq: Any, mr: Any) {}
    def union(mux: Mux3[Reactive, Any, Any, Any]) = mux
    def diff(mux: Mux3[Reactive, Any, Any, Any]) = this
  }

  abstract class Simple[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R] extends Mux3[Source, P, Q, R] {
    def union(mux: Mux3[Source, P, Q, R]) = new Composite(Array(this, mux))
    def diff(mux: Mux3[Source, P, Q, R]) = if (this eq mux) None[Source, P, Q, R] else this
  }

  class Composite[Source <: Reactive, @spec(Int, Double) P, @spec(Int, Double) Q, @spec(Byte, Int, Long, Double) R](val ms: Array[Mux3[Source, P, Q, R]]) extends Mux3[Source, P, Q, R] {
    def dispatch(source: Source, mp: P, mq: Q, mr: R) {
      var i = 0
      while (i < ms.length) {
        ms(i).dispatch(source, mp, mq, mr)
        i += 1
      }
    }
    def union(mux: Mux3[Source, P, Q, R]) = mux match {
      case c: Composite[Source, P, Q, R] => new Composite(ms ++ c.ms)
      case _ => new Composite(ms :+ mux)
    }
    def diff(mux: Mux3[Source, P, Q, R]) = mux match {
      case c: Composite[Source, P, Q, R] =>
        val nsmap = c.ms.toSet
        new Composite(ms.filter(!nsmap(_)))
      case _ =>
        new Composite(ms.filter(_ ne mux))
    }
  }

  object Factory {

    def mux[Source <: Reactive, P, Q, R](block: (Source, P, Q, R) => Any): Mux3[Source, P, Q, R] = macro block2mux_impl[Source, P, Q, R]

    implicit def mux3[Source <: Reactive, P, Q, R](block: (Source, P, Q, R) => Any): Mux3[Source, P, Q, R] = macro block2mux_impl[Source, P, Q, R]

    def block2mux_impl[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(block: c.Expr[(Source, P, Q, R) => Any]): c.Expr[Mux3[Source, P, Q, R]] = {
      import c.universe._

      val mux = reify {
        new SimpleMux3[Source, P, Q, R] {
          def dispatch(source: Source, mp: P, mq: Q, mr: R) {
            block.splice(source, mp, mq, mr)
          }
        }
      }

      c.inlineAndReset(mux)
    }

  }

}



