package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Mux1[Source <: Reactive, @spec(Byte, Char, Int, Long, Double) T] extends Serializable {

  def dispatch(source: Source, msg: T): Unit

  def union(mux: Mux1[Source, T]): Mux1[Source, T]

  def diff(mux: Mux1[Source, T]): Mux1[Source, T]

}


object Mux1 {

  implicit def Mux1IsMux[Source <: Reactive, @spec(Byte, Char, Int, Long, Double) T] = new IsMux[Mux1[Source, T]] {
    def none = None[Source, T]
    def union(m1: Mux1[Source, T], m2: Mux1[Source, T]) = m1 union m2
    def diff(m1: Mux1[Source, T], m2: Mux1[Source, T]) = m1 diff m2
  }

  def None[Source <: Reactive, @spec(Byte, Char, Int, Long, Double) T] = NoneImpl.asInstanceOf[Mux1[Source, T]]

  private case object NoneImpl extends Mux1[Reactive, Any] {
    def dispatch(source: Reactive, msg: Any) {}
    def union(mux: Mux1[Reactive, Any]) = mux
    def diff(mux: Mux1[Reactive, Any]) = this
  }

  abstract class Simple[Source <: Reactive, @spec(Byte, Char, Int, Long, Double) T] extends Mux1[Source, T] {
    def union(mux: Mux1[Source, T]) = new Composite(Array(this, mux))
    def diff(mux: Mux1[Source, T]) = if (this eq mux) None[Source, T] else this
  }

  class Composite[Source <: Reactive, @spec(Byte, Char, Int, Long, Double) T](val ms: Array[Mux1[Source, T]]) extends Mux1[Source, T] {
    def dispatch(source: Source, msg: T) {
      var i = 0
      while (i < ms.length) {
        ms(i).dispatch(source, msg)
        i += 1
      }
    }
    def union(mux: Mux1[Source, T]) = mux match {
      case c: Composite[Source, T] => new Composite(ms ++ c.ms)
      case _ => new Composite(ms :+ mux)
    }
    def diff(mux: Mux1[Source, T]) = mux match {
      case c: Composite[Source, T] =>
        val nsmap = c.ms.toSet
        new Composite(ms.filter(!nsmap(_)))
      case _ =>
        new Composite(ms.filter(_ ne mux))
    }
  }

  object Factory {

    def mux[Source <: Reactive, T](block: (Source, T) => Any): Mux1[Source, T] = macro block2mux_impl[Source, T]

    implicit def mux1[Source <: Reactive, T](block: (Source, T) => Any): Mux1[Source, T] = macro block2mux_impl[Source, T]

    def block2mux_impl[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(block: c.Expr[(Source, T) => Any]): c.Expr[Mux1[Source, T]] = {
      import c.universe._

      val mux = reify {
        new SimpleMux1[Source, T] {
          def dispatch(source: Source, msg: T) {
            block.splice(source, msg)
          }
        }
      }

      c.inlineAndReset(mux)
    }

  }

}



