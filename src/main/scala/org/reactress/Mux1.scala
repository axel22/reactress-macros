package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



abstract class Mux1[@spec(Byte, Char, Int, Long, Double) T] extends Serializable {

  def dispatch(source: Reactive, msg: T): Unit

  def union(mux: Mux1[T]): Mux1[T]

  def diff(mux: Mux1[T]): Mux1[T]

}


object Mux1 {

  implicit def Mux1IsMux[@spec(Byte, Char, Int, Long, Double) T] = new IsMux[Mux1[T]] {
    def none = None[T]
    def union(m1: Mux1[T], m2: Mux1[T]) = m1 union m2
    def diff(m1: Mux1[T], m2: Mux1[T]) = m1 diff m2
  }

  def None[@spec(Byte, Char, Int, Long, Double) T] = NoneImpl.asInstanceOf[Mux1[T]]

  private case object NoneImpl extends Mux1[Any] {
    def dispatch(source: Reactive, msg: Any) {}
    def union(mux: Mux1[Any]) = mux
    def diff(mux: Mux1[Any]) = this
  }

  abstract class Simple[@spec(Byte, Char, Int, Long, Double) T] extends Mux1[T] {
    def union(mux: Mux1[T]) = Composite(Array(this, mux))
    def diff(mux: Mux1[T]) = if (this eq mux) None[T] else this
  }

  case class Composite[@spec(Byte, Char, Int, Long, Double) T](ms: Array[Mux1[T]]) extends Mux1[T] {
    def dispatch(source: Reactive, msg: T) {
      var i = 0
      while (i < ms.length) {
        ms(i).dispatch(source, msg)
        i += 1
      }
    }
    def union(mux: Mux1[T]) = mux match {
      case Composite(ns) => Composite(ms ++ ns)
      case _ => Composite(ms :+ mux)
    }
    def diff(mux: Mux1[T]) = mux match {
      case Composite(ns) =>
        val nsmap = ns.toSet
        Composite(ms.filter(!nsmap(_)))
      case _ =>
        Composite(ms.filter(_ ne mux))
    }
  }

  object Factory {

    implicit def block2mux1[T](block: T => Any): Mux1[T] = macro block2mux_impl[T]

    def block2mux_impl[T: c.WeakTypeTag](c: Context)(block: c.Expr[T => Any]): c.Expr[Mux1[T]] = {
      import c.universe._

      val mux = reify {
        new DefaultMux1[T] {
          def dispatch(source: Reactive, msg: T) {
            block.splice(msg)
          }
        }
      }

      c.inlineAndReset(mux)
    }

  }

}



