package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



package object signal {

  /* signal constructors */

  def constant[T](v: T): Signal[T] = new Signal[T](v, Reactive.None) {
    def detach() {}
  }

  /* operations on signals */

  implicit class SignalOps[T](val signal: Signal[T]) extends AnyVal {
    def map[S](f: T => S): Signal[S] = macro mapSignal[T, S]
    def foreach[U](f: T => U): Signal[Unit] = macro foreachSignal[T, U]
  }

  def mapSignal[T: c.WeakTypeTag, S: c.WeakTypeTag](c: Context)(f: c.Expr[T => S]): c.Expr[Signal[S]] = {
    import c.universe._

    val Apply(TypeApply(Select(Apply(_, List(signal)), _), _), _) = c.macroApplication
    val field = reify {
      (c.Expr[Signal[T]](signal)).splice.value
    }
    val body = reify {
      f.splice(field.splice)
    }

    Struct.mapField[Signal[T], T, S](c)(field)(body)
  }

  def foreachSignal[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(f: c.Expr[T => U]): c.Expr[Signal[Unit]] = {
    import c.universe._

    val Apply(TypeApply(Select(Apply(_, List(signal)), _), _), _) = c.macroApplication
    val field = reify {
      (c.Expr[Signal[T]](signal)).splice.value
    }
    val body = reify {
      f.splice(field.splice)
    }

    Struct.onField[Signal[T], T, U](c)(field)(body)
  }

}