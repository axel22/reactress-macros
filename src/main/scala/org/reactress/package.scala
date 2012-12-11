package org



import language.experimental.macros
import scala.{Symbol => Sym}
import scala.reflect.macros.Context



package object reactress {

  def observe(r: Reactive, field: Sym)(body: Any)(implicit f: Multiplexer.Factory): Observer = macro observe_impl

  def observe_impl(c: Context)(r: c.Expr[Reactive], field: c.Expr[Sym])(body: c.Expr[Any])(f: c.Expr[Multiplexer.Factory]): c.Expr[Observer] = {
    import c.universe._

    // TODO

    println(r, field, body, f)

    reify {
      new Observer {}
    }
  }  

}






