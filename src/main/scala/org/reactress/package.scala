package org



import language.experimental.macros
import scala.{Symbol => Sym}
import scala.reflect.macros.Context



package object reactress {

  def observe(r: Reactive, fieldname: Sym)(body: Any)(implicit f: Mux.Factory): Observer = macro observe_impl

  def observe_impl(c: Context)(r: c.Expr[Reactive], fieldname: c.Expr[Sym])(body: c.Expr[Any])(f: c.Expr[Mux.Factory]): c.Expr[Observer] = {
    import c.universe._

    // TODO

    reify {
      null
    }
  }

}


package reactress {

  final class react extends scala.annotation.Annotation

}




