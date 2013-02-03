package org



import language.experimental.macros
import scala.reflect.macros.Context



package object reactress {

  def observe[T](field: T)(body: Mux)(implicit f: Mux.Factory): Mux = macro observe_impl[T]

  def observe_impl[T](c: Context)(field: c.Expr[T])(body: c.Expr[Mux])(f: c.Expr[Mux.Factory]): c.Expr[Mux] = {
    import c.universe._

    val (owner, muxname) = field.tree match {
      case Select(q, n) =>
        val muxname = newTermName(n + "$mux")
        q.symbol.typeSignature.member(muxname) match {
          case NoSymbol =>
            c.error(c.enclosingPosition, "Can only observe reactive fields.")
            return reify { null }
          case m =>
            (q, muxname)
        }
      case _ =>
        c.error(c.enclosingPosition, "Must use `object.field` to refer to a reactive field.")
        return reify { null }
    }

    val mux = c.Expr[Mux](Select(owner, muxname))
    val combined = reify {
      f.splice.merge(mux.splice, body.splice)
    }
    val assign = c.Expr[Mux](Assign(mux.tree, combined.tree))
    reify {
      assign.splice
      mux.splice
    }
  }

}


package reactress {

  final class react extends scala.annotation.Annotation

}




