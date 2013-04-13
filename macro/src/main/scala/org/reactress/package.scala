package org



import language.experimental.macros
import scala.reflect.macros.Context



package reactress {

  trait IsMux[M] {
    def none: M
    def add(m1: M, m2: M): M
    def diff(m1: M, m2: M): M
  }

  private[reactress] class Util[C <: Context](val c: C) {
    import c.universe._

    def inlineAndReset[T](expr: c.Expr[T]): c.Expr[T] =
      c.Expr[T](c resetAllAttrs inlineApplyRecursive(expr.tree))

    def inlineApplyRecursive(tree: Tree): Tree = {
      val ApplyName = newTermName("apply")

      object inliner extends Transformer {
        override def transform(tree: Tree): Tree = {
          tree match {
            case ap @ Apply(Select(prefix, ApplyName), args) =>
              prefix match {
                case Function(params, body)  =>
                  if (params.length != args.length)
                    c.abort(c.enclosingPosition, "incorrect arity: " + (params.length, args.length))
                  // val a$0 = args(0); val b$0 = args(1); ...
                  val paramVals = params.zip(args).map {
                    case (ValDef(_, pName, _, _), a) =>
                      ValDef(Modifiers(), newTermName("" + pName + "$0"), TypeTree(), a)
                  }
                  // val a = a$0; val b = b$0
                  val paramVals2 = params.zip(args).map {
                    case (ValDef(_, pName, _, _), a) =>
                      ValDef(Modifiers(), pName, TypeTree(), Ident(newTermName("" + pName + "$0")))
                  }
                  // The nested blocks avoid name clashes.
                  Block(paramVals, Block(paramVals2, body))
                case x => ap
              }
            case _ => super.transform(tree)
          }
        }
      }

      inliner.transform(tree)
    }
  }

}














