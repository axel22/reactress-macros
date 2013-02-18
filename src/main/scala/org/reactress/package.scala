package org



import language.experimental.macros
import scala.reflect.macros.Context



package object reactress {

  def observe[T](field: T)(body: Mux0): Mux0 = macro ObserveImplementations.field[T]

  def observe[M](source: Reactive.Source[M])(body: M)(implicit ismux: IsMux[M]): M = macro ObserveImplementations.source[M]

  object ObserveImplementations {
    def field[T](c: Context)(field: c.Expr[T])(body: c.Expr[Mux0]): c.Expr[Mux0] = {
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
  
      val selectmux = c.Expr[Mux0](Select(owner, muxname))
      val localName = newTermName("muxbody$0")
      val unionName = newTermName("union")
      val block = Block(
        ValDef(Modifiers(), localName, TypeTree(), body.tree),
        Assign(selectmux.tree, Apply(Select(selectmux.tree, unionName), List(Ident(localName)))),
        Ident(localName)
      )
      c.Expr(block)
    }
  
    def source[M](c: Context)(source: c.Expr[Reactive.Source[M]])(body: c.Expr[M])(ismux: c.Expr[IsMux[M]]): c.Expr[M] = {
      import c.universe._

      val localName = newTermName("muxbody$0")
      val attachName = newTermName("attach")
      val block = Block(
        ValDef(Modifiers(), localName, TypeTree(), body.tree),
        Apply(Select(source.tree, attachName), List(Ident(localName))),
        Ident(localName)
      )
      c.Expr(block)
    }
  }

  implicit def Util(context: Context) = new Util[context.type](context)

  type spec = specialized

  type DefaultMux0 = Mux0.Simple

  type DefaultMux1[T] = Mux1.Simple[T]

  type DefaultMux2[P, Q] = Mux2.Simple[P, Q]

  type DefaultMux3[P, Q, R] = Mux3.Simple[P, Q, R]

}


package reactress {

  final class react extends scala.annotation.Annotation

  trait IsMux[M] {
    def none: M
    def union(m1: M, m2: M): M
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




