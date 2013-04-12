package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



trait Reactive extends Serializable


object Reactive {

  trait Struct[Source <: Reactive] extends Reactive {
    def map[T, U](field: T)(body: U): Signal[U] = macro Reactive.mapField[Source, T, U]
    def map[T, U](method: () => T)(init: U)(body: T => U): Signal[U] = macro Reactive.mapMethod0[Source, T, U]
    def map[P, Q, U](method: P => Q)(init: U)(body: (P, Q) => U): Signal[U] = macro Reactive.mapMethod1[Source, P, Q, U]
    def map[P, Q, R, U](method: (P, Q) => R)(init: U)(body: (P, Q, R) => U): Signal[U] = macro Reactive.mapMethod2[Source, P, Q, R, U]
    def map[P, Q, R, S, U](method: (P, Q, R) => S)(init: U)(body: (P, Q, R, S) => U): Signal[U] = macro Reactive.mapMethod3[Source, P, Q, R, S, U]
  }

  def map[Source <: Reactive: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(member: c.Tree)(inst: (c.Expr[Source], c.Expr[Signal[U] => Unit]) => c.Expr[Signal[U]]): c.Expr[Signal[U]] = {
    import c.universe._

    val qn = member match {
      case Block(List(), Function(_, Apply(Select(q, n), _))) => (q, n)
      case Select(q, n) => (q, n)
      case _ => null
    }
    val (owner, muxname) = qn match {
      case (q, n) =>
        val muxname = newTermName(n + "$mux")
        q.symbol.typeSignature.member(muxname) match {
          case NoSymbol =>
            c.error(c.enclosingPosition, "Can only map reactive members.")
            return reify { null }
          case m =>
            (q, muxname)
        }
      case _ =>
        c.error(c.enclosingPosition, "Must use `object.field` or `object.method _` to refer to a reactive field.")
        return reify { null }
    }
    val selectmux = c.Expr[Mux0[Source]](Select(owner, muxname))
    val ownerExpr = c.Expr[Source](owner)
    val removeName = newTermName("remove")
    val paramName = newTermName("param$0")
    val detachBody = c.Expr[Signal[U] => Unit](Function(List(ValDef(Modifiers(), paramName, TypeTree(), EmptyTree)), Apply(Select(selectmux.tree, removeName), List(Ident(paramName)))))
    val signal0 = inst(ownerExpr, detachBody)
    val localName = newTermName("signal$0")
    val addName = newTermName("add")
    val block = Block(
      ValDef(Modifiers(), localName, TypeTree(), signal0.tree),
      Assign(selectmux.tree, Apply(Select(selectmux.tree, addName), List(Ident(localName)))),
      Ident(localName)
    )
    c.inlineAndReset(c.Expr(block))
  }

  def mapMethod0[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[() => T])(init: c.Expr[U])(body: c.Expr[T => U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = map[Source, U](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal[U](init.splice, ownerExpr.splice) with Mux1[Source, T] {
          def dispatch(source: Source, mt: T) {
            value = body.splice(mt)
          }
          def detach() {
            detachBody.splice(this)
          }
          def add(recv: Mux1[Source, T]) = ???
          def remove(recv: Mux1[Source, T]) = ???
        }
      }
    }

    v.asInstanceOf[c.universe.Expr[Signal[U]]]
  }

  def mapMethod1[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[P => Q])(init: c.Expr[U])(body: c.Expr[(P, Q) => U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = map[Source, U](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal[U](init.splice, ownerExpr.splice) with Mux2[Source, P, Q] {
          def dispatch(source: Source, mp: P, mq: Q) {
            value = body.splice(mp, mq)
          }
          def detach() {
            detachBody.splice(this)
          }
          def add(recv: Mux2[Source, P, Q]) = ???
          def remove(recv: Mux2[Source, P, Q]) = ???
        }
      }
    }

    v.asInstanceOf[c.universe.Expr[Signal[U]]]
  }

  def mapMethod2[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[(P, Q) => R])(init: c.Expr[U])(body: c.Expr[(P, Q, R) => U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = map[Source, U](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal[U](init.splice, ownerExpr.splice) with Mux3[Source, P, Q, R] {
          def dispatch(source: Source, mp: P, mq: Q, mr: R) {
            value = body.splice(mp, mq, mr)
          }
          def detach() {
            detachBody.splice(this)
          }
          def add(recv: Mux3[Source, P, Q, R]) = ???
          def remove(recv: Mux3[Source, P, Q, R]) = ???
        }
      }
    }

    v.asInstanceOf[c.universe.Expr[Signal[U]]]
  }

  def mapMethod3[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag, S: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[(P, Q, R) => S])(init: c.Expr[U])(body: c.Expr[(P, Q, R, S) => U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = map[Source, U](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal[U](init.splice, ownerExpr.splice) with Mux4[Source, P, Q, R, S] {
          def dispatch(source: Source, mp: P, mq: Q, mr: R, ms: S) {
            value = body.splice(mp, mq, mr, ms)
          }
          def detach() {
            detachBody.splice(this)
          }
          def add(recv: Mux4[Source, P, Q, R, S]) = ???
          def remove(recv: Mux4[Source, P, Q, R, S]) = ???
        }
      }
    }

    v.asInstanceOf[c.universe.Expr[Signal[U]]]
  }

  def mapField[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(field: c.Expr[T])(body: c.Expr[U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = map[Source, U](c)(field.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal[U](body.splice, ownerExpr.splice) with Mux0[Source] {
          def dispatch(source: Source) {
            value = body.splice
          }
          def detach() {
            detachBody.splice(this)
          }
          def add(recv: Mux0[Source]) = ???
          def remove(recv: Mux0[Source]) = ???
        }
      }
    }

    v.asInstanceOf[c.universe.Expr[Signal[U]]]
  }

}



