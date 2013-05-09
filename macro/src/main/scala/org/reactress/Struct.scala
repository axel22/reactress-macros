package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



trait Struct[Source <: Reactive] extends Reactive {
  def map[T, U](field: T)(body: U): Signal[U] = macro Struct.mapField[Source, T, U]
  def map[T, U](method: () => T)(init: U)(body: T => U): Signal[U] = macro Struct.mapMethod0[Source, T, U]
  def map[P, Q, U](method: P => Q)(init: U)(body: (P, Q) => U): Signal[U] = macro Struct.mapMethod1[Source, P, Q, U]
  def map[P, Q, R, U](method: (P, Q) => R)(init: U)(body: (P, Q, R) => U): Signal[U] = macro Struct.mapMethod2[Source, P, Q, R, U]
  def map[P, Q, R, S, U](method: (P, Q, R) => S)(init: U)(body: (P, Q, R, S) => U): Signal[U] = macro Struct.mapMethod3[Source, P, Q, R, S, U]
  def filter[T](field: T)(init: T)(body: Boolean): Signal[T] = macro Struct.filterField[Source, T]
  def filter[T](method: () => T)(init: T)(body: T => Boolean): Signal[T] = macro Struct.filterMethod0[Source, T]
  def tupled[P, Q](method: P => Q)(init: (P, Q)): Signal[(P, Q)] = macro Struct.tupledMethod1[Source, P, Q]
  def tupled[P, Q, R](method: (P, Q) => R)(init: (P, Q, R)): Signal[(P, Q, R)] = macro Struct.tupledMethod2[Source, P, Q, R]
  def tupled[P, Q, R, S](method: (P, Q, R) => S)(init: (P, Q, R, S)): Signal[(P, Q, R, S)] = macro Struct.tupledMethod3[Source, P, Q, R, S]
  def on[T, U](field: T)(body: U): Signal[Unit] = macro Struct.onField[Source, T, U]
  def on[T, U](method: () => T)(body: T => U): Signal[Unit] = macro Struct.onMethod0[Source, T, U]
  def on[P, Q, U](method: P => Q)(body: (P, Q) => U): Signal[Unit] = macro Struct.onMethod1[Source, P, Q, U]
  def on[P, Q, R, U](method: (P, Q) => R)(body: (P, Q, R) => U): Signal[Unit] = macro Struct.onMethod2[Source, P, Q, R, U]
  def on[P, Q, R, S, U](method: (P, Q, R) => S)(body: (P, Q, R, S) => U): Signal[Unit] = macro Struct.onMethod3[Source, P, Q, R, S, U]
  def foldPast[T, U](field: T)(init: U)(op: (U, T) => U): Signal[U] = macro Struct.foldField[Source, T, U]
}


object Struct {

  implicit def c2utils(c: Context) = new Util[c.type](c)

  def foldField[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(field: c.Expr[T])(init: c.Expr[U])(op: c.Expr[(U, T) => U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = createSignal[Source, U](c)(field.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M0[Source, U](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source) {
            value = op.splice(value, field.splice)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def onMethod3[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag, S: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[(P, Q, R) => S])(body: c.Expr[(P, Q, R, S) => U]): c.Expr[Signal[Unit]] = {
    import c.universe._

    val v = createSignal[Source, Unit](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M4[Source, Unit, P, Q, R, S]((), ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R, ms: S) {
            body.splice(mp, mq, mr, ms)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def onMethod2[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[(P, Q) => R])(body: c.Expr[(P, Q, R) => U]): c.Expr[Signal[Unit]] = {
    import c.universe._

    val v = createSignal[Source, Unit](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M3[Source, Unit, P, Q, R]((), ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R) {
            body.splice(mp, mq, mr)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def onMethod1[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[P => Q])(body: c.Expr[(P, Q) => U]): c.Expr[Signal[Unit]] = {
    import c.universe._

    val v = createSignal[Source, Unit](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M2[Source, Unit, P, Q]((), ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q) {
            body.splice(mp, mq)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def onMethod0[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[() => T])(body: c.Expr[T => U]): c.Expr[Signal[Unit]] = {
    import c.universe._

    val v = createSignal[Source, Unit](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M1[Source, Unit, T]((), ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mt: T) {
            body.splice(mt)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def onField[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(field: c.Expr[T])(body: c.Expr[U]): c.Expr[Signal[Unit]] = {
    import c.universe._

    val v = createSignal[Source, Unit](c)(field.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M0[Source, Unit]((), ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source) {
            body.splice
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def tupledMethod1[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag](c: Context)(method: c.Expr[P => Q])(init: c.Expr[(P, Q)]): c.Expr[Signal[(P, Q)]] = {
    import c.universe._

    val v = createSignal[Source, (P, Q)](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M2[Source, (P, Q), P, Q](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q) {
            value = (mp, mq)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def tupledMethod2[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(method: c.Expr[(P, Q) => R])(init: c.Expr[(P, Q, R)]): c.Expr[Signal[(P, Q, R)]] = {
    import c.universe._

    val v = createSignal[Source, (P, Q, R)](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M3[Source, (P, Q, R), P, Q, R](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R) {
            value = (mp, mq, mr)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def tupledMethod3[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag, S: c.WeakTypeTag](c: Context)(method: c.Expr[(P, Q, R) => S])(init: c.Expr[(P, Q, R, S)]): c.Expr[Signal[(P, Q, R, S)]] = {
    import c.universe._

    val v = createSignal[Source, (P, Q, R, S)](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M4[Source, (P, Q, R, S), P, Q, R, S](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R, ms: S) {
            value = (mp, mq, mr, ms)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def filterMethod0[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(method: c.Expr[() => T])(init: c.Expr[T])(body: c.Expr[T => Boolean]): c.Expr[Signal[T]] = {
    import c.universe._

    val v = createSignal[Source, T](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M1[Source, T, T](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, v: T) {
            if (body.splice(v)) value = v
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def filterField[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(field: c.Expr[T])(init: c.Expr[T])(body: c.Expr[Boolean]): c.Expr[Signal[T]] = {
    import c.universe._

    val v = createSignal[Source, T](c)(field.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M0[Source, T](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source) {
            if (body.splice) value = field.splice
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def mapMethod0[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[() => T])(init: c.Expr[U])(body: c.Expr[T => U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = createSignal[Source, U](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M1[Source, U, T](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mt: T) {
            value = body.splice(mt)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def mapMethod1[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[P => Q])(init: c.Expr[U])(body: c.Expr[(P, Q) => U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = createSignal[Source, U](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M2[Source, U, P, Q](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q) {
            value = body.splice(mp, mq)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def mapMethod2[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[(P, Q) => R])(init: c.Expr[U])(body: c.Expr[(P, Q, R) => U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = createSignal[Source, U](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M3[Source, U, P, Q, R](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R) {
            value = body.splice(mp, mq, mr)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def mapMethod3[Source <: Reactive: c.WeakTypeTag, P: c.WeakTypeTag, Q: c.WeakTypeTag, R: c.WeakTypeTag, S: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(method: c.Expr[(P, Q, R) => S])(init: c.Expr[U])(body: c.Expr[(P, Q, R, S) => U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = createSignal[Source, U](c)(method.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M4[Source, U, P, Q, R, S](init.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source, mp: P, mq: Q, mr: R, ms: S) {
            value = body.splice(mp, mq, mr, ms)
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def mapField[Source <: Reactive: c.WeakTypeTag, T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(field: c.Expr[T])(body: c.Expr[U]): c.Expr[Signal[U]] = {
    import c.universe._

    val v = createSignal[Source, U](c)(field.tree) { (ownerExpr, detachBody) =>
      reify {
        new Signal.M0[Source, U](body.splice, ownerExpr.splice) {
          def dispatch(ctx: Ctx, source: Source) {
            value = body.splice
          }
          def detach() {
            detachBody.splice(this)
          }
        }
      }
    }

    v
  }

  def createSignal[Source <: Reactive: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(member: c.Tree)(inst: (c.Expr[Source], c.Expr[Signal[U] => Unit]) => c.Expr[Signal[U]]): c.Expr[Signal[U]] = {
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
            println(q.symbol.typeSignature.members)
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
    c.inlineAndReset[Signal[U]](c.Expr[Signal[U]](block))
  }

}



