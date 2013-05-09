package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



package object signal {

  /* signal constructors */

  def constant[T](v: T): Signal[T] = new Signal[T](v) {
    override def priority = 0
    def detach() {}
  }

  def zip[P, Q, W](p: Signal[P], q: Signal[Q])(f: (P, Q) => W): Signal[W] = macro zipSignals[P, Q, W]

  /* operations on signals */

  implicit def c2utils(c: Context) = new Util[c.type](c)

  implicit class SignalOps[T](val signal: Signal[T]) extends AnyVal {
    def map[S](f: T => S): Signal[S] = macro mapSignal[T, S]
    def on[U](f: T => U): Signal[Unit] = macro onSignal[T, U]
    def foldPast[S](z: S)(op: (S, T) => S): Signal[S] = macro foldPastSignal[T, S]
  }

  def zipSignals[P: c.WeakTypeTag, Q: c.WeakTypeTag, W: c.WeakTypeTag](c: Context)(p: c.Expr[Signal[P]], q: c.Expr[Signal[Q]])(f: c.Expr[(P, Q) => W]): c.Expr[Signal[W]] = {
    import c.universe._

    val s = reify {
      val psrc = p.splice
      val qsrc = q.splice
      val s = new Signal.M0[Signal[Any], W](f.splice(psrc.value, qsrc.value)) with Deferrable {
        val psource = psrc
        val qsource = qsrc
        override val priority = {
          val pp = psource.priority
          val qp = qsource.priority
          1 + (if (pp > qp) pp else qp)
        }
        def asP = this.asInstanceOf[Mux0[Signal[P]]]
        def asQ = this.asInstanceOf[Mux0[Signal[Q]]]
        def dispatch(ctx: Ctx, source: Signal[Any]) {
          ctx.defer(this)
        }
        def execute(ctx: Ctx) {
          value = f.splice(psource.value, qsource.value)
        }
        def detach() {
          psource.value$mux.remove(asP)
          qsource.value$mux.remove(asQ)
        }
      }
      psrc.value$mux.add(s.asP)
      qsrc.value$mux.add(s.asQ)
      s
    }

    c.inlineAndReset[Signal[W]](s)
  }

  def foldPastSignal[T: c.WeakTypeTag, S: c.WeakTypeTag](c: Context)(z: c.Expr[S])(op: c.Expr[(S, T) => S]): c.Expr[Signal[S]] = {
    import c.universe._

    val Apply(TypeApply(Select(Apply(_, List(signal)), _), _), _) = c.macroApplication
    val field = reify {
      (c.Expr[Signal[T]](signal)).splice.value
    }

    Struct.foldField[Signal[T], T, S](c)(field)(z)(op)
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

  def onSignal[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(f: c.Expr[T => U]): c.Expr[Signal[Unit]] = {
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