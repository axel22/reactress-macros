package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



package object signal {

  /* signal constructors */

  def constant[@specialized T](v: T): Signal[T] = new Signal[T] {
    @react(source[Signal[T]]) var value = v
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
        var deferred = false
        override val priority = {
          val pp = psource.priority
          val qp = qsource.priority
          1 + (if (pp > qp) pp else qp)
        }
        def dispatch(ctx: Ctx, source: Signal[Any]) {
          if (!deferred) {
            deferred = true
            ctx.defer(this)
          }
        }
        def execute(ctx: Ctx) {
          deferred = false
          set(f.splice(psource.value, qsource.value), ctx)
        }
        def detach() {
          psource.value$mux = psource.value$mux.remove(this.asMux)
          qsource.value$mux = qsource.value$mux.remove(this.asMux)
        }
      }
      psrc.value$mux = psrc.value$mux.add(s.asMux)
      qsrc.value$mux = qsrc.value$mux.add(s.asMux)
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