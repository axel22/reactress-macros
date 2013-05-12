package org.reactress



import language.experimental.macros
import scala.reflect.macros.Context



object api {

  /* signal constructors */

  def constant[@specialized T](v: T): Signal[T] = new Signal[T] {
    @react(source[Signal[T]]) var value = v
    override def priority = 0
    def detach() {}
  }

  def zip[P, Q, W](p: Signal[P], q: Signal[Q])(f: (P, Q) => W): Signal[W] = macro signal.zipSignals[P, Q, W]

  def either[P, Q, W](p: Signal[P], q: Signal[Q])(pf: P => W)(qf: Q => W): Signal[W] = macro signal.eitherSignals[P, Q, W]

  def either[P, Q, R, W](p: Signal[P], q: Signal[Q], r: Signal[R])(pf: P => W)(qf: Q => W)(rf: R => W): Signal[W] = macro signal.eitherSignals3[P, Q, R, W]

  /* signal operations */

  implicit class SignalOps[T](val s: Signal[T]) extends AnyVal {
    def map[S](f: T => S): Signal[S] = macro signal.mapSignal[T, S]
    def on[U](f: T => U): Signal[Unit] = macro signal.onSignal[T, U]
    def foldPast[S](z: S)(op: (S, T) => S): Signal[S] = macro signal.foldPastSignal[T, S]
  }

}
