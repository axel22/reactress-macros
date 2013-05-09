package org.reactress






trait Reactive extends Serializable {
  def flush(ctx: Ctx): Unit = ctx.flush()
  def priority: Int = 0
}


object Reactive {
  object None extends Reactive
}