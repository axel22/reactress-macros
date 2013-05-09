package org.reactress






trait Reactive extends Serializable {
  def flush(ctx: Ctx) = ctx.flush()
}


object Reactive {
  object None extends Reactive
}