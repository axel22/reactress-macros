package org.reactress






trait Deferrable {

  def priority: Int

  def execute(ctx: Ctx): Unit

}


object Deferrable {

  implicit val ordering = new Ordering[Deferrable] {
    def compare(a: Deferrable, b: Deferrable) = a.priority - b.priority
  }

}


