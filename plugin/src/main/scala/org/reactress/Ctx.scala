package org.reactress






abstract class Ctx {

  def defer(priority: Int, d: Deferrable): Unit

  def flush(): Unit

}


object Ctx {

  private def local = new ThreadLocal[Ctx] {
    override def initialValue = new Default
  }

  implicit def current: Ctx = Thread.currentThread match {
    case _ => local.get
  }

  final class Default extends Ctx {
    def defer(priority: Int, d: Deferrable) = ???
    def flush {
      
    }
  }

}