package org.reactress



import scala.collection._



abstract class Ctx {

  def defer(d: Deferrable): Unit

  def flush(): Unit

}


object Ctx {

  private def local = new ThreadLocal[Ctx] {
    override def initialValue = new Default
  }

  implicit def current(): Ctx = Thread.currentThread match {
    case _ => local.get
  }

  final class Default extends Ctx {
    private val queue = mutable.PriorityQueue[Deferrable]()
    def defer(d: Deferrable) = queue += d
    def flush {
      while (queue.nonEmpty) {
        val d = queue.dequeue()
        d.execute(this)
      }
    }
  }

}