package org.reactress
package container



import scala.reflect.ClassTag



class ReactTileMap[@spec(Byte, Int, Long, Double) T: ClassTag](
  size0: Int,
  default0: T
) extends Reactive.Source[Mux3[Int, Int, T]] {
  import ReactTileMap._

  private var sz = nextPow2(size0)
  private var dflt = default0
  private var root: Node[T] = new Node.Leaf(dflt)
  private var clearsource = new Reactive.Source[Mux0] {}

  private[reactress] def quadRoot = root

  def size = sz
  
  def default = dflt

  def apply(x: Int, y: Int) = root.apply(x, y, size)

  def update(x: Int, y: Int, elem: T) = {
    assert(x >= 0 && x < size && y >= 0 && y < size)
    root = root.update(x, y, size, implicitly[ClassTag[T]], default, elem)
    mux.dispatch(this, x, y, elem)
  }
  
  def clearAtomic() = {
    root = new Node.Leaf(dflt)
    clearsource.mux.dispatch(this)
  }

}


object ReactTileMap {

  private def nextPow2(num: Int): Int = {
    var v = num - 1
    v |= v >> 1
    v |= v >> 2
    v |= v >> 4
    v |= v >> 8
    v |= v >> 16
    v + 1
  }

  final def matrixSize = 4

  abstract class Node[@spec(Byte, Int, Long, Double) T] {
    def apply(x: Int, y: Int, sz: Int): T
    def update(x: Int, y: Int, sz: Int, tag: ClassTag[T], d: T, elem: T): Node[T]
    def isLeaf: Boolean = false
    def asLeaf: Node.Leaf[T] = ???
  }

  object Node {
    final case class Leaf[@spec(Byte, Int, Long, Double) T](val element: T) extends Node[T] {
      def apply(x: Int, y: Int, sz: Int) = element
      def update(x: Int, y: Int, sz: Int, tag: ClassTag[T], d: T, elem: T) = if (element == elem) this else {
        if (sz > matrixSize) {
          val fork = new Fork[T](
            new Leaf(element),
            new Leaf(element),
            new Leaf(element),
            new Leaf(element)
          )
          fork.update(x, y, sz, tag, d, elem)
        } else {
          val matrix = new Matrix[T](tag.newArray(matrixSize * matrixSize))
          matrix.fill(d)
          matrix.update(x, y, sz, tag, d, elem)
        }
      }
      override def isLeaf = true
      override def asLeaf = this.asInstanceOf[Leaf[T]]
    }

    final case class Fork[@spec(Byte, Int, Long, Double) T](
      var nw: Node[T],
      var ne: Node[T],
      var sw: Node[T],
      var se: Node[T]
    ) extends Node[T] {
      def apply(x: Int, y: Int, sz: Int) = {
        val xmid = sz / 2
        val ymid = sz / 2
        if (x < xmid) {
          if (y < ymid) sw.apply(x, y, sz / 2)
          else nw.apply(x, y - ymid, sz / 2)
        } else {
          if (y < ymid) se.apply(x - xmid, y, sz / 2)
          else ne.apply(x - xmid, y - ymid, sz / 2)
        }
      }
      def update(x: Int, y: Int, sz: Int, tag: ClassTag[T], d: T, elem: T) = {
        val xmid = sz / 2
        val ymid = sz / 2

        if (x < xmid) {
          if (y < ymid) sw = sw.update(x, y, sz / 2, tag, d, elem)
          else nw = nw.update(x, y - ymid, sz / 2, tag, d, elem)
        } else {
          if (y < ymid) se = se.update(x - xmid, y, sz / 2, tag, d, elem)
          else ne = ne.update(x - xmid, y - ymid, sz / 2, tag, d, elem)
        }

        if (nw.isLeaf && ne.isLeaf && sw.isLeaf && se.isLeaf) {
          val nwe = nw.asLeaf.element
          val nee = ne.asLeaf.element
          val swe = sw.asLeaf.element
          val see = se.asLeaf.element
          if (nwe == nee && nwe == swe && nwe == see) new Leaf(nwe)
          else this
        } else this
      }
    }

    final case class Matrix[@spec(Byte, Int, Long, Double) T](val array: Array[T]) extends Node[T] {
      def apply(x: Int, y: Int, sz: Int) = {
        array(y * matrixSize + x)
      }
      private def allSame(elem: T): Boolean = {
        var i = 1
        while (i < matrixSize * matrixSize) {
          if (elem != array(i)) return false
          i += 1
        }
        true
      }
      def fill(elem: T) {
        var i = 0
        while (i < array.length) {
          array(i) = elem
          i += 1
        }
      }
      def update(x: Int, y: Int, sz: Int, tag: ClassTag[T], d: T, elem: T) = {
        array(y * matrixSize + x) = elem

        if (allSame(array(0))) new Leaf(elem)
        else this
      }
    }
  }

}














