package org.reactress






final class TopoQueue {
  var levels = new Array[TopoQueue.Level](4)
  var infinity = new TopoQueue.Level
}


object TopoQueue {

  class Level {
    var elems = new Array[AnyRef](8)
  }

}
