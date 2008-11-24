// $Id$

package scala.continuations

import scala.collection.mutable._


object TaskScheduler {
  val runq = new Queue[()=>Unit]()
  
  val maxNest = 15
  var curNest = 0
  
  def schedule(f:(()=>Unit)) {
    if (curNest < maxNest) {
      curNest += 1
      f();
    } else {
      curNest = 0
      runq += f
    }
  }
  
  def execAll() {
    while(!runq.isEmpty) {
      val k = runq.dequeue();
      k()
    }
  }
  
}
