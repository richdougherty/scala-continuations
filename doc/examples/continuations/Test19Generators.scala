// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.testing.Benchmark

import java.math.BigInteger

object Test19Fib extends Benchmark {
  
  private[this] final def fibIter(i: BigInteger, j: BigInteger): Unit @cps[Unit,Unit] = {
    shift { k: (Unit=>Unit) =>
      cur = j
      next = k
    }
    fibIter(j, i.add(j))
  }

  var cur: BigInteger = _
  var next: Unit=>Unit = _

  val size = Integer.parseInt(System.getProperty("size", "1000"))
  override def prefix = super.prefix + "(" + size + ")"


  def run() = {

    reset {
      fibIter(BigInteger.ZERO, BigInteger.ONE)
    }

    var i = 0
    while (i < size) {
      next()
      i += 1
    }
  }

}
  
object Test19Lin extends Benchmark {

  private[this] final def linIter(i: Int): Unit @cps[Unit,Unit] = {
    shift { k: (Unit=>Unit) =>
      cur = i
      next = k
    }
    linIter(i + 1)
  }
 
  var cur: Int = _
  var next: Unit=>Unit = _

  val size = Integer.parseInt(System.getProperty("size", "1000"))
  override def prefix = super.prefix + "(" + size + ")"


  def run() = {

    reset {
      linIter(0)
    }

    var i = 0
    while (i < size) {
      next()
      i += 1
    }
  }

}
 
object Test19Tree extends Benchmark {

  private[this] final def treeIter(start: Int, end: Int): Unit @cps[Unit,Unit] = {
      if (start < end - 1) {
          treeIter(start, start + (end - start)/2)
          treeIter(start + (end - start)/2, end)
      } else if (start < end) {
        shift { k: (Unit=>Unit) =>
          cur = start
          next = k
        }
      } else {
      }
  }

  var cur: Int = _
  var next: Unit=>Unit = _
  
  val size = Integer.parseInt(System.getProperty("size", "1000"))
  override def prefix = super.prefix + "(" + size + ")"
  
  
  def run() = {
    
    reset {
      treeIter(0, 2*size)
    }
    
    var i = 0
    while (i < size) {
      next()
      i += 1
    }
  }

}

object Test19Generators {
}