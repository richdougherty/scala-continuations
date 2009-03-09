// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.testing.Benchmark

trait Test18Samefringe {

  abstract case class Tree[+A]()
  case class Node[+A](left: Tree[A], right: Tree[A]) extends Tree[A]()
  case class Leaf[+A](value: A) extends Tree[A]()
  
  def toIterator[A](t: Tree[A]): Iterator[A] = t match {
    case Node(left, right) => toIterator(left) ++ toIterator(right)
    case Leaf(x) => Iterator.single(x)
  }

  
  def toList0[A](t: Tree[A], l: List[A]): List[A] = t match {
    case Node(left, right) => toList0(left, toList0(right, l))
    case Leaf(x) => x::l
  }

  def toList1[A](t: Tree[A]): List[A] = toList0(t, Nil)

  def toList2[A](t: Tree[A]): List[A] = t match {
    case Node(left, right) => toList2(left):::toList2(right)
    case Leaf(x) => List(x)
  }

  
  def toStream0[A](t: Tree[A], l: =>Stream[A]): Stream[A] = t match {
    case Node(left, right) => toStream0(left, toStream0(right, l))
    case Leaf(x) => Stream.cons(x,l)
  }

  def toStream1[A](t: Tree[A]): Stream[A] = toStream0(t, Stream.empty)
  
  def toStream2[A](t: Tree[A]): Stream[A] = t match {
    case Node(left, right) => toStream2(left).append(toStream2(right))
    case Leaf(x) => Stream.cons(x,Stream.empty)
  }


  def toCont1[A](t: Tree[A])(handler: (A, Unit=>Unit)=>Unit): Unit @cps[Unit,Unit] = t match {
    case Node(left, right) => toCont1(left)(handler); toCont1(right)(handler)
    case Leaf(x) => shift { k:(Unit=>Unit) => handler(x,k) }
  }

  def toCont2[A](t: Tree[A])(handler: A=>Unit @cps[Unit,Unit]): Unit @cps[Unit,Unit] = t match {
    case Node(left, right) => toCont2(left)(handler); toCont2(right)(handler)
    case Leaf(x) => shift { k:(Unit=>Unit) => reset { handler(x); k() } }
  }


  def buildLargeTree(start: Int, end: Int): Tree[Int] = {
    if (start < end)
      Node(buildLargeTree(start, start + (end - start) / 2), buildLargeTree(start + (end - start) / 2 + 1, end))
    else
      Leaf(end)
  }

  def testListX(t1: Tree[Int], t2: Tree[Int]) = {
    val i1 = toList1(t1)
    val i2 = toList1(t2)
    
    val same = i1 == i2

    println("as list")
    println(same)
  }



}

abstract class Test18Bench extends Benchmark with Test18Samefringe {
  val size = Integer.parseInt(System.getProperty("size", (1<<8).toString))
  val tree = buildLargeTree(0, size)
  override def prefix = super.prefix + "(" + size +" items)"
  def test(t1: Tree[Int], t2: Tree[Int])
  def run() = test(tree,tree)
}

object Test18Iter extends Test18Bench {
  def test(t1: Tree[Int], t2: Tree[Int]) = {
    val i1 = toIterator(t1)
    val i2 = toIterator(t2)
    
    var same = true
    while (i1.hasNext && i2.hasNext && same) {
      if (i1.next != i2.next)
        same = false
    }
    if (i1.hasNext || i2.hasNext)
      same = false
    
    println("as iterator")
    println(same)
  }
}

object Test18List1 extends Test18Bench {
  def test(t1: Tree[Int], t2: Tree[Int]) = {
    var i1 = toList1(t1)
    var i2 = toList1(t2)
    
    var same = true
    while (!i1.isEmpty && !i2.isEmpty) {
      if (i1.head != i2.head) {
        same = false
        i1 = Nil
      } else {
        i1 = i1.tail
        i2 = i2.tail
      }
    }
    if (!i1.isEmpty || !i2.isEmpty)
      same = false
    
    println("as list (using cons)")
    println(same)
  }
}

object Test18List2 extends Test18Bench {
  def test(t1: Tree[Int], t2: Tree[Int]) = {
    var i1 = toList2(t1)
    var i2 = toList2(t2)
    
    var same = true
    while (!i1.isEmpty && !i2.isEmpty) {
      if (i1.head != i2.head) {
        same = false
        i1 = Nil
      } else {
        i1 = i1.tail
        i2 = i2.tail
      }
    }
    if (!i1.isEmpty || !i2.isEmpty)
      same = false
    
    println("as list (using append)")
    println(same)
  }
}

object Test18Stream1 extends Test18Bench {
  def test(t1: Tree[Int], t2: Tree[Int]) = {
    var i1 = toStream1(t1)
    var i2 = toStream1(t2)

    var same = true
    while (!i1.isEmpty && !i2.isEmpty) {
      if (i1.head != i2.head) {
        same = false
        i1 = Stream.empty
      } else {
        i1 = i1.tail
        i2 = i2.tail
      }
    }
    if (!i1.isEmpty || !i2.isEmpty)
      same = false

    println("as stream (using cons)")
    println(same)
  }
}

object Test18Stream2 extends Test18Bench {
  def test(t1: Tree[Int], t2: Tree[Int]) = {
    var i1 = toStream2(t1)
    var i2 = toStream2(t2)

    var same = true
    while (!i1.isEmpty && !i2.isEmpty) {
      if (i1.head != i2.head) {
        same = false
        i1 = Stream.empty
      } else {
        i1 = i1.tail
        i2 = i2.tail
      }
    }
    if (!i1.isEmpty || !i2.isEmpty)
      same = false

    println("as stream (using append)")
    println(same)
  }
}

object Test18Cont2 extends Test18Bench {
  def test(t1: Tree[Int], t2: Tree[Int]) = {
    
    var v1:Int = 0
    var v2:Int = 0
    var k1: Unit=>Unit = null
    var k2: Unit=>Unit = null
    
    reset {
      toCont2(t1) { x => 
        shift { k:(Unit=>Unit) => k1 = k; v1 = x }
      }
      k1 = null
    }

    reset {
      toCont2(t2) { x => 
        shift { k:(Unit=>Unit) => k2 = k; v2 = x }
      }
      k2 = null
    }

    var same = true
    while (k1 != null && k2 != null) {
      if (v1 != v2) {
        same = false
        k1 = null
      } else {
        k1()
        k2()
      }
    }

    if (k1 != null || k2 != null)
      same = false

    println("shift/reset (2)")
    println(same)
  }
}

object Test18Cont1 extends Test18Bench {
  def test(t1: Tree[Int], t2: Tree[Int]) = {
    
    var v1:Int = 0
    var v2:Int = 0
    var k1: Unit=>Unit = null
    var k2: Unit=>Unit = null
    
    reset {
      toCont1(t1) { (x,k) => 
        k1 = k; v1 = x
      }
      k1 = null
    }

    reset {
      toCont1(t2) { (x,k) => 
        k2 = k; v2 = x
      }
      k2 = null
    }

    var same = true
    while (k1 != null && k2 != null) {
      if (v1 != v2) {
        same = false
        k1 = null
      } else {
        k1()
        k2()
      }
    }

    if (k1 != null || k2 != null)
      same = false

    println("shift/reset (1)")
    println(same)
  }
}


object Test18Samefringe extends Test18Samefringe {

  def testAll(t1: Tree[Int], t2: Tree[Int]) = {
  }

  def main(args: Array[String]) {

    val t1 = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
    val t2 = Node(Leaf(1), Node(Leaf(2), Leaf(3)))

    val t3 = Leaf(1)
    val t4 = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))

    testAll(t1,t2)
    println("-----")
    testAll(buildLargeTree(0,8),buildLargeTree(0,8))
    testAll(buildLargeTree(0,4),buildLargeTree(0,8))
    testAll(buildLargeTree(0,8),buildLargeTree(0,4))
    
    println("done")
    
  }
  
}
