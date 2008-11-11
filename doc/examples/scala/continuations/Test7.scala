import scala.continuations._
import scala.continuations.CPS._

import scala.collection.mutable._

final class FutureCell[A] {
  var value: A = _
  var hasVal: Boolean = false
  val handlers = new Queue[A=>Any]()
  
  def get(): A @cpstypes[Any,Any]= {
    shift((k:(A => Any)) =>
      if (hasVal) {
        val v = value
//        TaskScheduler.schedule(() => k(v))
	k(v)
      }
      else
        handlers += k
    )
  }
  
  def set(v: A) {
    if (hasVal)
      throw new Exception("cannot assign twice to single-assignment variable")

    value = v
    hasVal = true

    while (!handlers.isEmpty) {
      val k = handlers.dequeue()
//      TaskScheduler.schedule(() => k(v))
      k(v)
    }
  }
}

object Test7 {


  abstract class DStream[A] {
    
    // FIXME: cannot have abstract cps methods yet

    def isEmpty: Boolean  = true

    def head: A = 
      throw new Exception("head of empty stream")

    def tail: DStream[A] @cpstypes[Any,Any] = 
      throw new Exception("tail of empty stream")

    def tail_=(x: =>(DStream[A] @cpstypes[Any,Any])): Unit = 
      throw new Exception("tail of empty stream")

    def filter(p: (A=>Boolean)): DStream[A] @cpstypes[Any,Any] = this
  }

  case class DNil[A] extends DStream[A] {

  }

  case class DCons[A](override val head: A) extends DStream[A] {

    val tl = new FutureCell[DStream[A]]

    override def tail = tl.get()

    override def tail_=(x: =>(DStream[A] @cpstypes[Any,Any])) = 
      reset[Any,Any](tl.set(x))

    override def isEmpty = false

    override def filter(p: (A=>Boolean)) = {

      if (p(head)) {

	val xs = DCons(head)
	xs.tail = tail.filter(p)
	shiftUnit(xs) // implicit conversion, should be 
      }               // inserted by the compiler
      else
	tail.filter(p)
    }
  }

  def produceRecursive(n: Int, max: Int): DStream[Int] = { 
    println("produce " + n)

    if (n > max)
      DNil[Int]
    else {
      val xs = DCons[Int](n)
      xs.tail = produceRecursive(n + 1, max)
      xs
    }
  }

  def produceAppending(xs: DStream[Int], n: Int, max: Int): Unit = { 
    println("produce " + n)

    if (n > max)
      xs.tail = DNil[Int]
    else {
      val ys = DCons[Int](n)
      xs.tail = ys
      produceAppending(ys, n + 1, max)
    }
  }


  def sieve(xs: DStream[Int]): DStream[Int] @cpstypes[Any, Any] = {

    if (!xs.isEmpty) {

      val zs = DCons(xs.head)
      zs.tail = sieve(xs.tail.filter(_ % xs.head != 0))
      shiftUnit(zs) // implicit conversion, should be
    } else {        // inserted by the compiler
      xs
    }
  }
  
   
  def consume(xs: DStream[Int]): Unit @cpstypes[Any, Any] = {

    if (!xs.isEmpty) {
      println(xs.head)
      consume(xs.tail)
    } else {
      ()
    }
  }

  def main(args: Array[String]) = {

    val xs = DCons(2) //produceRecursive(2, 20)
    
    reset {
      consume(sieve(xs))
    }


    produceAppending(xs,2,20)


//    TaskScheduler.execAll()

  }






}
