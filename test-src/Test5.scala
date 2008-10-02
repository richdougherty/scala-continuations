// $Id$

import scala.continuations._
import scala.continuations.CPS._

import scala.collection.mutable._

object Test5 {

/*  
  implicit def delay[A](a: =>Context[A]) = {
    object Delayed {
      def &[B](b: =>Context[B]) = parallel(a,b)
    }
    Delayed
  }
*/
  
  def parallel[A,B](a: =>(A @cpstypes[Any,Any]), 
		    b: =>(B @cpstypes[Any,Any])): Pair[A,B] @cpstypes[Any,Any] = {

      println("BEGIN")

      val u = new Mailbox[A]("u")
      val v = new Mailbox[B]("v")

      reset[Any,Any](u.put(a))
      reset[Any,Any](v.put(b))

      val x = u.get()
      val y = v.get()

      println("END")
      (x,y)
  }

  def testCode(): Any @cpstypes[Any,Any] = {
    val ping = new Mailbox[String]("ping")
    val pong = new Mailbox[String]("pong")

    println("before parallel")

    val z = parallel({
        println("before get")
        val x = ping.get()
        println("after get: " + x)
        x
    },{
        println("before put")
        ping.put("secret")
        println("after put")
    })
    
    println("after parallel: " + z)
  }

  def main(args: Array[String]) {

    val t0 = java.lang.System.currentTimeMillis();

    reset[Any,Any](testCode())

    TaskScheduler.execAll()
    
    val t1 = java.lang.System.currentTimeMillis();
    println("dt: "+(t1-t0))
    
    /*
      expect output:
      """
      before parallel
      BEGIN
      before get
      before put
      after get: secret
      after put
      END
      after parallel: (secret,0)
      """
    */
  }
}
