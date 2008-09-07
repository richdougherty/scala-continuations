// $Id$

import scala.continuations._
import scala.continuations.CPS._

import scala.collection.mutable._

object Test5 {

  @cps def stop() = shift((k:Any=>Any)=>())
  @cps def continue(x:Any) = shift((k:Any=>Any)=>k(x))

/*  
  implicit def delay[A](a: =>Context[A]) = {
    object Delayed {
      def &[B](b: =>Context[B]) = parallel(a,b)
    }
    Delayed
  }
*/
  
  @cps def parallel[A,B](a: =>Shift[A,Any,Any], b: =>Shift[B,Any,Any]) = {
      println("BEGIN")

      val u = new Mailbox[A]("u")
      val v = new Mailbox[B]("v")

      reset(a.map((x:A) => u.put(x)))
      reset(b.map((y:B) => v.put(y)))

      val x:A = u.get()
      val y:B = v.get()

      println("END")
      (x,y)
  }

  @cps def testCode(): Any = {
    val ping = new Mailbox[String]("ping")
    val pong = new Mailbox[String]("pong")

    println("before parallel")

    val z = parallel({
        println("before get")
        val x:String = ping.get() // FIXME: need the type here, so implicit gets inserted!!!
        println("after get: " + x)
        x
    },{
        println("before put")
        ping.put("secret")
        println("after put")
        continue(0) // FIXME: would rather not have to use 'continue' and just write
                    // down the return val instead
    })
    
    println("after parallel: " + z)
    stop()
  }

  def main(args: Array[String]) {

    val t0 = java.lang.System.currentTimeMillis();

    reset(testCode())

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
