// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.collection.mutable._



final class Mailbox[A](val name: String) {
  val values = new Queue[A]()
  val rules = new Queue[A=>Any]()
  
  def get(): A @cps[Any,Any]= {
    shift((k:(A => Any)) =>
      if (!values.isEmpty) {
        val v = values.dequeue()
        TaskScheduler.schedule(() => k(v))
      }
      else
        rules += k
    )
  }
  
  def put(v: A) {
    if (!rules.isEmpty) {
      val k = rules.dequeue()
      TaskScheduler.schedule(() => k(v))
    }
    else
      values += v
  }
}




object Test4 {

  def stop(): Any @cps[Any,Any] = shift((k:Any=>Any)=>())

  def testCode():Unit = {
    val ping = new Mailbox[String]("ping")
    val pong = new Mailbox[String]("pong")

    val max = Integer.parseInt(System.getProperty("size", "1000000"))
    val step = max/10
    
    println("max: " + max)
    
    def pingActor(i: Int):Any @cps[Any,Any] = {
      pong.put("ping")
      ping.get()

      if (i%step == 0)
        println("Ping: " + i)

      if (i < max)
        pingActor(i+1)
      else
	      shiftUnit(())
    }
    
    def pongActor():Any @cps[Any,Any] = {
      val x = pong.get()

//    println("Pong: " + x)

      ping.put("pong")
      pongActor()
    }
    
    reset[Any,Any](pingActor(1))
    reset[Any,Any](pongActor())
  }

  def main(args: Array[String]) {

    val t0 = java.lang.System.currentTimeMillis();

    testCode()

    TaskScheduler.execAll()

    val t1 = java.lang.System.currentTimeMillis();
    if (!args.contains("jvm")) println("dt: "+(t1-t0))
    
  }
}
