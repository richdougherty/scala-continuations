// $Id$

import scala.continuations._
import scala.continuations.CPS._

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


final class Mailbox[A](val name: String) {
  val values = new Queue[A]()
  val rules = new Queue[A=>Any]()
  
  def get(): A @cpstypes[Any,Any]= {
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

  def stop(): Any @cpstypes[Any,Any] = shift((k:Any=>Any)=>())

  def testCode():Unit = {
    val ping = new Mailbox[String]("ping")
    val pong = new Mailbox[String]("pong")

    val max = 1*1000*1000
    val step = max/10
    
    def pingActor(i: Int):Any @cpstypes[Any,Any] = {
      pong.put("ping")
      ping.get()

      if (i%step == 0)
        println("Ping: " + i)

      if (i < max)
        pingActor(i+1)
      else
	()
    }
    
    def pongActor():Any @cpstypes[Any,Any] = {
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
    println("dt: "+(t1-t0))
    
    /*
      expect output:
      """
      parallel: put(Blabla) & get()
      inside rule body...
      => result: ((),Blabla)
      """
    */
  }
}
