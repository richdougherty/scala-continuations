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
  val rules = new Queue[A=>Unit]()
  
  @cps def get() = {
    shift((k:(A => Unit)) =>
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

  @cps def stop() = shift((k:Unit=>Unit)=>())

  def spawn[A](x: A) = reset[A, Unit](x);

  def testCode() {
    val ping = new Mailbox[String]("ping")
    val pong = new Mailbox[String]("pong")

    val max = 1*1000*1000
    val step = max/10
    
    @cps def pingActor(i: Int): Unit = {
      pong.put("ping")
      val x = ping.get()

      if (i%step == 0)
        println("Ping: " + i)

      if (i < max)
        pingActor(i+1)
      else
        stop()
    }
    
    @cps def pongActor(): Unit = {
      val x = pong.get()

//      println("Pong: " + x)

      ping.put("pong")
      pongActor()
    }
    
    spawn(pingActor(1))
    spawn(pongActor())
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
