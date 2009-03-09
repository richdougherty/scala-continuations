// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.collection.mutable._

import java.util.concurrent.locks._

import scala.concurrent._
import scala.concurrent.cpsops._


final class MailboxB[A](val name: String)(implicit private[this] val sched: AbstractTaskRunner) {
  private[this] val values = new Queue[A]()
  private[this] val rules = new Queue[A=>Any]()
  private[this] val lock = new ReentrantLock()
  
  def get(): A @cps[Any,Any] = {
    shift { k:(A => Any) =>
      lock.lock()
      if (!values.isEmpty) {
        val v = values.dequeue()
        lock.unlock()
//      k(v)
        sched.submitTask(() => k(v))
      } else {
        rules += k
        lock.unlock()
      }
    }
  }
  
  def put(v: A): Unit = {
    lock.lock()
    if (!rules.isEmpty) {
      val k = rules.dequeue()
      lock.unlock()
      sched.submitTask(() => k(v))
    } else {
      values += v
      lock.unlock()
    }
  }
}




object Test4b extends FJTaskRunners {

  def stop(): Any @cps[Any,Any] = shift((k:Any=>Any)=>())

  def testCode() = {
    val ping = new MailboxB[String]("ping")
    val pong = new MailboxB[String]("pong")

    val max = Integer.parseInt(System.getProperty("size", "1000000"))
    val step = max/10
    
    println("test4b.max: " + max)
    
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
    
    spawn(pongActor())
    pingActor(0)
  }

  def main(args: Array[String]) {

    val t0 = java.lang.System.currentTimeMillis();

    spawn {
      testCode()
      val t1 = java.lang.System.currentTimeMillis()
      if (!args.contains("jvm")) println("dt: "+(t1-t0))
      System.exit(0)
    }

    mainTaskRunner.waitUntilFinished()
    
  }
}
