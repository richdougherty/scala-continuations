// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.concurrent._

import java.util.Timer
import java.util.TimerTask

object Test14 {

  def loopWhile[T](cond: =>Boolean)(body: =>(Unit @cps[T,T])): Unit @cps[T,T] = {
    if (cond) {
      body
      loopWhile[T](cond)(body)
    } else 
      ()
  }

  val timer = new Timer()

  def sleep(delay: Int) = shift { k: (Unit => Unit) =>
    timer.schedule(new TimerTask {
      def run = k()
    }, delay)
  }

  def main(args: Array[String]) {

    var count = 15
    reset {
      println("start")
      loopWhile(count != 0) {
        sleep(500)
        println("the time is: " + System.currentTimeMillis())
        count -= 1
      }
      println("end")
      timer.cancel()
    }
    
  }
  
}
