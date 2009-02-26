// $Id$

package examples.continuations

import java.util.concurrent.Semaphore
import scala.actors.Actor
import scala.collection.immutable.Queue
import scala.concurrent._
import scala.continuations._
import scala.continuations.ControlContext._

import scala.actors.Actor
import scala.continuations.async._
import scala.continuations.ControlContext._
import java.util.concurrent.Semaphore

object TestAsync1 {

  def main(args: Array[String]) {
    // TODO: Come up with something more robust than Thread.sleep() for coordinating actors

    val finished = new Semaphore(0, true)

    val p = new APromise[String]
    
    Actor.actor {
      reset {
        assert(!p.isSet)
        assert(p.apply == "hello")
        assert(p.isSet)
        println("got value from promise")
        finished.release
      }
    }

    Actor.actor {
      reset {
        assert(!p.isSet)
        Thread.sleep(100)
        assert(!p.isSet)
        println("setting promise")
        p.set("hello")
        assert(p.apply == "hello")
        assert(p.isSet)
        finished.release
      }
    }

    finished.acquire
    finished.acquire
  }
  
}
