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

object TestAsync0 {

  def main(args: Array[String]) {
    // TODO: Come up with something more robust than Thread.sleep() for coordinating actors

    val lock = new ALock
    val finished = new Semaphore(0, true)

    def testLock(id: String) = {
      def lockUnlock = {
        lock.lock
        println(id + ": got lock")
        Thread.sleep(300)
        lock.unlock
      }

      Actor.actor {
        reset {
          lockUnlock
          lockUnlock
          finished.release
        }
      }
    }

    testLock("1")
    Thread.sleep(100)
    testLock("2")
    Thread.sleep(100)
    testLock("3")

    finished.acquire
    finished.acquire
    finished.acquire
  }
  
}
