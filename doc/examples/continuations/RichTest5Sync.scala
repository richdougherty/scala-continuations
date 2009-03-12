// $Id$

package examples.continuations

import java.util.concurrent.Semaphore
import scala.actors.Actor
import scala.continuations._
import scala.continuations.async._
import scala.continuations.ControlContext._
import scala.io.channels._

object RichTest4Sync {

  def main(args: Array[String]) {
    val finished = new Semaphore(0, true)

    Actor.actor {
      val lock = new ALock

      def printLockState: Unit = {
        if (lock.tryLock) {
          println("-> Unlocked")
          lock.unlock // Restore state
        } else {
          println("-> Locked")
        }
      }
      def fauxShift: Unit @suspendable = {
        shift((ret: Unit => Unit) => ret(()))
      }

      reset {
        Actor.self // Force Channel.recv to work properly!
        printLockState
        println("Locking")
        lock.lock
        printLockState
        println("Unlocking")
        lock.unlock
        printLockState
        println("Entering sync block")
        val x = lock.sync {
          fauxShift
          printLockState
          12
        }
        println("Exited sync block")
        printLockState
        println("Entering sync block")
        lock.sync {
          fauxShift
          printLockState          
        }
        println("Exited sync block")
        finished.release
      }
    }

    finished.acquire
  }

  
}
