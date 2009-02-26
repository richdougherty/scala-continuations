package scala.continuations.async

import scala.collection.immutable.Queue
import scala.continuations._
import scala.continuations.ControlContext._

/**
 * An asynchronous lock implementation.
 */
class ALock {

  // TODO: Should we parameterise the Suspender?

  sealed trait State
  case object Unlocked extends State
  case class Locked(q: Queue[Suspendable[Unit]]) extends State

  /**
   * The internal state.
   */
  private var state: State = Unlocked

  def lock = {
    ActorSuspender.shiftSuspendable { (suspendable: Suspendable[Unit]) =>
      synchronized {
        state match {
          case Unlocked => {
            state = Locked(Queue.Empty)
            //println("lock: was unlocked, returning")
            suspendable.transfer(())
          }
          case Locked(q) => {
            state = Locked(q.enqueue(suspendable))
            //println("lock: was locked, queueing and suspending")
            suspendable.suspend
          }
        }
      }
    }
  }

  def tryLock = synchronized {
    state match {
      case Unlocked => {
        state = Locked(Queue.Empty)
        true
      }
      case Locked(q) => {
        false
      }
    }
  }

  def unlock: Unit = synchronized {
    state match {
      case Locked(q) => {
        if (q.isEmpty) {
          state = Unlocked
          //println("unlock: nothing queued")
        } else {
          val (suspendable, newQ) = q.dequeue
          state = Locked(newQ)
          //println("unlock: resuming queued task")
          suspendable.resume(())
        }
      }
      case s => throw new IllegalStateException(s.toString)
    }
  }

}
