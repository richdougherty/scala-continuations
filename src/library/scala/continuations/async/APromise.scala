package scala.continuations.async

import scala.collection.immutable.Queue
import scala.continuations._
import scala.continuations.ControlContext._

/**
 * An asynchronous promise.
 */
class APromise[A] extends AFuture[A] {

  private sealed trait State
  private case class Unset(pending: Queue[Suspendable[A]]) extends State
  private case class Set(result: Either[Throwable,A]) extends State
  
  private var state: State = Unset(Queue.Empty)

  def apply: A @cps[Unit,Nothing] = {
    ActorSuspender.shiftSuspendable { (suspendable: Suspendable[A]) =>
      //println(scala.actors.Actor.self + ": promise.apply: before sync")
      synchronized {
        state match {
          case Unset(pending) => {
            //println(scala.actors.Actor.self + ": promise.apply: suspending until value ready")
            state = Unset(pending.enqueue(suspendable))
            suspendable.suspend
          }
          case Set(result) => {
            //println(scala.actors.Actor.self + ": promise.apply: value already set")
            suspendable.transferWithResult(result)
          }
        }
      }
    }
  }

  def set(x: A): Unit = setResult(Right(x))

  def setResult(result: Either[Throwable,A]): Unit = {
    //println(scala.actors.Actor.self + ": promise.setResult: before sync")
    synchronized {
    state match {
      case Unset(pending) => {
        //println(scala.actors.Actor.self + ": promise.setResult: setting")
        state = Set(result)
        for (suspendable <- pending) {
          //println(scala.actors.Actor.self + ": promise.setResult: resuming")
          suspendable.resumeWithResult(result)
        }
      }
      case s => throw new IllegalStateException(s.toString)
    }
  }
  }

  def isSet: Boolean = synchronized {
    state match {
      case Unset(pending) => false
      case Set(value) => true
    }
  }
  
  def immediate: Option[A] = synchronized {
    state match {
      case Unset(_) => None
      case Set(result) => Some(Result.invokeResult(result))
    }
  }
}
