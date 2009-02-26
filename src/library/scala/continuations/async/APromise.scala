package scala.continuations.async

import scala.collection.immutable.Queue
import scala.continuations._
import scala.continuations.ControlContext._

/**
 * An asynchronous promise.
 */
trait APromise[A] extends AFuture[A] {

  private sealed trait State
  private case class Unset(pending: Queue[Suspendable[A]]) extends State
  private case class Set(result: Either[Throwable,A]) extends State
  
  private var state: State = Unset(Queue.Empty)

  def apply = {
    ActorSuspender.shiftSuspendable { (suspendable: Suspendable[A]) =>
      synchronized {
        state match {
          case Unset(pending) => {
            state = Unset(pending.enqueue(suspendable))
            suspendable.suspend
          }
          case Set(Right(x)) => {
            suspendable.transfer(x.asInstanceOf[A]) // Work around type erasure
          }
          case Set(Left(t)) => {
            suspendable.transferWithError(t)
          }
        }
      }
    }
  }

  def set(result: Either[Throwable,A]): Unit = synchronized {
    state match {
      case Unset(pending) => {
        state = Set(result)
        for (suspendable <- pending) {
          result match {
            case Right(x) => suspendable.resume(x.asInstanceOf[A]) // Work around type erasure
            case Left(t) => suspendable.resumeWithError(t)
          }
        }
      }
      case s => throw new IllegalStateException(s.toString)
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
      case Set(Right(x)) => Some(x.asInstanceOf[A]) // Work around type erasure
      case Set(Left(t)) => throw t
    }
  }
}
