package scala.io.iteratee

import scala.collections.immutable.Binary
import scala.collection.immutable.Queue
import scala.continuations._
import scala.continuations.async._
import scala.continuations.ControlContext._

import Iteratee._

class IterateeReader {

  private sealed trait State
  private case class Open(tailRef: IterateeTailRef[Binary,Byte]) extends State
  private case class Running(tailRef: IterateeTailRef[Binary,Byte], pending: Queue[Suspendable[IterateeTailRef[Binary,Byte]]]) extends State
  private case object Closed extends State

  private val stateLock = new ALock
  private var state: State = Open(new IterateeTailRef[Binary,Byte](IEDone((), Chunk(Binary.empty))))

  def tail = synchronized {
    state match {
      case Open(tailRef) => tailRef.promise
      case Running(tailRef, _) => tailRef.promise
      case _ => throw new IllegalStateException(state.toString)
    }
  }

  private def bind0[A](f: IterateeTailRef[Binary,Byte] => (A @suspendable)): A @suspendable = {
    //println(scala.actors.Actor.self + ": IterateeReader.bind0")
    val tailRef = ActorSuspender.shiftSuspendable { suspendable: Suspendable[IterateeTailRef[Binary,Byte]] =>
      synchronized {
        state match {
          case Open(tailRef) => {
            // FIXME: Use destructuring assignment once supported.
            state = Running(tailRef, Queue.Empty)
            suspendable.resume(tailRef)
          }
          case Running(tailRef, pending) => {
            state = Running(tailRef, pending.enqueue(suspendable))
          }
          case other => suspendable.resumeWithResult(Left(new IllegalStateException(other.toString))) // XXX: Friendlier exception?
        }
      }
      suspendable.suspend
    }
    f(tailRef)
  }
  
  // XXX: Better name?
  def bind[A](sub: Iteratee[Binary,Byte,A]): A @suspendable = bind0 { tailRef: IterateeTailRef[Binary,Byte] =>
    val tuple = tailRef.bind(sub)
    val (resultIteratee, nextTailRef) = tuple
    resultIteratee match {
      case d @ IEDone(_, err@StreamError(t)) => {
        //println(scala.actors.Actor.self + ": IterateeReader.bind: got IEDone(_, err)")
        synchronized {
          state = Closed
        }
        nextTailRef.promise.set(IEDone((), err)) // Close underlying iteratee
        throw t
      }
      case d @ IEDone(x, _) => {
        //println(scala.actors.Actor.self + ": IterateeReader.bind: got IEDone(x, _)")
        synchronized {
          state match {
            case Running(tailRef, pending) => {
              if (pending.isEmpty) {
                state = Open(nextTailRef)
              } else {
                val (suspendable, newPending) = pending.dequeue
                state = Running(tailRef, newPending)
                suspendable.resume(nextTailRef)
              }
            }
            case other => throw new IllegalStateException(other.toString)
          }
        }
        x
      }
      case _ => {
        //println(scala.actors.Actor.self + ": IterateeReader.bind: got _")
        synchronized {
          state = Closed
        }
        val e = new Exception("Invalid result iteratee: " + resultIteratee)
        nextTailRef.promise.set(IEDone((), StreamError(e))) // Close underlying iteratee
        throw e
      }
    }
  }

  def readByte: Option[Byte] @suspendable = bind(snext[Binary,Byte])

  def readAll: Binary @suspendable = bind(join[Binary,Byte])

  //def readLine(charset: String): String @suspendable = bind(breakMatch(newLineMatcher).map((line: Binary, newline: Option[Binary]) => line.decodeString(charSet)))

  def readClose: Unit @suspendable =  bind0 { tailRef: IterateeTailRef[Binary,Byte] =>
    tailRef.promise.set(IEDone((), StreamEnd))
    synchronized {
      state match {
        case Running(_, pending) => {
          state = Closed
          for (suspendable <- pending) {
            suspendable.resumeWithResult(Left(new IllegalStateException(state.toString)))
          }
        }
        case _ => throw new IllegalStateException(state.toString)
      }
    }
  }

}
