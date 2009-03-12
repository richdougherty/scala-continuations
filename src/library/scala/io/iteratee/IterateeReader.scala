package scala.io.iteratee

import scala.collections.immutable.Binary
import scala.continuations._
import scala.continuations.async._
import scala.continuations.ControlContext._

import Iteratee._

class IterateeReader {

  sealed trait State
  case class Open(tailRef: IterateeTailRef[Binary,Byte]) extends State
  case object Closed extends State

  private val stateLock = new ALock
  private var state: State = Open(new IterateeTailRef[Binary,Byte](IEDone((), Chunk(Binary.empty))))

  def tail = stateLock.sync {
    state match {
      case Open(tailRef) => tailRef.promise
      case _ => throw new IllegalStateException(state.toString)
    }
  }
  
  def bind[A](sub: Iteratee[Binary,Byte,A]): A @suspendable = {
    println("Binding: " + sub)
    stateLock.sync {
      state match {
        case Open(tailRef) => {
          val (resultIteratee, nextTailRef) = tailRef.bind(sub)
          resultIteratee match {
            case d @ IEDone(_, err@StreamError(t)) => {
              state = Closed
              nextTailRef.promise.set(IEDone((), err)) // Close underlying iteratee
              throw t
            }
            case d @ IEDone(x, _) => {
              state = Open(nextTailRef)
              x
            }
            case _ => {
              state = Closed
              val e = new Exception("Invalid result iteratee: " + resultIteratee)
              nextTailRef.promise.set(IEDone((), StreamError(e))) // Close underlying iteratee
              throw e
            }
          }
        }
        case other => throw new IllegalStateException(other.toString) // XXX: Friendlier exception?
      }
    }
  }

  def readByte: Option[Byte] @suspendable = bind(snext[Binary,Byte])

  def readAll: Binary @suspendable = bind(join[Binary,Byte])

  //def readLine(charset: String): String @suspendable = bind(breakMatch(newLineMatcher).map((line: Binary, newline: Option[Binary]) => line.decodeString(charSet)))

  def readClose: Unit @suspendable = stateLock.sync {
    state match {
      case Open(tailRef) => {
        state = Closed
        tailRef.promise.set(IEDone((), StreamEnd))
      }
      case _ => throw new IllegalStateException(state.toString)
    }
  }

}
