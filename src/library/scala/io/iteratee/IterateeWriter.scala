package scala.io.iteratee

import scala.collections.immutable.Binary
import scala.continuations._
import scala.continuations.async._
import scala.continuations.ControlContext._

import Iteratee._

class IterateeWriter(first: Iteratee[Binary,Byte,Any]) {

  sealed trait State
  case class Open(next: IECont[Binary,Byte,Any]) extends State
  case class Running(pending: Suspendable[IECont[Binary,Byte,Any]]) extends State
  case object Closed extends State

  val lock = new ALock
  var next = first

  // lock must be held before calling
  private def feedEvent(str: StreamEvent[Binary]): Unit @suspendable = {
    next match {
      case IECont(k) => next = k(str)
      case other => throw new IllegalStateException(other.toString)
    }
  }

  def feedChunks(chunks: List[Binary]): Unit @suspendable = lock.sync {
    def feedChunks0(chunks0: List[Binary]): Unit @suspendable = chunks0 match {
      case Nil => ()
      case head :: tail => {
        feedEvent(Chunk(head))
        feedChunks0(tail)
      }
    }
    feedChunks0(chunks)
  }

  def writeAll(binary: Binary): Unit @suspendable = feedChunks(binary :: Nil)

  def writeClose: Unit @suspendable = lock.sync { feedEvent(StreamEnd) }

}
