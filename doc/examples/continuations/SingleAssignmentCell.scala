// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.collection.mutable.Queue
//import scala.collection.immutable.Map

import scala.concurrent._
import scala.concurrent.cpsops._

import java.util.concurrent.atomic.AtomicReference

final class SingleAssignmentCell[A](implicit private[this] val sched: AbstractTaskRunner) {

  private[this] var value: A = _
  private[this] var hasVal = false
//  private[this] val handlers = new Queue[A=>Unit]()

  type State = Either[List[A=>Unit],A]
  
  private[this] val state = new AtomicReference[State](Left(Nil))
  
  def get(): A @cps[Unit,Unit] = {
    shift { (k:(A => Unit)) =>

      if (hasVal) {// fast-path
        //sched.submitTask(() => k(value)) // could exec on calling thread
        k(value)
      } else {
        while (!(state.get match {
          case Right(value) =>
//            sched.submitTask(() => k(value)) // could exec on calling thread
            k(value)
            true
          case s @ Left(handlers) =>
            state.compareAndSet(s, Left(k::handlers))
        }))()
      }

/*
      synchronized {
        if (hasVal) {
          val v = value
          sched.submitTask(() => k(v))
        } else {
          handlers += k
        }
      }
*/      
    }
  }
  
  def set(v: A) {

    while (!(state.get match {
      case Right(value) =>
        throw new Exception("cannot assign twice to single-assignment variable")
      case s @ Left(handlers) =>
        if (state.compareAndSet(s, Right(v))) {
          this.value = v
          this.hasVal = true // fast-path
          for (k <- handlers) {
            sched.submitTask(() => k(v))
          }
          true
        } else false
    }))()
    
/*
    synchronized {
      if (hasVal)
        throw new Exception("cannot assign twice to single-assignment variable")
      
      value = v
      hasVal = true
      for (k <- handlers) {
        sched.submitTask(() => k(v))
      }
      handlers.clear()
    }
*/    
  }
}
