// $Id$


// TODO: scaladoc

package scala.continuations.async

import scala.continuations._
import scala.continuations.ControlContext._

/**
 * An object which is able to suspend and resume computations.
 */
trait Suspender {

  /**
   * Construct a Suspendable from the given pair of continuations.
   */
  def prepare2[A](ret: A => Any, thr: Throwable => Any): Suspendable[A]

  /**
   * Construct a Suspendable from the given continuation.
   */
  final def prepare[A](ret: A => Any): Suspendable[A] = { prepare2(ret, { t: Throwable => throw t }) }

  /**
   * Construct a Suspendable from the current continuation and supply
   * it to the given function.
   */
  final def shiftSuspendable[A,C](fun: Suspendable[A] => C) = {
    shift2 { (ret: A => Unit, thr: Throwable => Unit) =>
      val suspendable = prepare2(ret, thr)
      fun(suspendable)
    }
  }

}

/**
 * A computation which can be suspended and executed. This can only be
 * done once.
 */
trait Suspendable[A] {

  /**
   * Suspend this task. May be resumed later by calling resume.
   */
  def suspend: Nothing

  /**
   * Resume a computation that has been suspended, providing an input
   * value. The computation should be run in another thread, so that
   * calling resume is a lightweight operation.
   */
  def resume(x: A): Unit

  /**
   * Resume a computation that has been suspended, providing a reified
   * result. The computation should be run in another thread, so that
   * calling resume is a lightweight operation.
   */
  def resumeWithResult(result: Either[Throwable,A]): Unit

  /**
   * Transfer the Suspendable to another thread and execute it there.
   * Analagous to calling suspend then resume, but manages ordering of
   * the operations without the need for synchronization.
   */
  def transfer(x: A): Nothing

  def transferWithResult(result: Either[Throwable,A]): Nothing

}

