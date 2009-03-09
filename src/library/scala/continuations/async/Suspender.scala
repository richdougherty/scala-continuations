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
 * A computation which can be suspended and resumed. Suspending the
 * computation will throw an exception to terminate the current
 * thread.  Resuming it will restart the computation on another
 * thread.
 *
 * It is possible to "resume" a Suspendable before suspending it. This
 * will result in it automatically being resumed as soon as it is
 * suspended.
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
  def resume(x: A): Unit = resumeWithResult(Right(x))

  /**
   * Resume a computation that has been suspended, providing a reified
   * result. The computation should be run in another thread, so that
   * calling resume is a lightweight operation.
   */
  def resumeWithResult(result: Either[Throwable,A]): Unit

  /**
   * @deprecated Call resume then suspend instead.
   */
  def transfer(x: A): Nothing = transferWithResult(Right(x))

  /**
   * @deprecated Call resumeWithResult then suspend instead.
   */
  def transferWithResult(result: Either[Throwable,A]): Nothing

}

