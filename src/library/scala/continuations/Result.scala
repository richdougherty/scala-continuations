package scala.continuations

/**
 * Methods for manipulating results of expressions as
 * Either[Throwable,A] objects.
 */
object Result {

  /**
   * Executes body and reifies its result into an Either[Throwable,A].
   * A normal result is stored in a Right, an exception is stored in a
   * Left.
   */
  def reifyResult[A](body: => A): Either[Throwable,A] =
    try { Right(body) } catch { case t => Left(t) }

  /**
   * Given a reified result, return its result or throw its exception.
   */
  def invokeResult[A](result: Either[Throwable,A]): A =
    sendResult((a: A) => a, (t: Throwable) => throw t, result)

  //def flatInvokeResult[A](result: Either[Throwable,ControlContext[A,A,A]]): A =
  //  flatSendResult((x: A) => x, (t: Throwable) => throw t, result)

  /**
   * Given a reified result, send either send its result to ret1 or
   * send its exception to thr1.
   */
  def sendResult[A,B](ret1: A => B, thr1: Throwable => B, result: Either[Throwable,A]): B = {
    result match {
      case Right(a) => ret1(a)
      case Left(t) => thr1(t)
    }
  }

  /**
   * Given a reified result, send either apply its result with both
   * continuations or send its exception to thr1.
   */
  def flatSendResult[A,B<:C,C](ret1: A => B, thr1: Throwable => B, result: Either[Throwable,ControlContext[A,B,C]]): C = {
    result match {
      case Right(cc1) => cc1.fun(ret1, thr1)
      case Left(t) => thr1(t)
    }
  }

  /**
   * Evaluates body and then calls one of the given continuations
   * based on its result. If body returns normally then the ret1
   * continuation will be called with the result. Otherwise the thr1
   * continuation will be called with the exception.
   */
  def send[A,B](ret1: A => B, thr1: Throwable => B)(body: => A): B =
    sendResult(ret1, thr1, reifyResult(body))

  /**
   * Evaluates body to get a ControlContext to execute, or calls thr1
   * if body fails. If body returns normally then the ControlContext
   * will be evaluated with both continuations. Otherwise the thr1
   * continuation will be called with the exception.
   */
  def flatSend[A,B<:C,C](ret1: A => B, thr1: Throwable => B)(body: => ControlContext[A,B,C]): C =
    flatSendResult(ret1, thr1, reifyResult(body))

}
