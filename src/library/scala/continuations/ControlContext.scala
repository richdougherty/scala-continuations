// $Id$


// TODO: scaladoc

package scala.continuations


class cps[-B,+C] extends TypeConstraint


final class ControlContext[+A,-B,+C](val fun: (A => B, Throwable => B) => C) {
  
  final def map[A1](f: (A => A1)): ControlContext[A1,B,C] = {
    new ControlContext((ret1: A1 => B, thr1: Throwable => B) => fun(f andThen ret1, thr1))
  }
  
  final def flatMap[A1,B1<:B](f: (A => ControlContext[A1,B1,B])): ControlContext[A1,B1,C] = {
    new ControlContext((ret1: A1 => B1, thr1: Throwable => B1) => fun(((x:A) => f(x).fun(ret1, thr1)), thr1))
  }

  // TODO: filter

}




private class cpsv[-B] extends Annotation // implementation detail

private class uncps extends Annotation // implementation detail



import scala.concurrent.AbstractTaskRunner

object ControlContext {

  type suspendable = cps[Unit,Unit]
  

  def shift[A,B,C](fun: (A => B) => C): A @cps[B,C] = {
    shift2((ret: A => B, thr: Throwable => B) => fun(ret))
  }

  def shift2[A,B,C](fun: (A => B, Throwable => B) => C): A @cps[B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the scala CPS plugin")
  }

  def reset[A,C](ctx: =>(A @cps[A,C])): C = {
    val ret = { x: A => x }
    val thr = { t: Throwable => throw t }
    reify[A,A,C](ctx).fun(ret, thr)
  }

  def run[A](ctx: =>(Any @cps[Unit,A])): A = {
    val ret = { x: Any => () }
    val thr = { t: Throwable => throw t }
    reify[Any,Unit,A](ctx).fun(ret, thr)
  }

  def spawn(ctx: =>(Any @cps[Unit,Any]))(implicit sched: AbstractTaskRunner): Unit = {
    sched.submitTask(() => run(ctx))
  }

  
  // methods below are mostly implementation details and are not
  // needed frequently in client code

  def shiftUnit0[A,B](x: A): A @cps[B,B] = {
    shiftUnit[A,B,B](x)
  }

  def shiftUnit[A,B,C>:B](x: A): A @cps[B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the scala CPS plugin")
  }

  def reify[A,B,C](ctx: =>(A @cps[B,C])): ControlContext[A,B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the scala CPS plugin")
  }  

  def shiftUnitR[A,B](x: A): ControlContext[A,B,B] = {
    shiftR((k:A=>B) => k(x))
  }

  def shiftR[A,B,C](fun: (A => B) => C): ControlContext[A,B,C] = {
    new ControlContext((ret: A => B, thr: Throwable => B) => fun(ret))
  }

  def shift2R[A,B,C](fun: (A => B, Throwable => B) => C): ControlContext[A,B,C] = {
    new ControlContext(fun)
  }

  def reifyR[A,B,C](ctx: => ControlContext[A,B,C]): ControlContext[A,B,C] = {
    ctx
  }

}
