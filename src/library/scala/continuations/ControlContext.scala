// $Id$


// TODO: scaladoc

package scala.continuations


class cps[-B,+C] extends TypeConstraint


final class ControlContext[+A,-B,+C](val fun: (A => B, Throwable => B) => C) {
  import ControlContext.{eval,flatEval}

  private def extend[A1,B1<:B](translate: (A1 => B1, Throwable => B1) => (A => B, Throwable => B)): ControlContext[A1,B1,C] = {
    val fun1 = (ret1: A1 => B1, thr1: Throwable => B1) => {
      val (ret, thr) = translate(ret1, thr1)
      fun(ret, thr)
    }
    new ControlContext(fun1)
  } 
 
  final def map[A1](f: (A => A1)): ControlContext[A1,B,C] = {
    extend { (ret1: A1 => B, thr1: Throwable => B) =>
      val ret = { x: A => eval(ret1,thr1) { f(x) } }
      (ret, thr1)
    }
  }
  
  final def flatMap[A1,B1<:B](f: (A => ControlContext[A1,B1,B])): ControlContext[A1,B1,C] = {
    extend { (ret1: A1 => B1, thr1: Throwable => B1) =>
      val ret = { x: A => flatEval[A1,B1,B](ret1,thr1) { f(x) } }
      (ret, thr1)
    }
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

  def into[A,B](ret: A => B, thr: Throwable => B)(ctx: =>(A @cps[B,B])): B = {
    flatEval(ret, thr) { reify[A,B,B](ctx) }
  }

  def spawn(ctx: =>(Any @cps[Unit,Any]))(implicit sched: AbstractTaskRunner): Unit = {
    sched.submitTask(() => run(ctx))
  }

  // support for working with ret/thr continuations

  def eval[A,B](ret1: A => B, thr1: Throwable => B)(body: => A): B = {
    val result = try { Right(body) } catch { case t => Left(t) }
    result match {
      case Right(a) => ret1(a)
      case Left(t) => thr1(t)
    }
  }

  def flatEval[A,B<:C,C](ret1: A => B, thr1: Throwable => B)(body: => ControlContext[A,B,C]): C = {
    val result = try { Right(body) } catch { case t => Left(t) }
    result match {
      case Right(cc1) => cc1.fun(ret1, thr1)
      case Left(t) => thr1(t)
    }
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
