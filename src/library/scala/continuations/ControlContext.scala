// $Id$


// TODO: scaladoc

package scala.continuations

import scala.continuations.Result._

class cps[-B,+C] extends TypeConstraint


final class ControlContext[+A,-B,+C](val fun: (A => B, Throwable => B) => C) {

  private def extend[A1,B1<:B](translate: (A1 => B1, Throwable => B1) => (A => B, Throwable => B)): ControlContext[A1,B1,C] = {
    val fun1 = (ret1: A1 => B1, thr1: Throwable => B1) => {
      val (ret, thr) = translate(ret1, thr1)
      fun(ret, thr)
    }
    new ControlContext(fun1)
  }

  // linear execution
 
  final def map[A1](f: (A => A1)): ControlContext[A1,B,C] = {
    extend { (ret1: A1 => B, thr1: Throwable => B) =>
      val ret = { x: A => send(ret1,thr1) { f(x) } }
      (ret, thr1)
    }
  }
  
  final def flatMap[A1,B1<:B](f: (A => ControlContext[A1,B1,B])): ControlContext[A1,B1,C] = {
    extend { (ret1: A1 => B1, thr1: Throwable => B1) =>
      val ret = { x: A => flatSend[A1,B1,B](ret1,thr1) { f(x) } }
      (ret, thr1)
    }
  }

  // TODO: filter

  // catch
  
  final def cat[A1>:A](pf: PartialFunction[Throwable, A1]): ControlContext[A1,B,C] = {
    // Create new THROW continuation; leave return continuation unchanged.
    extend { (ret1: A1 => B, thr1: Throwable => B) =>
      val thr = { t: Throwable =>
        if (pf.isDefinedAt(t)) {
          send[A1,B](ret1, thr1) { pf(t) }
        } else {
          thr1(t)
        }
      }
      (ret1, thr)
    }
  }

  final def flatCat[A1>:A,B1<:B](pf: PartialFunction[Throwable, ControlContext[A1,B1,B]]): ControlContext[A1,B1,C] = {
    // Create new THROW continuation; leave return continuation unchanged.
    extend { (ret1: A1 => B1, thr1: Throwable => B1) =>
      val thr = { t: Throwable =>
        if (pf.isDefinedAt(t)) {
          flatSend[A1,B1,B](ret1, thr1) { pf(t) }
        } else {
          thr1(t)
        }
      }
      (ret1, thr)
    }
  }

  // finally

  final def fin(f: => Unit): ControlContext[A,B,C] = {
    extend { (ret1: A => B, thr1: Throwable => B) => 
      val ret = { a: A =>
        // Save return value, evaluate f, continue with return value unless exception.
        val savedRet = { _: Unit => ret1(a) }
        send[Unit,B](savedRet, thr1) { f }
      }
      val thr = { t: Throwable =>
        // Save thrown exception, evaluate f, continue by re-throwing exception unless another exception.
        val savedThr = { _: Unit => thr1(t) }
        send[Unit,B](savedThr, thr1) { f }
      }
      (ret, thr)
    }
  }

  // XXX: Is type of f correct?
  final def flatFin[B1<:B](f: Unit => ControlContext[Unit,B1,B1]): ControlContext[A,B1,C] = {
    extend { (ret1: A => B1, thr1: Throwable => B1) => 
      val ret = { a: A =>
        // Save return value, evaluate f, continue with return value unless exception.
        val savedRet = { _: Unit => ret1(a) }
        flatSend[Unit,B1,B1](savedRet, thr1) { f(()) }
      }
      val thr = { t: Throwable =>
        // Save thrown exception, evaluate f, continue by re-throwing exception unless another exception.
        val savedThr = { _: Unit => thr1(t) }
        flatSend[Unit,B1,B1](savedThr, thr1) { f(()) }
      }
      (ret, thr)
    }
  }

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

  def relay[A,B](ret: A => B, thr: Throwable => B)(ctx: =>(A @cps[B,B])): B = {
    flatSend(ret, thr) { reify[A,B,B](ctx) }
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
