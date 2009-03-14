// $Id$


// TODO: scaladoc

package scala.continuations

import scala.continuations.Result._

class cps[-B,+C] extends TypeConstraint


final class ControlContext[+A,-B,+C](val fun: (A => B, Throwable => B) => C) {

  // linear execution

  case class Rethrowable(t: Throwable) extends Throwable
 
  final def map[A1](f: (A => A1)): ControlContext[A1,B,C] = {
    val fun1 = (ret1: A1 => B, thr1: Throwable => B) => {
      val ret: A => B = { x: A =>
        var captureExceptions = true
        try {
          val x1 = f(x)
          captureExceptions = false
          ret1(x1)
        } catch {
          case t1 if captureExceptions => thr1(t1)
        }
      }
      fun(ret, thr1)
    }
    new ControlContext(fun1)
  }
  
  final def flatMap[A1,B1<:B,C1<:B](f: (A => ControlContext[A1,B1,C1])): ControlContext[A1,B1,C] = {
    val fun1 = (ret1: A1 => B1, thr1: Throwable => B1) => {
      val ret: A => B = { x: A =>
        var captureExceptions = true
        try {
          val cc1 = f(x)
          captureExceptions = false
          cc1.fun(ret1, thr1)
        } catch {
          case t1 if captureExceptions => thr1(t1)
        }
      }
      fun(ret, thr1)
    }
    new ControlContext(fun1)
  }

  final def foreach(f: (A => B)) = {
    fun(f, ((t: Throwable) => throw t))
  }

  // TODO: filter (?)

  // catch
  
  final def cat[A1>:A](pf: PartialFunction[Throwable, A1]): ControlContext[A1,B,C] = {
    val fun1 = (ret1: A1 => B, thr1: Throwable => B) => {
      val thr: Throwable => B = { t: Throwable =>
        var captureExceptions = true
        try {
          if (pf.isDefinedAt(t)) {
            val x1 = pf(t)
            captureExceptions = false
            ret1(x1)
          } else {
            captureExceptions = false
            thr1(t)
          }
        } catch {
          case t1 if captureExceptions => thr1(t1)
        }
      }
      fun(ret1, thr)
    }
    new ControlContext(fun1)
  }

  final def flatCat[A1>:A,B1<:B,C1<:B](pf: PartialFunction[Throwable, ControlContext[A1,B1,C1]]): ControlContext[A1,B1,C] = {
    val fun1 = (ret1: A1 => B1, thr1: Throwable => B1) => {
      val thr: Throwable => B = { t: Throwable =>
        var captureExceptions = true
        try {
          if (pf.isDefinedAt(t)) {
            val cc1 = pf(t)
            captureExceptions = false
            cc1.fun(ret1, thr1)
          } else {
            captureExceptions = false
            thr1(t)
          }
        } catch {
          case t1 if captureExceptions => thr1(t1)
        }
      }
      fun(ret1, thr)
    }
    new ControlContext(fun1)
  }

  // finally

  final def fin(f: => Unit): ControlContext[A,B,C] = {
    val fun1 = (ret1: A => B, thr1: Throwable => B) => {
      val ret: A => B = { x: A =>
        var captureExceptions = true
        try {
          f
          captureExceptions = false
          ret1(x)
        } catch {
          case t1 if captureExceptions => thr1(t1)
        }
      }
      val thr: Throwable => B = { t: Throwable =>
        var captureExceptions = true
        try {
          f
          captureExceptions = false
          thr1(t)
        } catch {
          case t1 if captureExceptions => thr1(t1)
        }
      }
      fun(ret, thr1)
    }
    new ControlContext(fun1)
  }

  // XXX: Is type of f correct?
  final def flatFin[B1<:B,C1<:B](f: Unit => ControlContext[Unit,B1,C1]): ControlContext[A,B1,C] = {
    val fun1 = { (ret1: A => B1, thr1: Throwable => B1) =>
      val ret: A => B = { x: A =>
        var captureExceptions = true
        try {
          val cc1 = f()
          captureExceptions = false
          val savedRet = { _: Unit => ret1(x) }
          cc1.fun(savedRet, thr1)
        } catch {
          case t1 if captureExceptions => thr1(t1)
        }
      }
      val thr: Throwable => B = { t: Throwable =>
        var captureExceptions = true
        try {
          val cc1 = f()
          captureExceptions = false
          val savedThr = { _: Unit => thr1(t) }
          cc1.fun(savedThr, thr1)
        } catch {
          case t1 if captureExceptions => thr1(t1)
        }
      }
      fun(ret, thr1)
    }
    new ControlContext(fun1)
  }
}




private class cpsv[-B] extends Annotation // implementation detail

private class uncps extends Annotation // implementation detail
private class docps extends Annotation // implementation detail



object ControlContext {

  type suspendable = cps[Unit,Unit]
  

  def shift[A,B,C](fun: (A => B) => C): A @cps[B,C] = {
    shift2((ret: A => B, thr: Throwable => B) => fun(ret))
  }

  def shift2[A,B,C](fun: (A => B, Throwable => B) => C): A @cps[B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the Scala CPS plugin")
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
    flatSend[A,B,B,B](ret, thr) { reify[A,B,B](ctx) }
  }

  // methods below are mostly implementation details and are not
  // needed frequently in client code

  def shiftUnit0[A,B](x: A): A @cps[B,B] = {
    shiftUnit[A,B,B](x)
  }

  def shiftUnit[A,B,C>:B](x: A): A @cps[B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the Scala CPS plugin")
  }

  def reify[A,B,C](ctx: =>(A @cps[B,C])): ControlContext[A,B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the Scala CPS plugin")
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
