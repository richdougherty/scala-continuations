// $Id$


// TODO: scaladoc

package scala.continuations


class cps[-B,+C] extends TypeConstraint


final class ControlContext[+A,-B,+C](val fun: (A => B) => C) {
  
  final def map[A1](f: (A => A1)): ControlContext[A1,B,C] = {
    new ControlContext((k:(A1 => B)) => fun((x:A) => k(f(x))))
  }
  
  /*  
    final def flatMap[A1,B1<:B](f: (A => ControlContext[A1,B1,B])): ControlContext[A1,B1,C] = {
      new ControlContext((k:(A1 => B1)) => fun((x:A) => f(x).fun(k)))
    }
  */

    final def flatMap[A1,B1,C1<:B](f: (A => ControlContext[A1,B1,C1])): ControlContext[A1,B1,C] = {
      new ControlContext({ k:(A1 => B1) =>
        fun { (x:A) =>
          val res: C1 = f(x).fun(k)
          res
        }
      })
    }

  // TODO: filter (?)

}




private class cpsv[-B] extends Annotation // implementation detail

private class uncps extends Annotation // implementation detail
private class docps extends Annotation // implementation detail



object ControlContext {

  type suspendable = cps[Unit,Unit]
  

  def shift[A,B,C](fun: (A => B) => C): A @cps[B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the Scala CPS plugin")
  }

  def reset[A,C](ctx: =>(A @cps[A,C])): C = {
    reify[A,A,C](ctx).fun((x:A) => x)
  }

  def run[A](ctx: =>(Any @cps[Unit,A])): A = {
    reify[Any,Unit,A](ctx).fun((x:Any) => ())
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
    new ControlContext(fun)
  }

  def reifyR[A,B,C](ctx: => ControlContext[A,B,C]): ControlContext[A,B,C] = {
    ctx
  }

}
