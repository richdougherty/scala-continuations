// $Id$


// TODO: scaladoc

package scala.continuations


class cps[-B,+C] extends TypeConstraint


final class ControlContext[+A,-B,+C](val fun: (A => B) => C) {
  
  final def map[D](f: (A => D)) = {
    new ControlContext((g:(D => B)) => fun((x:A) => g(f(x))))
  }
  
  final def flatMap[D,E<:B](f: (A => ControlContext[D,E,B])): ControlContext[D,E,C] = {
    new ControlContext((g:(D => E)) => fun((x:A) => f(x).fun(g)))
  }

}




private class cpsv[-B] extends Annotation

private class uncps extends Annotation





object ControlContext {

/*
  implicit def shiftUnit0[A](x: A): A @cps[A,A] = {
    shiftUnit[A,A](x)
  }
*/

  def shiftUnit[A,B,C>:B](x: A): A @cps[B,C] = {
    throw new Exception("cps!")
  }

  def shift[A,B,C](fun: (A => B) => C): A @cps[B,C] = {
    throw new Exception("cps!")
  }

  def reify[A,B,C](ctx: =>(A @cps[B,C])): ControlContext[A,B,C] = {
    throw new Exception("cps!")
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


  def reset[A,C](ctx: =>(A @cps[A,C])): C = {
    reify[A,A,C](ctx).fun((x:A) => x)
  }

  
}
