// $Id$

package scala.continuations

class cps[-B] extends Annotation {
}

class uncps extends Annotation {
}

class cpstypes[-B,+C] extends TypeConstraint {
}



final class Shift[+A,-B,+C](val fun: (A => B) => C) {
  final def map[D](f: (A => D)) = {
    new Shift((g:(D => B)) => fun((x:A) => g(f(x))))
  }
  final def flatMap[D,E<:B](f: (A => Shift[D,E,B])): Shift[D,E,C] = {
    new Shift((g:(D => E)) => fun((x:A) => f(x).fun(g)))
  }
}







object CPS {

  def shiftUnit[A,B,C](x: A): A @cpstypes[B,C] = {
    throw new Exception("cps!")
  }

  def shift[A,B,C](fun: (A => B) => C): A @cpstypes[B,C] = {
    throw new Exception("cps!")
  }

  def reify[A,B,C](ctx: =>(A @cpstypes[B,C])): Shift[A,B,C] = {
    throw new Exception("cps!")
  }
  

  def shiftUnitR[A,B](x: A): Shift[A,B,B] = {
    shiftR((k:A=>B) => k(x))
  }

  def shiftR[A,B,C](fun: (A => B) => C): Shift[A,B,C] = {
    new Shift(fun)
  }

  def reifyR[A,B,C](ctx: => Shift[A,B,C]): Shift[A,B,C] = {
    ctx
  }


  def reset[A,C](ctx: =>(A @cpstypes[A,C])):C = {
    reify[A,A,C](ctx).fun((x:A) => x)
  }

  
}
