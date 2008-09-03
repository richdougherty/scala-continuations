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

  type Context[A] = Shift[A,Any,Any]
  
  // at compile-time, these are removed

  @uncps implicit def shift2val[A,B,C](x: =>Shift[A,B,C]): A @cpstypes[B,C] = {
    x.asInstanceOf[A @cpstypes[B,C]]
  }

  @uncps implicit def val2shift[A](x: =>A): Shift[A,A,Any] = {
    x.asInstanceOf[Shift[A,A,Any]]
  }


  @uncps def stronglyTyped[A,B,C](x: =>A @cpstypes[B,C]): Shift[A,B,C] = {
    x.asInstanceOf[Shift[A,B,C]]
  }

  // methods marked @cps will return Context[A] instead of A

  @cps def shift[A,B,C](fun: (A => B) => C): Shift[A,B,C] = {
    new Shift[A,B,C](fun)
  }

  def reset[A,C](ctx: =>Shift[A,A,C]):C = {
    ctx.fun((x:A) => x)
  }

  def spawn[A,C](ctx: =>Shift[A,Unit,C]):C = {
    ctx.fun((x:A) => ())
  }
  
}
