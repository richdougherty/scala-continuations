// $Id$

package scala.continuations

class cps extends Annotation {
}

class uncps extends Annotation {
}


trait Context[A] {
  def map[B](f: (A => B)): Context[B]
  def flatMap[B](f: (A => Context[B])): Context[B]
}

final class Shift[A,B,C](val fun: (A => B) => C) extends Context[A] {
  override final def map[D](f: (A => D)) = {
    new Shift((g:(D => B)) => fun((x:A) => g(f(x))))
  }
  override final def flatMap[D](f: (A => Context[D])) = {
    new Shift((g:(D => B)) => fun((x:A) => f(x).asInstanceOf[Shift[D,B,B]].fun(g)))
  }
}







object CPS {


  // at compile-time, methods marked @uncps are tranformed
  // becoming the identity function


  @uncps def shift2val[A,B,C](x: Shift[A,B,C]): A = {
    x.asInstanceOf[A]
  }


  @uncps def val2shift[A,B,C](x: A): Shift[A,B,C] = {
    x.asInstanceOf[Shift[A,B,C]]
  }


  // methods marked @cps will return Context[A] instead of A

  @cps def shift[A,B,C](fun: (A => B) => C):A = {
    shift2val[A,B,C](new Shift[A,B,C](fun))
  }

  def reset[A,C](/*@uncps */ctx: A):C = {
    val2shift[A,A,C](ctx).fun((x:A) => x)
  }

}
