// $Id$

package scala.continuations

class cps extends Annotation {
}

class cpsnoconvert extends Annotation {
}


class Shift[A,B,C](val fun: (A => B) => C) {
  def bind[D](f: (A => D)) = {
    new Shift((g:(D => B)) => fun((x:A) => g(f(x))))
  } 
}


object CPS {

  @cps def shift[A,B,C](fun: (A => B) => C):A = {
    
    @cps val x = new Shift(fun)
    x.asInstanceOf[A]
  }

  def reset[A,B](x: A):B = {
    x.asInstanceOf[Shift[A,A,B]].fun((x:A) => x)
  }

}
