// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.collection.mutable._

object Test9 {

  type Monadic[+U, CC[_]] = {
    def flatMap[V](f: U => CC[V]): CC[V]
  }

  class Reflective[+A, C[_]](xs: Monadic[A,C]) {
    def reflect[B](): A @cps[C[B], C[B]] = {
      shift { k:(A => C[B]) =>
        xs.flatMap(k)
      }
    }
  }

  implicit def reflective[A](xs:Iterable[A]) = new Reflective[A,Iterable](xs)
  implicit def reflective[A](xs:Option[A]) = new Reflective[A,Option](xs) 
  

  def main(args: Array[String]) = {

    println("reflecting lists: " + reset {
      List((List("x","y","z").reflect[Any], List(4,5,6).reflect[Any]))
    })
  
    println("reflecting options: " +  reset {
      
      def mightFail() = None.reflect[Any] // long computation here ...
      
      type comp[A] = A @cps[Option[A], Option[A]]
      
//      def tryCatch[A](a: =>comp[A], b: =>comp[A]): comp[A] = { // doesn't work ?!?
        
      def tryCatch[A](a: =>(A @cps[Option[A], Option[A]]))(
        b: =>(A @cps[Option[A], Option[A]])): 
            (A @cps[Option[A], Option[A]]) = {
        
        reset(Some(a)) match {
          case Some(x) => x
          case None => b
        }
      }
      
      
      val x = tryCatch {
        mightFail()
        "after failure"
      } {
        "after catching failure"
      }
      
      Some(x)
    })

  }
}
