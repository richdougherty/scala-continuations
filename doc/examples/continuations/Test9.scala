// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.collection.mutable._

object Test9 {

  type Monadic[+U, C[_]] = {
    def flatMap[V](f: U => C[V]): C[V]
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
      
      val left = List("x","y","z")
      val right = List(4,5,6)
      
      List(left.reflect[Any] -> right.reflect[Any])
    })
  
    println("reflecting options: " +  reset {
      
      def mightFail() = None.reflect[Any] // long computation here ...
      
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
