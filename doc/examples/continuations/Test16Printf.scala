// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._


object Test16Printf {

  def visit[A](lst: List[A]): List[A] @cps[List[A],List[A]] = lst match {
    case Nil => shift { k: (Nothing=>Any) => Nil }
    case a::rest => {
      val tail = shift { k: (List[A]=>Any) => k(Nil) :: reset(k(visit(rest))) }
      a::tail
    }
  }
  
  def prefix[A](list: List[A]) = reset(visit(list))
  
  val int = (x: Int) => x.toString
  val str = (x: String) => x
  
  def format[A,B](toStr: A => String) = shift { k: (String => B) => (x:A) => k(toStr(x)) }
  
  def main(args: Array[String]) {

    val res = prefix(List(1,2,3))
    println(res)
    
    val fun0 = reset[String,String]("Hello World!")
    println(fun0)
    
    val fun1 = reset { "Hello " + format[String,String](str) + "!"}
    println(fun1)
    println(fun1("World"))
    
/*
    val fun2 = {
      reify(format[String,Int=>String](str)).flatMap { a: String =>
        reify(format[Int,String](int)).map { b: String =>
          a + "=" + b
        }
      }
    }.fun(x => x)
*/

    val fun2 = reset {
      "The value of " + format[String,Int=>String](str) + " is " + format[Int,String](int) + "."
    }
    
    println(fun2)
    println(fun2("x"))
    println(fun2("x")(3))


    def printf[A](str: =>String @cps[String,A]) = {
      reset(println(str))
    }

    printf("Hello " + format[String,String](str) + "!")("World")
    
    
    def sprintf[A](str: =>String @cps[String,A]) = {
      reset(str)
    }

    val ff1 = sprintf[String]("Hello World!")
    val ff2 = sprintf("Hello " +
      format[String,String](str) + "!")
    val ff3 = sprintf("The value of " + 
      format[String,Int=>String](str) + 
      " is " + format[Int,String](int) + ".")
    
    
    
    println("done")
    
  }
  
}
