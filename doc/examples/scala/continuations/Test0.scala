// $Id$

import scala.continuations._
import scala.continuations.ControlContext._




object Test0 {


  def test0: Int = {
    reset[Int,Int](4)
  }

/*
  def test1: Int = {
    2 * {
      shift((k:Int=>Int) => k(1))
      println("bla")
      3
    }
  }

*/

  def main(args: Array[String]) {

    val z = reset {
      
      2 * { 
	      println("up")
	      val x = shift((k:Int=>Int) => k(k(k(17))))
	      println("down")
	      x
      }
      
    }
    
    println(z)
    
  }

}
