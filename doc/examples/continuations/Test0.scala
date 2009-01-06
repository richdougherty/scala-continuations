// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._


object Test0 {


  def main(args: Array[String]) {

    val z = reset {
      
      2 * {
	      println("up")
	      val x = shift((k:Int=>Int) => k(k(k(8))))
	      println("down")
	      x
      }
      
    }
    
    println(z)
    
  }

}
