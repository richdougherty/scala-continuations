// $Id$

import scala.continuations._
import scala.continuations.CPS._




object Test0 {


  def main(args: Array[String]) {

    println(reset[Int,Int] {
      
      2 * { println("up"); val x = 1 + shift((k:Int=>Int) => k(k(k(17)))); println("down"); x }
      
      
    })
    
  }

}
