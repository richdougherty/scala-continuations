// $Id$

import scala.continuations._
import scala.continuations.CPS._




object Test1 {

  def testThisMethod(): Int @cpstypes[Int,Int] = {

    1 + shift((k:Int=>Int) => k(k(k(17))))

  }

  def testThisCallingMethod(): Int @cpstypes[Int,Int] = {
    
    testThisMethod() * 2
    
  }

  def main(args: Array[String]) {

    val result = reset[Int,Int](testThisCallingMethod())
    
    // ((((((17 + 1) * 2) + 1) * 2) + 1) * 2) = 150
    
    println(result)
    
    /*
      expect output:
      """
      150
      """
    */
    
  }

}
