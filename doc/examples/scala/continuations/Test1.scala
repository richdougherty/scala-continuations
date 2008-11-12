// $Id$

import scala.continuations._
import scala.continuations.ControlContext._




object Test1 {

  def testThisMethod() = {

    1 + shift((k:Int=>Int) => k(k(k(17))))

  }

  def testThisCallingMethod() = {
    
    testThisMethod() * 2
    
  }

  def main(args: Array[String]) {

    val result = reset(testThisCallingMethod())
    
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
