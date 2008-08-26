// $Id$

import scala.continuations._
import scala.continuations.CPS._




object Test1 {

  @cps def testThisMethod() = {

    1 + shift((k:Int=>Int) => k(k(k(17))))

  }

  @cps def testThisCallingMethod() = {
    
    testThisMethod() * 2
    
  }

  def main(args: Array[String]) {


    val result: Int = reset(testThisCallingMethod())
    
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
