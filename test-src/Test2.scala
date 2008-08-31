// $Id$

import scala.continuations._
import scala.continuations.CPS._





object Test2 {

  @cps def testThisMethod() = {
    

    def fun(k:Int=>String) = { 
      for (i<-List(1,2,3,4,5)) 
        println(k(i))
      Some("returnvalue")
    }
    
    2 * shift(fun)
  }

  @cps def testThisCallingMethod() = {
    
    "   +++ " + testThisMethod().toString()
    
  }


  def main(args: Array[String]) {

    val result: Any = reset(testThisMethod())

    println(result)
    
    /*
      expect output:
      """
         +++ 2
         +++ 4
         +++ 6
         +++ 8
         +++ 10
      Some(returnvalue)
      """
    */
  }
  
}
