// $Id$

import scala.continuations._
import scala.continuations.CPS._





object Test2b {

  @cps[String] def methA() = {
    
    def fun(k:Int=>Any) = {
      for (i<-List(1,2,3,4)) 
        println(k(i))
      
      Some("returnvalue")
    }
    
    shift(fun) * 2
  }

  @cps[String] def methB() = {
    
    val x = testThisMethod()

    "   +++ " + x.toString()
    
  }

  def main(args: Array[String]) {

    val result = reset(methB())

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
