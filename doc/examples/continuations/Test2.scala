// $Id$

package examples.continuations


import scala.continuations._
import scala.continuations.ControlContext._



object Test2 {

  // type before translation:
  //
  //    () => Int
  //
  // "real" type after translation:
  //
  //    () => Shift[Int, String, Some[String]]
  //
  // can only find out params 2 and 3 by
  // looking at method body

  def methA() = {

    def fun(k:Int=>String) = {
      for (i<-List(1,2,3,4)) 
        println(k(i))
      Some("returnvalue")
    }
    
    2 * shift(fun) // FIXME: doing shift(fun) * 2 yields error!!
  }

  // type before translation:
  //
  //    () => String
  //
  // "real" type after translation:
  //
  //    () => Shift[String, String, Some[String]]
  //
  // params 2 and 3 depend on type of methA
  // after translation, which depends on methA's
  // body, not just the signature.

  def methB() = {

    "   +++ " + methA().toString()
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
      Some(returnvalue)
      """
    */
  }
  
}
