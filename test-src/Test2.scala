// $Id$

import scala.continuations._
import scala.continuations.CPS._



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


  @cps def methA() = {
    

    2 * shift((k:Int=>String) => { 

      println(k(1))
      println(k(2))
      println(k(3))
      println(k(4))

      Some("returnvalue")
    })

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


  @cps def methB() = {
    
    "   +++ " + methA().toString()
    
  }


  def main(args: Array[String]) {

    val result: Some[String] = reset(methB())

    println(result)

    // so far, there's a type cast in reset
    //
    // possible options to get rid of it:
    //
    //   - erase types during transformation and re-type
    //     program afterwards (slow)
    //   - control flow analysis during transformation
    //     (not easy to do, problems with hof's)
    //   - use annotations @cps[A,B], possibly with
    //     default case @cps = @cps[Unit,Unit]

    
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
