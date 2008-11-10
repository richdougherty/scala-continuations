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

  def methA() = {

    def fun(k:Int=>String) = {
      for (i<-List(1,2,3,4)) 
        println(k(i))
      Some("returnvalue")
    }
    
    shift(fun) * 2
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
    //
    // Chose option #3, but with additional annotation 
    // attached to the result type. Reason: type vars are
    // not accessible in method annotation.
    // There is a shorthand, though: With @cps[A], the
    // 2nd parameter can be given.
    // And we use Any,Any as default case, not Unit.

    
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
