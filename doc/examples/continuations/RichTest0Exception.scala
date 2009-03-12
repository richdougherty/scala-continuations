// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

object RichTest0Exception {

  def printResult(ctx: =>(Any @cps[Unit,Unit])): Unit = {
    println("---")
    val ret = { x: Any => println("Returned: " + x) }
    val thr = { t: Throwable => println("Threw: " + t.getMessage) }
    relay[Any,Unit](ret,thr) { ctx }
  }

  def main(args: Array[String]): Any = {
    printResult { "no shift" }
    printResult { error("no shift (auto)") }

    printResult { shiftUnit("shift unit") }

    printResult {
      "before shift"
      shift { k: (String => Unit) =>
        k("inside shift")
      }
      "after shift"
    }
    printResult {
      "before shift"
      shift { k: (String => Unit) => k("inside shift") }
    }

    printResult {
      error("before shift (auto)")
      shift { k: (String => Unit) =>
        k("inside shift")
      }
      "after shift"
    }
    printResult {
      "before shift"
      shift2 { (ret: String => Unit, thr: Throwable => Unit) =>
        thr(new Exception("inside shift, before ret (explicit)"))
        ret("inside shift")
      }
      "after shift"
    }
    printResult {
      "before shift"
      shift2 { (ret: String => Unit, thr: Throwable => Unit) =>
        thr(new Exception("inside shift (explicit)"))
      }
      "after shift"
    }
    printResult {
      "before shift"
      shift2 { (ret: String => Unit, thr: Throwable => Unit) =>
        ret("inside shift")
        thr(new Exception("inside shift, after ret (explicit)"))
      }
      "after shift"
    }
    printResult {
      "before shift"
      shift { k: (String => Unit) =>
        k("inside shift")
      }
      error("after shift, immediately (auto)")
      "after shift"
    }
    printResult {
      "before shift"
      shift { k: (String => Unit) =>
        k("inside shift")
      }
      "after shift"
      error("after shift, later (auto)")
    }
  }
  
}
