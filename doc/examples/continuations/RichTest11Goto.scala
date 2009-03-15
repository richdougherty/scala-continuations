package examples.continuations

import scala.continuations.ControlContext._

case class Label(k: Label => Unit)

object Goto {

  private case class GotoThunk(l: Label) extends Throwable

  def label: Label @suspendable = shift { (k: Label => Unit) =>
    val l = new Label(k)
    def execute: Unit = {
      try {
        k(l)
      } catch {
        case GotoThunk(l0) if (l0 == l) => execute
      }
    }
    execute
  }

  def goto(l: Label): Nothing = throw new GotoThunk(l)

}

object RichTest11Goto {

  def main(args: Array[String]) = {
    import Goto._

    reset {
      var sum = 0
      var i = 0
      val beforeLoop = label
      if (i < 10) {
        println(i)
        sum += i
        i += 1
        goto(beforeLoop)
      }
      println(sum)
    }

  }

}
