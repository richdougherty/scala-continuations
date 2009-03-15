package examples.continuations

import scala.continuations.ControlContext._

object Goto {

  case class Label(k: Label => Unit)

  private case class GotoThunk(label: Label) extends Throwable

  def label: Label @suspendable =
    shift((k: Label => Unit) => executeFrom(Label(k)))

  def goto(l: Label): Nothing =
    throw new GotoThunk(l)

  private def executeFrom(label: Label): Unit = {
    val nextLabel = try {
      label.k(label)
      None
    } catch {
      case g: GotoThunk => Some(g.label)
    }
    if (nextLabel.isDefined) executeFrom(nextLabel.get)
  }

}

object RichTest11Goto {

  def main(args: Array[String]) = {
    import Goto._

    reset {
      var sum = 0
      var i = 0
      val beforeLoop = label
      if (i < 10000) {
        println(i)
        sum += i
        i += 1
        goto(beforeLoop)
      }
      println(sum)
    }

  }

}
