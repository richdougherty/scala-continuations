import scala.continuations.ControlContext._

object Test {
  def main(args: Array[String]): Unit = {
    reset { 0 }
  }
}
