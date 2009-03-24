import scala.actors.Actor
import scala.collection.immutable.Queue
import scala.continuations.ControlContext._
import scala.continuations._
import scala.continuations.async._

object Test {
  def main(args: Array[String]): Unit = {
    reset {
      def f(g: () => (Int @suspendable)): Int @suspendable = g()
      f(() => 1)
    }
  }
}
