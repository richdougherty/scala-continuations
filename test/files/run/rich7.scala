import scala.actors.Actor
import scala.collection.immutable.Queue
import scala.continuations.ControlContext._
import scala.continuations._
import scala.continuations.async._

case class Cont(k: Int => Cont @suspendable)

object Test {
  def main(args: Array[String]): Unit = {
    def create: Cont = Cont((x: Int) => create)
    def transform(c: Cont): Cont @suspendable = {
      c match {
        case Cont(k) => Cont((x: Int) => transform(k(x)))
      }
    }
    reset {
      val c = transform(create)
      c.k(1)
    }
  }
}
