import scala.continuations.ControlContext._

object Test {
  def suspended[A](x: A): A @suspendable = x
  def test[A](x: A): A @suspendable = suspended(x) match { case x => x }
}
