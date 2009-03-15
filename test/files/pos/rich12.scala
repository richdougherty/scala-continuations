import scala.continuations.ControlContext._

object Test12 {
  def test(b: => Boolean) = {
    reset {
      if (b) {
        shiftUnit[Unit,Unit,Unit](())
        error("x")
      }
    }
  }
}
