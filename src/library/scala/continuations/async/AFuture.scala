package scala.continuations.async

import scala.continuations._
import scala.continuations.ControlContext._

trait AFuture[+A] extends Function0[A @cps[Unit,Nothing]] {

  def immediate: Option[A]

  def isSet: Boolean

}
