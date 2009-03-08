package scala.continuations.async

import scala.actors._
import scala.continuations.ControlContext._
import scala.continuations.Result._

/**
 * An object that suspends and resumes computations using reactions.
 */
object ActorSuspender extends Suspender {

  def prepare2[A](ret: A => Any, thr: Throwable => Any) = new Suspendable[A] {

    private sealed trait State
    private case object Waiting extends State
    private case class Suspended(ch: Channel[Either[Throwable,A]]) extends State
    private case class Resumed(result: Either[Throwable,A]) extends State

    private var state: State = Waiting

    private def reactOn(ch: Channel[Either[Throwable,A]]) = {
      ch.react {
        case Right(x: Any) => send(ret, thr) { x.asInstanceOf[A] } // Workaround for type erasure
        case Left(t) => thr(t)
      }
    }

    def suspend: Nothing = synchronized {
      state match {
        case Waiting => {
          val ch = new Channel[Either[Throwable,A]] /* Actor.self */
          state = Suspended(ch)
          reactOn(ch)
        }
        case Resumed(result) => {
          val ch = new Channel[Either[Throwable,A]] /* Actor.self */
          state = Waiting
          ch ! result
          reactOn(ch)
        }
        case illegal => throw new IllegalStateException(illegal.toString)
      }
    }

    def resumeWithResult(result: Either[Throwable,A]): Unit = synchronized {
      state match {
        case Waiting => {
          state = Resumed(result)
        }
        case Suspended(ch) => {
          state = Waiting
          ch ! result
        }
        case illegal => throw new IllegalStateException(illegal.toString)
      }
    }

    def transferWithResult(result: Either[Throwable,A]): Nothing = {
      resumeWithResult(result)
      suspend
    }

  }

}
