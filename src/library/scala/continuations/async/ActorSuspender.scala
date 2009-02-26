package scala.continuations.async

import scala.actors._
import scala.continuations.ControlContext._

/**
 * An object that suspends and resumes computations using reactions.
 */
object ActorSuspender extends Suspender {

  def prepare2[A](ret: A => Any, thr: Throwable => Any) = new Suspendable[A] {

    private sealed trait State
    private case object Initial extends State
    private case class Suspended(ch: Channel[Either[Throwable,A]]) extends State
    private case object Resumed extends State

    private var state: State = Initial

    private def reactOn(ch: Channel[Either[Throwable,A]]) = {
      ch.react {
        case Right(x: Any) => eval(ret, thr) { x.asInstanceOf[A] } // Workaround for type erasure
        case Left(t) => thr(t)
      }
    }

    def suspend: Nothing = synchronized {
      state match {
        case Initial => {
          val ch = new Channel[Either[Throwable,A]] /* Actor.self */
          state = Suspended(ch)
          reactOn(ch)
        }
        case illegal => throw new IllegalStateException(illegal.toString)
      }
    }

    def resume(x: A): Unit = synchronized {
      state match {
        case Suspended(ch) => {
          state = Resumed
          ch ! Right(x)
        }
        case illegal => throw new IllegalStateException(illegal.toString)
      }
    }

    def resumeWithError(t: Throwable): Unit = synchronized {
      state match {
        case Suspended(ch) => {
          state = Resumed
          ch ! Left(t)
        }
        case illegal => throw new IllegalStateException(illegal.toString)
      }
    }

    def transfer(x: A): Nothing = synchronized {
      state match {
        case Initial => {
          state = Resumed
          val ch = new Channel[Either[Throwable,A]] /* Actor.self */
          ch ! Right(x)
          reactOn(ch)
        }
        case illegal => throw new IllegalStateException(illegal.toString)
      }
    }

    def transferWithError(t: Throwable): Nothing = synchronized {
      state match {
        case Suspended(ch) => {
          state = Resumed
          val ch = new Channel[Either[Throwable,A]] /* Actor.self */
          ch ! Left(t)
          reactOn(ch)
        }
        case illegal => throw new IllegalStateException(illegal.toString)
      }
    }

  }

}
