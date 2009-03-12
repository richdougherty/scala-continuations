package scala.io.iteratee

import scala.actors.Actor
import scala.collections.immutable.Binary
import scala.continuations._
import scala.continuations.async._
import scala.continuations.ControlContext._

// XXX: Better name?
class IterateeTailRef[Ch,El](prev: Iteratee[Ch,El,Unit])(implicit chunking: Chunking[Ch,El]) {

  val promise = new APromise[Iteratee[Ch,El,Unit]]

  def bind[A](sub: Iteratee[Ch,El,A]): Tuple2[Iteratee[Ch,El,A],IterateeTailRef[Ch,El]] @suspendable = {
    ActorSuspender.shiftSuspendable { (suspendable: Suspendable[Tuple2[Iteratee[Ch,El,A],IterateeTailRef[Ch,El]]]) =>
      Actor.actor {
        reset {
          def transform(sub0: Iteratee[Ch,El,A]): Iteratee[Ch,El,Unit] @suspendable = sub0 match {
            case IECont(k) => {
              println("Transforming cont")
              IECont((str: StreamEvent[Ch]) => transform(k(str)))
            }
            case IEDone(x, unprocessed) => {
              println("Transforming done")
              val nextTailRef = new IterateeTailRef[Ch,El](IEDone((), unprocessed))
              println("Resuming result actor")
              suspendable.resume((sub0, nextTailRef)) // Sends result to original caller of bind
              println("Waiting on promise")
              nextTailRef.promise.apply // Waits for promise in nextTailRef
            }
          }
          println("IterateeTailRef.bind called")
          val transformedNext = transform(sub)
          println("Setting promise")
          promise.set(prev >> transformedNext)
          println("Suspending result actor")
        }
      }
      suspendable.suspend
    }
  }
}
