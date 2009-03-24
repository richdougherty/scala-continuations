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
      //println(scala.actors.Actor.self + ": IterateeTailRef.bind: binding " + sub)
      Actor.actor {
        //println(scala.actors.Actor.self + ": IterateeTailRef.bind: new actor to transform sub-iteratee")
        reset {
          def transform(sub0: Iteratee[Ch,El,A]): Iteratee[Ch,El,Unit] @suspendable = sub0 match {
            case IECont(k) => {
              //println(scala.actors.Actor.self + ": IterateeTailRef.bind: Transforming cont")
              IECont((str: StreamEvent[Ch]) => transform(k(str)))
            }
            case IEDone(x, unprocessed) => {
              //println(scala.actors.Actor.self + ": IterateeTailRef.bind: Transforming done")
              val nextTailRef = new IterateeTailRef[Ch,El](IEDone((), unprocessed))
              //println(scala.actors.Actor.self + ": IterateeTailRef.bind: Resuming result actor")
              suspendable.resume((sub0, nextTailRef)) // Sends result to original caller of bind
              //println(scala.actors.Actor.self + ": IterateeTailRef.bind: Waiting on promise")
              nextTailRef.promise.apply // Waits for promise in nextTailRef
            }
          }
          val transformedNext = transform(sub)
          //println(scala.actors.Actor.self + ": IterateeTailRef.bind: setting tail promise")
          promise.set(prev >> transformedNext)
          //println(scala.actors.Actor.self + ": IterateeTailRef.bind: Suspending result actor")
        }
      }
      suspendable.suspend
    }
  }
}
