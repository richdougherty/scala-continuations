package scala.actors.io

import java.nio.channels._
import scala.continuations._
import scala.continuations.async.{ActorSuspender,Suspendable}
import scala.continuations.ControlContext._
import scala.collection.immutable.Queue
import scala.collection.jcl.Conversions._

object ASelector {
  abstract sealed class Operation { val mask: Int }
  case object Accept extends Operation { val mask = SelectionKey.OP_ACCEPT }
  case object Connect extends Operation { val mask = SelectionKey.OP_CONNECT }
  case object Read extends Operation { val mask = SelectionKey.OP_READ }
  case object Write extends Operation { val mask = SelectionKey.OP_WRITE }
  private[ASelector] val OPS = List(Accept, Connect, Read, Write)
}

/**
 * Wraps a <code>Selector</code>, adding support for callbacks when operations
 * are ready.
 *
 * @param selector The <code>Selector</code> to wrap.
 *
 * @author <a href="http://www.richdougherty.com/">Rich Dougherty</a>
 */
class ASelector(val selector: Selector) {
  import ASelector._

  def this() = this(Selector.open)

  private type Attachment = Map[Operation,Queue[Suspendable[Unit]]]

  private var registrationCount: Int = 0

  /**
   * That actor that is servicing the registrations.
   */
  private var selectActor: Option[Actor] = None

  private[this] def selectionLoop: Unit = {

    selector.select(250)

    var registrationsFinished = 0

    for (key <- selector.selectedKeys) {
      key.synchronized { // XXX: Synchronize on ch.blockingLock instead?
        // Key state
        val currentInterestOps = key.interestOps
        val readyOps = key.readyOps
        val actualReadyOps = key.interestOps & key.readyOps // readyOps is not always updated by the Selector
        var opMap = key.attachment.asInstanceOf[Attachment]
        // Process each ready op
        for (op <- OPS if ((actualReadyOps & op.mask) != 0)) {
          val opEntry = opMap(op)
          for (suspendable <- opEntry) {
            registrationsFinished += 1
            suspendable.resume(())
          }
          opMap = opMap - op
        }
        // Update key
        val newInterestOps = currentInterestOps & ~actualReadyOps
        key.interestOps(newInterestOps)
        key.attach(opMap)
      }      
    }

    // Terminate actor if no current registrations.
    synchronized {
      registrationCount -= registrationsFinished
      if (registrationCount == 0) {
        selectActor = None
        Actor.exit
      }
    }

    selectionLoop // Hopefully tail-recursive.
  }

  def register(ch: SelectableChannel, op: Operation): Unit @suspendable =
    ActorSuspender.shiftSuspendable { suspendable: Suspendable[Unit] =>
      val key = try {
        ch.register(selector, 0, Map())
      } catch {
        case t: Throwable => suspendable.transferWithResult(Left(t))
      }

      key.synchronized { // XXX: Synchronize on ch.blockingLock instead?
        try {
          val currentInterestOps = key.interestOps
          val newInterestOps = currentInterestOps | op.mask
          if (newInterestOps != currentInterestOps) {
            key.interestOps(newInterestOps)
          }
          val oldOpMap = key.attachment.asInstanceOf[Attachment]
          val oldOpEntry = oldOpMap.getOrElse(op, Queue.Empty)
          val newOpEntry = oldOpEntry.enqueue(suspendable)
          val newOpMap = oldOpMap + ((op, newOpEntry))
          key.attach(newOpMap)
        } catch {
          case t: Throwable => suspendable.transferWithResult(Left(t))
        }

        // Stay synchronized on the key until the Suspendable has been
        // suspended.Because we don't want it being resumed before
        // then. It is safe to synchronize on the ASelector because
        // we always synchronize on a key first, thus avoiding deadlocks.

        // Start actor if first registration.
        synchronized {
          if (registrationCount == 0) {
            selectActor = Some(Actor.actor { selectionLoop })
          }
          registrationCount += 1
        }
        
        suspendable.suspend
      }
    }

  // TODO: Implement stop/cancel?
  
}
