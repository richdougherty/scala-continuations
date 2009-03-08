package scala.io.channels

import java.nio.channels._
import scala.actors.Actor
import scala.continuations._
import scala.continuations.async.{ActorSuspender,Suspendable}
import scala.continuations.ControlContext._
import scala.collection.immutable.Queue
import scala.collection.jcl.Conversions._
import scala.collection.mutable.ArrayBuffer

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

  case class PendingRegistration(ch: SelectableChannel, op: Operation, suspendable: Suspendable[Unit])
  private var pendingRegistrations = new ArrayBuffer[PendingRegistration]

  private[this] def selectionLoop: Unit = {

    synchronized {
      //println("Processing registrations.")
      for (PendingRegistration(ch, op, suspendable) <- pendingRegistrations) {
        processPendingRegistration(ch, op, suspendable)
      }
      pendingRegistrations.clear
    }

    selector.select(3000)

    /*selector.synchronized {
      selector.keys.synchronized {
        for (key <- selector.keys) {
          var opMap = key.attachment.asInstanceOf[Attachment]
          println("Currently attached: " + opMap)
        }
      }
    }*/

    var registrationsFinished = 0

    for (key <- selector.selectedKeys) {
      key.synchronized { // XXX: Synchronize on ch.blockingLock instead?
        var opMap = key.attachment.asInstanceOf[Attachment]

        // Resumes all suspendables associated with the given operation,
        // also removing them from the attached opMap and decrementing
        // the registration count.
        def resumeForOp(op: Operation, result: Either[Throwable,Unit]) = {
          val opEntry = opMap(op)
          for (suspendable <- opEntry) {
            //println("Resuming suspended: " + op)
            registrationsFinished += 1
            suspendable.resume(())
          }
          opMap = opMap - op
        }

        try {
          // Key state
          val currentInterestOps = key.interestOps
          val readyOps = key.readyOps
          val actualReadyOps = key.interestOps & key.readyOps // readyOps is not always updated by the Selector
          // Process each ready op
          for (op <- OPS if ((actualReadyOps & op.mask) != 0)) {
            resumeForOp(op, Right(()))
          }
          // Update key
          val newInterestOps = currentInterestOps & ~actualReadyOps
          key.interestOps(newInterestOps)
          key.attach(opMap)
        } catch {
          case cke: CancelledKeyException => {
            // Key has been cancelled, presumably because the channel has been closed.
            // (SelectionKeys are not exposed for explicit cancelling, unless user
            // registers them directly against the underlying Selector.)
            // We pass on the exception to any still-suspended or pending operations.
            // TODO: Check that this is correct behaivour.
            //println("Key cancelled with " + opMap.size + " operations pending.")
            //cke.printStackTrace
            //println
            for (op <- opMap.keys) {
              resumeForOp(op, Left(cke))
            }
            // This is an invalid attachment, but it should not be accessed again because Selector should remove cancelled keys.
            // If it *is* accessed again, then a NPE is probably better than silently re-processing the operations.
            key.attach(null)
          }
        }
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

  def register(ch: SelectableChannel, op: Operation): Unit @suspendable = {
    ActorSuspender.shiftSuspendable { suspendable: Suspendable[Unit] =>
      //println("Adding pending registration: " + op)
      synchronized {
        pendingRegistrations += PendingRegistration(ch, op, suspendable)
        selector.wakeup

        // Start actor if first registration.
        synchronized {
          if (registrationCount == 0) {
            selectActor = Some(Actor.actor { selectionLoop })
          }
          registrationCount += 1
        }

        suspendable.suspend // FIXME: Registration could be processed before suspension occurs. Need to alter Suspendable's behaviour.
      }
    }
  }

  private def processPendingRegistration(ch: SelectableChannel, op: Operation, suspendable: Suspendable[Unit]): Unit = {
    //println("Processing pending registration: " + op)
    val key: SelectionKey = try {
      ch.blockingLock.synchronized {
        // TODO: Need to test contention for keys.
        val existingKey = ch.keyFor(selector)
        // TODO: Use Option idiom for this.
        if (existingKey == null) ch.register(selector, 0, Map()) else existingKey
      }
    } catch {
      case t: Throwable => {
        suspendable.resumeWithResult(Left(t))
        return // TODO: Restructure to avoid early return?
      }
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
        //println("Just attached: " + newOpMap)
      } catch {
        case t: Throwable => {
          suspendable.resumeWithResult(Left(t))
          return // TODO: Restructure to avoid early return?
        }
      }

      // Stay synchronized on the key until the Suspendable has been
      // suspended.Because we don't want it being resumed before
      // then. It is safe to synchronize on the ASelector because
      // we always synchronize on a key first, thus avoiding deadlocks.
    }
  }

  // TODO: Implement stop/cancel?
  
}
