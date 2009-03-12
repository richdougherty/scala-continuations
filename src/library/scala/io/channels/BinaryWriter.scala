package scala.io.channels

import java.nio.ByteBuffer
import java.nio.channels._
import scala.collections.immutable.Binary
import scala.continuations._
import scala.continuations.ControlContext._

/**
 * Provides support for writing a Binary to a channel.
 */
final class BinaryWriter(selector: ASelector, channel: SelectableChannel with WritableByteChannel) {

  private def writeBuffer(buffer: ByteBuffer): Unit @suspendable = {
    if (buffer.hasRemaining) {
      AOperations.write(selector, channel, buffer)
      writeBuffer(buffer)
    } else ()
  }

  def write(binary: Binary): Unit @suspendable = {
    // XXX: Manual foreach until cps conversion handles properly
    def foreach0[A](i: Iterator[A])(body: A => Unit @suspendable): Unit @suspendable = {
      if (i.hasNext) {
        body
        foreach0(i)(body)
      } else ()
    }
    foreach0(binary.byteBuffers.elements) { buffer: ByteBuffer => writeBuffer(buffer) }
  }

}
