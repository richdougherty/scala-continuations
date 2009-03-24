package scala.io.channels

import java.nio.ByteBuffer
import java.nio.channels._
import scala.collections.immutable.Binary
import scala.continuations._
import scala.continuations.ControlContext._

/**
 * Provides support for reading a Binary from a channel. This class is
 * not thread-safe; its use should be externally synchronized.
 */
final class BinaryReader(selector: ASelector, channel: SelectableChannel with ReadableByteChannel, bufferSize: Int) {

  private var buffer: ByteBuffer = null

  // Returns Binary.empty if stream finished
  def read: Binary @suspendable = {
    //println(scala.actors.Actor.self + ": BinaryReader.read")
    if (buffer == null) { buffer = ByteBuffer.allocate(bufferSize) }
    val readLength = AOperations.read(selector, channel, buffer)
    readLength match {
      case 0 => Binary.empty
      case n => {
        buffer.flip
        val bin = Binary.fromArray(buffer.array, buffer.position, buffer.limit)
        buffer.flip
        if (!buffer.hasRemaining) { buffer = null } // Eagerly deallocate
        bin
      }
    }
  }

}
