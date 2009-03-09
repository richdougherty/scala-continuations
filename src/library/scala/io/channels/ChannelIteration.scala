package scala.io.channels

import java.nio.ByteBuffer
import java.nio.channels._
import scala.collections.immutable.Binary
import scala.continuations._
import scala.continuations.ControlContext._
import scala.io.iteratee._

object ChannelIteration {

  def read[A](selector: ASelector, channel: SelectableChannel with ReadableByteChannel, bufferSize: Int, iteratee: Iteratee[Binary,Byte,A]): Tuple2[A, StreamEvent[Binary]] @suspendable = {
    def newBuffer = ByteBuffer.allocate(bufferSize)
    def read0(iteratee0: Iteratee[Binary,Byte,A], buffer: ByteBuffer): Tuple2[A, StreamEvent[Binary]] @suspendable = {
      if (!buffer.hasRemaining) read0(iteratee0, newBuffer)
      else {
        iteratee0 match {
          case IEDone(x, unprocessed) => {
            channel.close
            (x, unprocessed)
          }
          case IECont(k) => {
            // XXX: Need try/catch block to generate StreamError, but not yet supported by selectivecps.
            val readLength = AOperations.read(selector, channel, buffer)
            val streamEvent = readLength match {
              case 0 => StreamEnd
              case n => {
                buffer.flip
                val bin = Binary.fromArray(buffer.array, buffer.position, buffer.limit)
                buffer.flip
                Chunk(bin)
              }
            }
            val nextIteratee = k(streamEvent)
            read0(nextIteratee, buffer)
          }
        }
      }
    }
    read0(iteratee, newBuffer)
  }

}
