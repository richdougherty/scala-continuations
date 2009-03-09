package scala.io.channels

import java.nio.ByteBuffer
import java.nio.channels._
import scala.collections.immutable.Binary
import scala.continuations._
import scala.continuations.ControlContext._
import scala.io.iteratee._

object ChannelIteration {

  def read[A](selector: ASelector, channel: SelectableChannel with ReadableByteChannel, bufferSize: Int, iteratee: Iteratee[Binary,Byte,A]): Tuple2[A, StreamEvent[Binary]] @suspendable = {
    val binaryReader = new BinaryReader(selector, channel, bufferSize) // XXX: Not thread-safe, so iteratee should only call IEConts once.
    def read0(iteratee0: Iteratee[Binary,Byte,A]): Tuple2[A, StreamEvent[Binary]] @suspendable = {
      iteratee0 match {
        case IEDone(x, unprocessed) => {
          channel.close
          (x, unprocessed)
        }
        case IECont(k) => {
          val binary = binaryReader.read
          // XXX: Need try/catch block to generate StreamError, but not yet supported by selectivecps.
          val streamEvent = if (binary.isEmpty) StreamEnd else Chunk(binary)
          val nextIteratee = k(streamEvent)
          read0(nextIteratee)
        }
      }
    }
    read0(iteratee)
  }

  def write(selector: ASelector, channel: SelectableChannel with WritableByteChannel): Iteratee[Binary,Byte,Unit] @suspendable = {
    val binaryWriter = new BinaryWriter(selector, channel)
    def step(str: StreamEvent[Binary]): Iteratee[Binary,Byte,Unit] @suspendable = {
      str match {
        case Chunk(binary) => {
          // XXX: Need try/catch block to generate IEDone with StreamError, but not yet supported by selectivecps.
          binaryWriter.write(binary)
          IECont(step(_))
        }
        case _ => {
          channel.close
          IEDone((), str)
        }
      }
    }
    IECont(step(_))
  }

}
