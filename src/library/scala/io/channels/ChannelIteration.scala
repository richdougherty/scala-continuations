package scala.io.channels

import java.nio.ByteBuffer
import java.nio.channels._
import scala.actors._
import scala.collections.immutable.Binary
import scala.continuations._
import scala.continuations.ControlContext._
import scala.io.iteratee._

object ChannelIteration {

  def read[A](selector: ASelector, channel: SelectableChannel with ReadableByteChannel, bufferSize: Int, iteratee: Iteratee[Binary,Byte,A]): IEDone[Binary,Byte,A] @suspendable = {
    println("ChannelIteration.read called")
    val binaryReader = new BinaryReader(selector, channel, bufferSize) // XXX: Not thread-safe, so iteratee should only call IEConts once.
    def read0(iteratee0: Iteratee[Binary,Byte,A]): IEDone[Binary,Byte,A] @suspendable = {
      iteratee0 match {
        case d @ IEDone(_, _) => {
          channel.close
          d
        }
        case IECont(k) => {
          val binary = binaryReader.read
          // XXX: Need try/catch block to generate StreamError, but not yet supported by selectivecps.
          val streamEvent = if (binary.isEmpty) StreamEnd else Chunk(binary)
          println("Sending StreamEvent to cont: " + streamEvent)
          val nextIteratee = k(streamEvent)
          read0(nextIteratee)
        }
      }
    }
    read0(iteratee)
  }

  def write(selector: ASelector, channel: SelectableChannel with WritableByteChannel): Iteratee[Binary,Byte,Unit] = {
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

  def reader(selector: ASelector, channel: SelectableChannel with ReadableByteChannel, bufferSize: Int): IterateeReader = {
    val reader = new IterateeReader
    Actor.actor {
      reset {
        println("Reading tail of reader")
        val readerTail = reader.tail.apply
        println("Got tail of reader")
        read(selector, channel, bufferSize, readerTail)
        ()
      }
    }
    reader
  }

  def writer(selector: ASelector, channel: SelectableChannel with WritableByteChannel): IterateeWriter = {
    val iteratee = write(selector, channel)
    new IterateeWriter(iteratee)
  }

}
