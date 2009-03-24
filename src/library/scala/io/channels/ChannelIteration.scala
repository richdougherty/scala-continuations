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
    //println(scala.actors.Actor.self + ": ChannelIteration.read called")
    val binaryReader = new BinaryReader(selector, channel, bufferSize) // XXX: Not thread-safe, so iteratee should only call IEConts once.
    def read0(iteratee0: Iteratee[Binary,Byte,A]): IEDone[Binary,Byte,A] @suspendable = {
      iteratee0 match {
        case d @ IEDone(_, _) => {
          //println(scala.actors.Actor.self + ": ChannelIteration.read: got IEDone, closing channel.")
          channel.close
          d
        }
        case IECont(k) => {
          //println(scala.actors.Actor.self + ": ChannelIteration.read: Got IECont, reading from BinaryReader.")
          val binary = binaryReader.read
          // XXX: Need try/catch block to generate StreamError, but not yet supported by selectivecps.
          val streamEvent = if (binary.isEmpty) StreamEnd else Chunk(binary)
          //println(scala.actors.Actor.self + ": ChannelIteration.read: Sending StreamEvent to IECont: " + streamEvent)
          val nextIteratee = k(streamEvent)
          //println(scala.actors.Actor.self + ": ChannelIteration.read: Reading into next iteratee")
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
          //println(scala.actors.Actor.self + ": ChannelIteration.write: putting chunk in IECont")
          // XXX: Need try/catch block to generate IEDone with StreamError, but not yet supported by selectivecps.
          binaryWriter.write(binary)
          IECont(step(_))
        }
        case _ => {
          //println(scala.actors.Actor.self + ": ChannelIteration.write: closing")
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
        //println(scala.actors.Actor.self + ": ChannelIteration.reader: Getting tail of reader")
        val readerTail = reader.tail.apply
        //println(scala.actors.Actor.self + ": ChannelIteration.reader: Got tail of reader, starting read.")
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
