package scala.actors.io

import java.nio.ByteBuffer
import java.nio.channels._
import scala.continuations._
import scala.continuations.ControlContext._

/**
 * Low-level access to non-blocking operations on channels.
 */
object AOperations {

  def accept(selector: ASelector, channel: ServerSocketChannel): SocketChannel @suspendable = {
    def accept0: SocketChannel @suspendable = {
      channel.accept match {
        case null => {
          // Accept failed, use selector to callback when ready.
          selector.register(channel, ASelector.Accept)
          accept0
        }
        case socketChannel => socketChannel
      }
    }
    accept0
  }

  def finishConnect(selector: ASelector, channel: SocketChannel): Unit @suspendable = {
    def finishConnect0: Unit @suspendable = {
      channel.finishConnect match {
        case false => {
          // Connect failed, use selector to callback when ready.
          selector.register(channel, ASelector.Connect)
          finishConnect0
        }
        case true => ()
      }
    }
    finishConnect0
  }

  /**
   * @return Number of bytes read. A value of 0 means there is no more input.
   */
  def read(selector: ASelector, channel: SelectableChannel with ReadableByteChannel, buffer: ByteBuffer): Int @suspendable = {
    def read0: Int @suspendable = {
      channel.read(buffer) match {
        case -1 => 0 // Nothing to read
        case 0 => {
          // No data available yet: set callback.
          selector.register(channel, ASelector.Read)
          read0
        }
        case length => length
      }
    }
    read0
  }

  // newly written, not copied from Scala OTP - untested
  /**
   * @return Number of bytes read. A value of 0 means there is no more input.
   */
  def scatteringRead(selector: ASelector, channel: SelectableChannel with ScatteringByteChannel, buffers: Array[ByteBuffer], offset: Int, length: Int): Long @suspendable = {
    def scatteringRead0: Long @suspendable = {
      channel.read(buffers, offset, length) match {
        case -1 => 0
        case 0 => {
          // No data available yet: set callback.
          selector.register(channel, ASelector.Write)
          scatteringRead0
        }
        case length => length
      }
    }
    scatteringRead0
  }

  // newly written, not copied from Scala OTP - untested
  def write(selector: ASelector, channel: SelectableChannel with WritableByteChannel, buffer: ByteBuffer): Int @suspendable = {
    def write0: Int @suspendable = {
      channel.write(buffer) match {
        case 0 => {
          // No buffer space available yet: set callback.
          selector.register(channel, ASelector.Write)
          write0
        }
        case length => length
      }
    }
    write0
  }

  def gatheringWrite(selector: ASelector, channel: SelectableChannel with GatheringByteChannel, buffers: Array[ByteBuffer], offset: Int, length: Int): Long @suspendable = {
    def gatheringWrite0: Long @suspendable = {
      channel.write(buffers, offset, length) match {
        case 0 => {
          // No buffer space available yet: set callback.
          selector.register(channel, ASelector.Write)
          gatheringWrite0
        }
        case length => length
      }
    }
    gatheringWrite0
  }

}
