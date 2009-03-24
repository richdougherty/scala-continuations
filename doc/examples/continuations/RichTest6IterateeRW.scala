// $Id$

package examples.continuations

import java.net.InetSocketAddress
import java.nio.channels._
import java.nio.ByteBuffer
import java.util.concurrent.Semaphore
import scala.actors.Actor
import scala.collections.immutable.Binary
import scala.continuations._
import scala.continuations.ControlContext._
import scala.io.channels._

object RichTest6IterateeRW {

  def main(args: Array[String]) {
    for (i <- 0 until 100) {
      openClose(i.toString)
    }
  }

  def openClose(id: String) = {
    def log(msg: String) = println(id + ": " + msg)

    log("Starting")

    val selector = new ASelector

    def writeAll(sc: SocketChannel, message: List[Byte]): Unit @suspendable = {
      val bytes = message.toArray
      val bb = ByteBuffer.wrap(bytes)
      def write: Unit @suspendable = {
        if (bb.hasRemaining) {
          AOperations.write(selector, sc, bb)
          write
        } else ()
      }
      write
    }

    def readAll(sc: SocketChannel): List[Byte] @suspendable = {
      val bb = ByteBuffer.allocate(500)
      def read: List[Byte] @suspendable = {
        val length = AOperations.read(selector, sc, bb)
        if (length == 0) {
          bb.flip
          val array = new Array[Byte](bb.remaining)
          bb.get(array)
          array.toList
        } else {
          read
        }
      }
      read
    }

    val finished = new Semaphore(0, true)

    val address = new InetSocketAddress("localhost", 12345)

    log("Server binding")
    val ssc = ServerSocketChannel.open
    ssc.configureBlocking(false)
    val ss = ssc.socket
    ss.setReuseAddress(true)
    ss.bind(address)

    Actor.actor {
      reset {
        //log("Server accepting")
        val sc1 = AOperations.accept(selector, ssc)
        sc1.configureBlocking(false)
        val message1 = Binary.fromString("helloworld")
        log("Sending: " + message1)
        val writer = ChannelIteration.writer(selector, sc1)
        writer.writeAll(message1)
        //log("Server connected: " + sc1.isConnected)
        writer.writeClose
        //log("Server connected: " + sc1.isConnected)
        ssc.close
        //log("Server finished")
        finished.release
      }
    }

    Actor.actor {
      reset {
        //log("Client actor starting")
        val sc2: SocketChannel = SocketChannel.open
        sc2.configureBlocking(false)
        //log("Client connecting")
        sc2.connect(address)
        //log("Client finishing connection")
        AOperations.finishConnect(selector, sc2)
        //log("Client reading")

        val reader = ChannelIteration.reader(selector, sc2, 512)
        val message1 = reader.readAll
        log("Received: " + message1)
        //log("Client connected: " + sc2.isConnected)
        reader.readClose
        //log("Client nearly finished")
        //log("Client connected: " + sc2.isConnected)
        //log("Client finished")
        finished.release
      }
    }

    finished.acquire
    finished.acquire

    log("Finished")
  }
  
}
