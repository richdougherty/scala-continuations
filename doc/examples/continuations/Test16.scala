// $Id$

package examples.continuations

import java.net.InetSocketAddress
import java.nio.channels._
import java.nio.ByteBuffer
import java.util.concurrent.Semaphore
import scala.actors.Actor
import scala.continuations._
import scala.continuations.ControlContext._
import scala.io.channels._

object Test16 {

  def main(args: Array[String]) {

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
    //val wait = new Semaphore(0, true)

    val address = new InetSocketAddress("localhost", 12345)

    val ssc = ServerSocketChannel.open
    ssc.configureBlocking(false)
    val ss = ssc.socket
    ss.setReuseAddress(true)
    ss.bind(address)

    Actor.actor {
      reset {
        val sc1 = AOperations.accept(selector, ssc)
        sc1.configureBlocking(false)
        val message1 = "helloworld".getBytes.toList
        println("Sending: " + message1)
        writeAll(sc1, message1)
        //wait.acquire
        //val message2 = "goodbye".getBytes.toList
        //println("Sending: " + message2)
        //writeAll(sc1, message2)
        sc1.close
        ssc.close
        finished.release
      }
    }

    Thread.sleep(100)

    Actor.actor {
      reset {
        val sc2: SocketChannel = SocketChannel.open
        sc2.configureBlocking(false)
        sc2.connect(address)
        AOperations.finishConnect(selector, sc2)
        val message1 = readAll(sc2)
        println("Received: " + message1)
        //wait.release
        //val message2 = readAll(sc2)
        //println("Received: " + message2)
        sc2.close
        finished.release
      }
    }

    finished.acquire
    finished.acquire
  }
  
}
