// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.concurrent._
import scala.concurrent.cpsops._


import java.net.InetSocketAddress
import java.net.InetAddress

import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.channels.SelectableChannel
import java.nio.channels.ServerSocketChannel
import java.nio.channels.SocketChannel
import java.nio.channels.spi.SelectorProvider

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.nio.charset.CharsetEncoder

import java.util.regex.Pattern
import java.util.regex.Matcher

import java.util.Set
import scala.collection.jcl.Conversions._


// adapted from http://vodka.nachtlicht-media.de/tutHttpd.html

object Test17Webserver extends FJTaskRunners {

//  type Foo = ControlContext[Any,Any,Any]

  case class Generator[+A,-B,+C](val fun: (A => B @cps[Any,Any]) => (C @cps[Any,Any])) {

    final def foreach(f: (A => B @cps[Any,Any])): C @cps[Any,Any] = {
      fun(f)
    }

  }

  def selections(selector: Selector): ControlContext[Set[SelectionKey], Unit, Unit] = 
  shiftR { k: (Set[SelectionKey] => Any) =>

      println("inside select")

      while(true) { // problem???
        val count = selector.selectNow()
        if (count > 0)
          k(selector.selectedKeys())
   }
  }

  def createAsyncSelector() = {

  	val selector = SelectorProvider.provider().openSelector()
	
	  // TODO: this should run in its own thread, so select can block safely
  	spawn {
  		selections(selector).fun({ keySet =>
  		  for (key <- keySet) {
    			println("Select: " + key)
			
    			val handler = key.attachment().asInstanceOf[(SelectionKey => Any)]
  			
    			println("handling: " + handler)
  			  handler(key)
    		}
  		  keySet.clear()
  		}, { t: Throwable => throw t })
  	}

    selector
  }


  def callbacks(channel: SelectableChannel, selector: Selector, ops: Int) =
  Generator { k: (SelectionKey => Unit @cps[Any,Any]) =>

	  println("level 1 callbacks")

    shift { outerk: (Unit => Any) =>

    	def callback(key: SelectionKey) = {

    		key.interestOps(0)
		
    		spawn { 
  			  println("before continuation in callback")
  			  
    			k(key)
    			  
    			println("after continuation in callback")
    			  
    			if (key.isValid()) {
    				key.interestOps(ops)
    				selector.wakeup()
    			} else {
    			  outerk()
            //returnto.gen();
    			}
    		}
    	}
  	
  	  println("before registering callback")
  	  
	    val selectionKey = channel.register(selector, ops, callback _)
  	
  	  println("after registering callback")
  	  // stop
  	  ()
	  }
  }

  def acceptConnections(selector: Selector, port: Int) =
  Generator { k: (SocketChannel => Unit @cps[Any,Any]) =>

  	val serverSocketChannel = ServerSocketChannel.open()

  	serverSocketChannel.configureBlocking(false)
	
  	val isa = new InetSocketAddress(port)

  	serverSocketChannel.socket().bind(isa)
	
    for (key <- callbacks(serverSocketChannel, selector, SelectionKey.OP_ACCEPT)) {

    	val serverSocketChannel = key.channel().asInstanceOf[ServerSocketChannel]

  		val socketChannel = serverSocketChannel.accept()
  		socketChannel.configureBlocking(false)

      k(socketChannel)
    }
    
	  println("accept returning")
  }

  
  def readBytes(selector: Selector, socketChannel: SocketChannel) =
  Generator { k: (ByteBuffer => Unit @cps[Any,Any]) =>
    shift { outerk: (Unit => Any) =>
      reset {
      val bufSize = 4096 // for example...
  		val buffer = ByteBuffer.allocateDirect(bufSize)

      println("about to read")

  		for (key <- callbacks(socketChannel, selector, SelectionKey.OP_READ)) {

        println("about to actually read")
        
  			val count = socketChannel.read(buffer)

  			if (count < 0) {
          println("should close connection")
  				socketChannel.close()
  				
  				println("result of outerk " + outerk())
  				//returnto.gen() should cancel here!
  			} else {

    			buffer.flip()

          println("about to call read cont")
          
    			k(buffer)

  			  buffer.clear()
  			  shift { k: (Unit=>Any) => k() }
			  }
  		}
  		
  		println("readBytes returning")
  		outerk()
	    }
  	}
  }
  

  def readRequests(selector: Selector, socketChannel: SocketChannel) =
  Generator { k: (String => Unit @cps[Any,Any]) =>
    
    var s: String = "";
    
    for (buf <- readBytes(selector, socketChannel)) {
      k("read: " + buf)
    }
  }


  def writeResponse(selector: Selector, socketChannel: SocketChannel, res: String) = {

    val reply = res

  	val charset = Charset.forName("ISO-8859-1")
  	val encoder = charset.newEncoder()

  	socketChannel.write(encoder.encode(CharBuffer.wrap(reply)))
  }


  def handleRequest(req: String) = req


  def test() = {

    val sel = createAsyncSelector()

    println("http daemon running...")

    for (socketChannel <- acceptConnections(sel, 8080)) {

      spawn {
    	  println("Connect: " + socketChannel)

      	for (req <- readRequests(sel, socketChannel)) {

      		val res = handleRequest(req)
		
      		writeResponse(sel, socketChannel, res)
    		
      		shift { k: (Unit => Any) => k() } // FIXME: shouldn't be needed
      	}

    	  println("Disconnect: " + socketChannel)
  	  }

  		shift { k: (Unit => Any) => k() } // FIXME: shouldn't be needed
    }

  }



  def main(args: Array[String]) = {
  
    reset(test())

    Thread.sleep(1000*60*60)  // 1h!
    //    test.mainTaskRunner.waitUntilFinished()
  
  }

}
