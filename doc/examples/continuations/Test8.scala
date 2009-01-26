// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.concurrent._


trait ProducersConsumers extends JoinPatterns { self: TaskRunners =>

  abstract class ProduceConsume[A] {
    val produce = new (A ==> Unit)("produce")
    val consume = new (Unit ==> A)("consume")
  }

  // synchronous produce/consume without buffering

  class SynchProduceConsume[A] extends ProduceConsume[A] {
    join {
      case produce(x <== return_put) <&> consume(_ <== return_get) => 
        println("inside rule body (exchanging value " + x + ")")
        return_put() <&> return_get(x)
    }
  }


  // asynchronous produce/consume with one-place buffer
  
  class Asynch0ProduceConsume[A] extends ProduceConsume[A] {
    
    val empty = new (Unit ==> Unit)
    val item = new (A ==> Unit)
    
    join {
      case produce(x <== return_put) & empty(_ <== _) => 
        return_put() <&> item(x)
    }

    join {
      case consume(_ <== return_get) <&> item(x <== _) => 
        println("inside rule body (exchanging value " + x + ")")
        return_get(x) <&> empty()
    }

    spawn {
      empty()
    }
  }


  // asynchronous produce/consume (unlimited buffer)
  // without stable ordering of items

  class Asynch1ProduceConsume[A] extends ProduceConsume[A] {
    
    val item = new (A ==> Unit)
    
    join {
      case produce(x <== return_put) => 
        return_put() <&> item(x)
    }

    join {
      case consume(_ <== return_get) <&> item(x <== _) => 
        println("inside rule body (exchanging value " + x + ")")
        return_get(x)
    }
  }

  // asynchronous produce/consume with stable ordering of items,
  // produce and consume are mutually exclusive

  class Asynch2ProduceConsume[A] extends ProduceConsume[A] {
    
    val item = new (List[A] ==> Unit)
    
    join {
      case produce(x <== return_put) & item(xs <== _)=> 
        return_put() <&> item(xs ::: List(x))
    }

    join {
      case consume(_ <== return_get) <&> item((x::xs) <== _) => 
        println("inside rule body (exchanging value " + x + ")")
        return_get(x) <&> item(xs)
    }

    spawn {
      item(Nil)
    }
  }


  // asynchronous produce/consume, stable ordering, concurrent
  // produce and consume (using dynamic joins, i.e. rule creation
  // independent of channel creation)

  class Asynch3ProduceConsume[A] extends ProduceConsume[A] {
    
    case class Elem(x: A, next: Unit ==> Elem)
    
    val last = new ((Unit ==> Elem) ==> Unit)
    val first = new ((Unit ==> Elem) ==> Unit)
    
    join {
      case produce(x <== return_put) & last(elem <== _) => 
        val next = new (Unit ==> Elem)
        join { 
          case elem(_ <== return_elem) => return_elem(Elem(x, next))
        }
        return_put() <&> last(next)
    }

    join {
      case consume(_ <== return_get) <&> first(elem <== _) =>
        val data = elem()
        println("inside rule body (exchanging value " + data.x + ")")
        return_get(data.x) <&> first(data.next)
    }

    spawn {
      val elem = new (Unit ==> Elem)
      first(elem) <&> last(elem)
    }
  }

}


object Test8 extends ProducersConsumers with ThreadTaskRunners {
    

  // test setup, independent of actual implementation
  
  def testCode(p: ProduceConsume[String]) = {

    println("starting up...")

    val res = {
      p.produce("item 1")
      p.produce("item 2")
      p.produce("item 3")
      p.produce("item 4")
      "done producing"
    } <&> {
      println("received: " + p.consume())
      println("received: " + p.consume())
      println("received: " + p.consume())
      println("received: " + p.consume())
      "done consuming"
    }

    println("=> result: " + res)
  }


  def main(args: Array[String]) {

    run {
    
    println("=== synchronous")

    testCode(new SynchProduceConsume())

    println()
    println("=== asynchronous 0")
    
    testCode(new Asynch0ProduceConsume())
    
    println()
    println("=== asynchronous 1")
    
    testCode(new Asynch1ProduceConsume())

    println()
    println("=== asynchronous 2")
    
    testCode(new Asynch2ProduceConsume())

    println()
    println("=== asynchronous 3")
    
    testCode(new Asynch3ProduceConsume())
    
    }
    
    mainTaskRunner.waitUntilFinished()
    
    /*
      expect output:
      """
      starting up...
      inside rule body (exchanging value item 1)
      received: item 1
      inside rule body (exchanging value item 2)
      received: item 2
      inside rule body (exchanging value item 3)
      received: item 3
      inside rule body (exchanging value item 4)
      received: item 4
      => result: (done producing,done consuming)
      """
    */
  }
}
