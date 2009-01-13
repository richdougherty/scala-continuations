// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.collection.mutable._


object Test8 {
  
  def dprintln(x: =>String): Unit = {
//    println(x)
  }
  
  class Rule[A,B](val body: PartialFunction[ValBase[A,B], Any @cps[Unit,Unit]]) {
  	val channels = new ArrayBuffer[Channel[_,_]]()
	
  	def activateIfPossible() = {
  		// TODO: test all combinations?
		
  		while (body.isDefinedAt(ActivatePreCheck[A,B]())) {
  			run(body(ActivateExecute[A,B]()))
  		}
  	}
  }



  abstract class ValBase[A,B] // TODO: variance

  case class ActivatePreCheck[A,B] extends ValBase[A,B]
  case class ActivateExecute[A,B] extends ValBase[A,B]

  case class Definator[A,B](rule: Rule[A,B]) extends ValBase[A,B]

  case class ValWithCont[A,B](v: A, c: (B=>(Any @suspendable))) extends ValBase[A,B]

  class Channel[A,B] extends (A => (Any @suspendable)){ // FIXME: should be B @suspendable
  	val values = new ArrayBuffer[ValWithCont[A, B]]()
  	val rules = new ArrayBuffer[Rule[_,_]]()
	
  	override def apply(v: A):B @suspendable = {
  		shift { k:(B=>Unit) =>
/*
    		val c = new Channel[B,Unit];
    		Join.rule { case c(x !? r) => k(x) }

        val w = ValWithCont(v, c)
*/
    		val w = ValWithCont(v, (x:B) => shiftUnit(k(x))) // FIXME: need manual eta-expansion
    		values.append(w)

    		for (val r <- rules)
    			r.activateIfPossible();
  		}
  	}

  	def unapply(v: ValBase[_,_]): Option[ValBase[A,B]] = v match { // FIXME: result should be WithCont
  		case Definator(rule) =>
  		  rules.prepend(rule);
  		  rule.channels.append(this);
        // is it made sure that all other channels are inited?
  		  Some(v.asInstanceOf[Definator[A,B]])
      
  		case ActivatePreCheck() => if (values.length > 0) Some(values(0))  else None
  		case ActivateExecute() => if (values.length > 0) Some(values.remove(0)) else None
  	}
	
  }

  type ==>[A,B] = Channel[A,B]

  object <&> {
  	def unapply(v: Any) = {
  	  Some(v,v)
    }
  }

  object !? {
  	def unapply[A,B](v: ValBase[A,B]): Option[(A, (B=>(Any @suspendable)))] = v match {
  		case ValWithCont(x, c) => Some((x,c))
  		case Definator(rule) => Some((null.asInstanceOf[A],null.asInstanceOf))
  	}
  }


  object Join {

    def rule(pf: PartialFunction[Any, Any @cps[Unit,Unit]]) = {
    	val rule = new Rule[Any,Any](pf)
    	pf.isDefinedAt(Definator(rule))

      rule.activateIfPossible()
    }
  
  }

  case class ParSeq[A](f: ()=>(A @cps[Unit,Unit])) {
    def <&>[B](x: =>(B @cps[Unit,Unit])): Pair[A,B] @cps[Unit,Unit] = {
      parallel(f(), x)
    }
  }
  
  def parallel[A,B](a: =>(A @cps[Unit,Unit]), 
		    b: =>(B @cps[Unit,Unit])): Pair[A,B] @cps[Unit,Unit] = {

      dprintln("BEGIN")

      val u = new Channel[A,Unit]
      val v = new Channel[B,Unit]

      shift { k:(((A,B))=>Unit) =>
        
        Join.rule {
          case u(x !? ru) <&> v(y !? rv) =>
            dprintln("END")
            k((x,y))
        }
        
        run(u(a))
        run(v(b))
      }
  }

  implicit def block2ParSeq[A](x: =>(A @cps[Unit,Unit])) = ParSeq(() => x)

  
  // -----------------------------------------------------------------------------
  // client code starts here
  // -----------------------------------------------------------------------------

  abstract class ProduceConsume[A] {
    val produce = new (A ==> Unit)
    val consume = new (Unit ==> A)
  }

  class SynchProduceConsume[A] extends ProduceConsume[A] {
    Join.rule {
      case produce(x !? return_put) <&> consume(_ !? return_get) => 
        println("inside rule body (exchanging value " + x + ")")
        return_put() <&> return_get(x)
    }
  }


  class Asynch1ProduceConsume[A] extends ProduceConsume[A] {
    
    val item = new (A ==> Unit)
    
    Join.rule {
      case produce(x !? return_put) => 
        return_put() <&> item(x)
    }

    Join.rule {
      case consume(_ !? return_get) <&> item(x !? _) => 
        println("inside rule body (exchanging value " + x + ")")
        return_get(x)
    }
  }

  class Asynch2ProduceConsume[A] extends ProduceConsume[A] {
    
    val item = new (List[A] ==> Unit)
    
    Join.rule {
      case produce(x !? return_put) & item(xs !? _)=> 
        return_put() <&> item(xs ::: List(x))
    }

    Join.rule {
      case consume(_ !? return_get) <&> item((x::xs) !? _) => 
        println("inside rule body (exchanging value " + x + ")")
        return_get(x) <&> item(xs)
    }

    run {
      item(Nil)
    }
  }

  class Asynch3ProduceConsume[A] extends ProduceConsume[A] {
    
    case class Elem(x: A, next: Unit ==> Elem)
    
    val last = new ((Unit ==> Elem) ==> Unit)
    val first = new ((Unit ==> Elem) ==> Unit)
    
    Join.rule {
      case produce(x !? return_put) & last(elem !? _) => 
        val next = new (Unit ==> Elem)
        Join.rule { 
          case elem(_ !? return_elem) => return_elem(Elem(x, next))
        }
        return_put() <&> last(next)
    }

    Join.rule {
      case consume(_ !? return_get) <&> first(elem !? _) =>
        val data = elem()
        println("inside rule body (exchanging value " + data.x + ")")
        return_get(data.x) <&> first(data.next)
    }

    run {
      val elem = new (Unit ==> Elem)
      first(elem) <&> last(elem)
    }
  }



  def testCode(p: ProduceConsume[String]): Unit = run {

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

    println("=== synchronous")

    testCode(new SynchProduceConsume())

    println()
    println("=== asynchronous 1")
    
    testCode(new Asynch1ProduceConsume())

    println()
    println("=== asynchronous 2")
    
    testCode(new Asynch2ProduceConsume())

    println()
    println("=== asynchronous 3")
    
    testCode(new Asynch3ProduceConsume())
    
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
