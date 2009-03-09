// $Id$

package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Map

import scala.concurrent._
import scala.concurrent.cpsops._

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicInteger

trait JoinPatterns { self: TaskRunners =>
  
//  type TaskRunner <: AbstractSequentialTaskRunner
  

  def dprintln(x: =>String): Unit = {
//    println(x)
  }


  abstract class ValBase[A,B] // TODO: variance

  case class MatchData(var value: ValBase[_,_])

  case class ActivatePreCheck[A,B](items: Map[Channel[_,_], MatchData]) extends ValBase[A,B]
  case class ActivateExecute[A,B](items: Map[Channel[_,_], MatchData]) extends ValBase[A,B]

  case class Definator[A,B](rule: Rule[A,B]) extends ValBase[A,B]

  case class ValWithCont[A,B](v: A, c: (B=>(Any @suspendable))) extends ValBase[A,B]

  var nChannels = new AtomicInteger(0)

  class Channel[A,B](name:String) extends (A => (Any @suspendable)){ // FIXME: should be B @suspendable
  	val values = new ArrayBuffer[ValWithCont[A, B]]()
  	val rules = new ArrayBuffer[Rule[_,_]]()
	
	  val lock = new ReentrantLock()
	
	  val id = nChannels.getAndIncrement()
	  override def toString = name + id
	
	  def this() = this("chan")
	
  	override def apply(v: A):B @suspendable = {
  		shift { k:(B=>Unit) =>
/*
    		val c = new Channel[B,Unit];
    		Join.rule { case c(x !? r) => k(x) }

        val w = ValWithCont(v, c)
*/
        dprintln("[!lock " + this + "!]")
        lock.lock()
        
    		val w = ValWithCont(v, (x:B) => shiftUnit(k(x))) // FIXME: need manual eta-expansion
    		values.append(w)

        dprintln("[!unlock " + this + "!]")
        lock.unlock()

    		for (val r <- rules)
    			r.activateIfPossible();
  		}
  	}

  	def unapply(v: ValBase[_,_]): Option[ValBase[A,B]] = v match { // FIXME: result should be WithCont
  		case Definator(rule) =>
  		  rule.channels.append(this);
        // can we be sure that all other channels are inited?
  		  Some(v.asInstanceOf[Definator[A,B]])
      
  		case ActivatePreCheck(items) =>
  		  Some(items(this).value.asInstanceOf[ValBase[A,B]])
  		case ActivateExecute(items) =>
		    Some(items(this).value.asInstanceOf[ValBase[A,B]])
  	}
	
  }

  class Rule[A,B](val body: PartialFunction[ValBase[A,B], Any @cps[Unit,Unit]]) {
  	val channels = new ArrayBuffer[Channel[_,_]]()
	
  	def activateIfPossible() = {
  		// TODO: test all combinations?
		
		  // lock 'em all (list should be sorted ...)
      for (c <- channels) { dprintln("[lock " + c + "]"); c.lock.lock() }
		
		  var loop:Boolean = true
		  do {
		  
  		  var items = Map[Channel[_,_], MatchData]()
  		  for (c <- channels; if !c.values.isEmpty) {
  		    items += (c.asInstanceOf[Channel[_,_]] -> MatchData(c.values(0)))
  		  }
		
  		  if (items.size == channels.length && body.isDefinedAt(ActivatePreCheck[A,B](items))) {

    		  for (c <- channels) c.values.remove(0)

          spawn(body(ActivateExecute[A,B](items)))
		    
  		  } else {
  		    loop = false
  		  }
		
      } while (loop)
      
		  // unlock 'em all
      for (c <- channels.reverse) { dprintln("[unlock " + c + "]"); c.lock.unlock() }
  	}
  }

  def join(pf: PartialFunction[Any, Any @cps[Unit,Unit]]) = {
  	val rule = new Rule[Any,Any](pf)
  	pf.isDefinedAt(Definator(rule))

    for (c <- rule.channels)
	    c.rules.prepend(rule); // TOOD: should be locked ?!?

    rule.activateIfPossible()
  }

  type ==>[A,B] = Channel[A,B]

  object <&> {
  	def unapply(v: Any) = {
  	  Some(v,v)
    }
  }

  object <== {
  	def unapply[A,B](v: ValBase[A,B]): Option[(A, (B=>(Any @suspendable)))] = v match {
  		case ValWithCont(x, c) => Some((x,c))
  		case Definator(rule) => Some((null.asInstanceOf[A],null.asInstanceOf))
  	}
  }


  // parallel blocks

  case class ParSeq[A](f: ()=>(A @cps[Unit,Unit])) {
    def <&>[B](x: =>(B @cps[Unit,Unit])): Pair[A,B] @cps[Unit,Unit] = {
      parallel(f(), x)
    }
  }
  

  def parallel[A,B](a: =>(A @cps[Unit,Unit]), 
		    b: =>(B @cps[Unit,Unit])): Pair[A,B] @cps[Unit,Unit] = {

      dprintln("BEGIN")

      val u = new Channel[A,Unit]("u")
      val v = new Channel[B,Unit]("v")

      shift { k:(((A,B))=>Unit) =>
        
        join {
          case u(x <== ru) <&> v(y <== rv) =>
            dprintln("END")
            k((x,y))
        }
        
        spawn(u(a))
        spawn(v(b))
      }
  }

  implicit def block2ParSeq[A](x: =>(A @cps[Unit,Unit])) = ParSeq(() => x)

}