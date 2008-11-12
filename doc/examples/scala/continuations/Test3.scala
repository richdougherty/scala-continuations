// $Id$

import scala.continuations._
import scala.continuations.ControlContext._

import scala.collection.mutable._


// FIXME: breaks with current version, there seems to be a
// problem with case classes in selectiveanf


case class Definator(rule: Rule);

class Rule(val body: PartialFunction[Any, Any]) {
	val channels = new ArrayBuffer[Channel]()
	
	def activateIfPossible() = {
		// TODO: test all combinations?
		
		while (body.isDefinedAt(false)) {
			body(true)
		}
	}
}


case class WithCont(v: Any, c: Channel);

class Parallel(left: Channel, right: Channel) {

	def !?(v: Any, w: Any):Any @cps[Any,Any] = {
		shift((k:Any => Any) => {
  		val c = new Channel("c");
  		val d = new Channel("d");
  		Join.rule { case c(x) & d(y) => k((x,y)) }

  		this!(WithCont(v, c), WithCont(w, d))
		})
	}

	def !(v: Any, w: Any) = {
		left.values.append(v)
		right.values.append(w)

		for (val r <- left.rules)
			r.activateIfPossible();
		for (val r <- right.rules)
			r.activateIfPossible();
	}

}





class Channel(val name: String) {
	val values = new ArrayBuffer[Any]()
	val rules = new ArrayBuffer[Rule]()
	
	def !?(v: Any):Any @cps[Any,Any] = {
		shift((k:Any => Any) => {
  		val c = new Channel("c");
  		Join.rule { case c(x) => k(x) }

  		this!(WithCont(v, c))
		})
	}

	def !(v: Any) = {
		values.append(v)

		for (val r <- rules)
			r.activateIfPossible();
	}


  def &(c: Channel) = {
    new Parallel(this, c)
  }


	
	def unapply(v: Any) = v match {
		case Definator(rule) => rules.prepend(rule); rule.channels.append(this); Some(v)	// is it made sure that all
																							// other channels are inited?

		case false => if (values.length > 0) Some(values(0))  else None
		case true => if (values.length > 0) Some(values.remove(0)) else None
	}
	
}

object & {
	def unapply(v: Any) = Some((v,v))
}

object !? {
	def unapply(v: Any) = v match {
		case WithCont(x, c) => Some((x,c))
	}
}


object Join {

  def rule(pf: PartialFunction[Any, Any]) = {
  	val rule = new Rule(pf)
  	pf.isDefinedAt(Definator(rule))
	
//  rule.activateIfPossible();
  }

}


object Test3 {

  def testCode(): Any @cps[Any,Any] = {
    val put = new Channel("put")
    val get = new Channel("get")

    Join.rule { case put(x !? return_put) & get(y !? return_get)=> 
      println("inside rule body..."); (return_put & return_get)!((),x) }


    println("parallel: put(Blabla) & get()")

    val combined = (put & get)

    val res = combined!?(("data"),())

    println("=> result: " + res)
  }

  def main(args: Array[String]) {

    val x = reset[Any,Any](testCode())
    
    ()

    /*
      expect output:
      """
      parallel: put(Blabla) & get()
      inside rule body...
      => result: ((),Blabla)
      """
    */
  }
}
