package scala.io.iteratee

import scala.collections.immutable.Binary

sealed trait StreamEvent[+E]
case class Chunk[+E](e: E) extends StreamEvent[E]
case object StreamEnd extends StreamEvent[Nothing]
case class StreamError(t: Throwable) extends StreamEvent[Nothing]

/**
 * @see http://okmij.org/ftp/Streams.html#iteratee
 */
object Iteratee {

  /**
   * Rules about how to "chunk" a particular type of element for use
   * in a StreamEvent. Fewer operations may be needed once we have
   * Scala 2.8's collections.
   */
  trait Chunking[T,El] {
    val empty: T
    def length(a: T): Int
    def join(a: T, b: T): T
    def splitAt(a: T, n: Int): (T,T)
    def get(a: T, i: Int): El
    def toList(a: T): List[El]
    object Cons {
      def unapply(a: T): Option[(El, T)] = uncons(a)
    }
    protected def uncons(a: T): Option[(El, T)]
  }
  implicit val binaryChunking: Chunking[Binary,Byte] = new Chunking[Binary,Byte] {
    val empty = Binary.empty
    def length(a: Binary): Int = a.length
    def join(a: Binary, b: Binary) = a ++ b
    def splitAt(a: Binary, n: Int): (Binary,Binary) = a.splitAt(n)
    def get(a: Binary, i: Int) = a(i)
    def toList(a: Binary) = a.toList
    protected def uncons(a: Binary): Option[(Byte, Binary)] =
      if (a.isEmpty) None else Some((a(0), a.slice(1).force))
  }
  implicit def listChunking[A]: Chunking[List[A],A] = new Chunking[List[A],A] {
    val empty = Nil
    def length(a: List[A]): Int = a.length
    def join(a: List[A], b: List[A]) = a ++ b
    def splitAt(a: List[A], n: Int): (List[A],List[A]) = a.splitAt(n)
    def get(a: List[A], i: Int) = a(i)
    def toList(a: List[A]) = a
    protected def uncons(a: List[A]) = if (a.isEmpty) None else Some((a.head, a.tail))
  }

  final def unit[E,El,R](r: R)(implicit chunking: Chunking[E,El]) = IEDone[E,El,R](r, Chunk(chunking.empty))

  def snext[E,El](implicit chunking: Chunking[E,El]): Iteratee[E,El,Option[El]] = {
    def step(stream: StreamEvent[E]): Iteratee[E,El,Option[El]] = stream match {
      case Chunk(chunking.empty) => snext
      case Chunk(chunking.Cons(c, t)) => IEDone(Some(c), Chunk(t))
      case stream => IEDone(None, stream)
    }
    IECont(step _)
  }

  def skipN[E,El](n: Int)(implicit chunking: Chunking[E,El]): Iteratee[E,El,Unit] = {
    if (n == 0) return IEDone((), Chunk(chunking.empty))
    def step(e: StreamEvent[E]): Iteratee[E,El,Unit] = e match {
      case Chunk(s) if (chunking.length(s) <= n) => skipN(n - chunking.length(s))
      case Chunk(s) => chunking.splitAt(s, n) match {
        case (_, s2) => IEDone((), Chunk(s2))
      }
      case e => Iteratee.unit(())
    }
    IECont(step _)
  }

  def streamToList[E,El](implicit chunking: Chunking[E,El]): Iteratee[E,El,List[El]] = {
    def step(acc: List[El], e: StreamEvent[E]): Iteratee[E,El,List[El]] = e match {
      case Chunk(chunking.empty) => IECont(step(acc, _))
      case Chunk(c) => IECont(step(acc ++ chunking.toList(c), _))
      case e => IEDone(acc, e)
    }
    IECont(step(Nil, _: StreamEvent[E]))
  }

  def join[E,El](implicit chunking: Chunking[E,El]): Iteratee[E,El,E] = {
    def step(acc: E, e: StreamEvent[E]): Iteratee[E,El,E] = e match {
      case Chunk(chunking.empty) => IECont(step(acc, _))
      case Chunk(c) => IECont(step(chunking.join(acc, c), _))
      case e => IEDone(acc, e)
    }
    IECont(step(chunking.empty, _: StreamEvent[E]))
  }

  def stake[E,El,R](n: Int, iter: Iteratee[E,El,R])(implicit chunking: Chunking[E,El]): Iteratee[E,El,Iteratee[E,El,R]] = (n, iter) match {
    case (n, IEDone(_, _)) => {
      skipN[E,El](n) >> Iteratee.unit(iter)
    }
    case (0, iter) => Iteratee.unit(iter)
    case (n, IECont(k)) => {
      def step(stream: StreamEvent[E]): Iteratee[E,El,Iteratee[E,El,R]] = stream match {
        case Chunk(chunking.empty) => IECont(step _)
        case chunk @ Chunk(cs) if (chunking.length(cs) <= n) => stake(n - chunking.length(cs), k(chunk))
        case Chunk(cs) => chunking.splitAt(cs, n) match {
          case (s1, s2) => IEDone(k(Chunk(s1)), Chunk(s2))
        }
        case stream => IEDone(k(stream), stream)
      }
      IECont(step _)
    }
  }

  /*def breakBefore[E](delimiter: E)(implicit chunking: Chunking[E,El]): Iteratee[E,(E,Option[E])] = {
    def step(acc: E, ) = {

    }
    IECont(step(chunking.empty, _))
  }*/

}

/**
 * An object which can iterate over a stream of <code>E</code>
 * objects, ultimately returning an <code>R</code>.
 *
 * @see http://okmij.org/ftp/Streams.html#iteratee
 */
sealed trait Iteratee[E,El,+R] {
  import Iteratee._
  def >>=[S](f: R => Iteratee[E,El,S])(implicit chunking: Chunking[E,El]): Iteratee[E,El,S]
  def >>[S](f: => Iteratee[E,El,S])(implicit chunking: Chunking[E,El]): Iteratee[E,El,S] = >>=(r => f) // XXX: Correct definition?
  // This relationship due to Luc Duponcheel
  // http://lucdup.blogspot.com/2008/11/scala-monads-and-arrows.html
  //final def map[S](f: R => S): Iteratee[E,S] = >>=(r => unit(f(r)))
  //final def flatMap[S](f: A => Iteratee[E,S]): Iteratee[E,S] = >>=(f)
}
final case class IEDone[E,El,+R](r: R, unprocessed: StreamEvent[E]) extends Iteratee[E,El,R] {
  import Iteratee._
  final def >>=[S](f: R => Iteratee[E,El,S])(implicit chunking: Chunking[E,El]): Iteratee[E,El,S] = {
    val rightIteratee = f(r)
    if (unprocessed == Chunk(chunking.empty)) {
      rightIteratee
    } else {
      rightIteratee match {
        case IEDone(s, _) => IEDone[E,El,S](s, unprocessed)
        case IECont(k) => k(unprocessed)
      }
    }
  }
}
final case class IECont[E,El,+R](k: StreamEvent[E] => Iteratee[E,El,R]) extends Iteratee[E,El,R] {
  import Iteratee._
  final def >>=[S](f: R => Iteratee[E,El,S])(implicit chunking: Chunking[E,El]): Iteratee[E,El,S] = IECont[E,El,S](k(_: StreamEvent[E]) >>= (f))
}

/*trait AsyncProcessor[+E,-R] {
  def process(e: StreamEvent[E]): AsyncFunction0[ProcessingResult[E,R]]
  def >>=[S](f: R => AsyncProcessor[E,S]): AsyncProcessor[E,S] = new AsyncProcessor[E,S] {
    def process(e: StreamEvent[E]): AsyncFunction0[ProcessingResult[E,S] = {
      AsyncProcessor.this.process(e) /- async1 { pr: ProcessingResult[E,R] => pr match {
          case Done(r, None) => async0 { f(r) }
          case Done(r, Option(unprocessed)) => f(r).process(unprocessed) 
        }
      }
    }
  }
}*/

