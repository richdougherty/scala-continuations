package examples.continuations

import scala.continuations._
import scala.continuations.ControlContext._

object DepthFirst {

  private object BacktrackThunk extends Throwable

  private sealed trait Result
  private case object Backtrack extends Result
  private case object Success extends Result
  private case object TrySibling extends Result

  def choose[A,B](i: Iterable[A]): A @suspendable = {
    shift { k: (A => Unit) =>
      val elements = i.elements
      def next: Unit = {
        val result = try {
          if (elements.hasNext) {
            k(elements.next)
            Success
          } else Backtrack
        } catch {
          case BacktrackThunk => TrySibling
        }
        result match {
          case Backtrack => throw BacktrackThunk
          case TrySibling => next
          case Success => ()
        }
      }
      next
    }
  }

  def backtrack: Nothing @suspendable = shift { k: (Nothing => Unit) => throw BacktrackThunk }

}

object RichTest18Backtracking {

  import DepthFirst._

  def main(args: Array[String]): Unit = {
    reset {
      println(factors(15))
      println(nqueens(8))
    }
    

  }

  def factors(n: Int) = {
    val a = choose(1 until n)
    val b = choose(1 until n)
    if (a * b != n) backtrack else shiftUnit[Unit,Unit,Unit](())
    (a, b)
  }

  def printBoard(queens: List[Int]) = {
    val n = queens.length
    for (x <- 0 until n) {
      for (y <- 0 until n) {
        if (queens.zipWithIndex.exists(_ == (x, y))) {
          print("Q")
        } else {
          print(".")
        }
      }
      println
    }
  }

  def nqueens(n: Int): List[Int] @suspendable = {
    def nqueens0(acc: List[Int]): List[Int] @suspendable = {
      if (acc.length >= n) acc
      else {
        val next = choose(0 until n)
        val intersects = acc.zipWithIndex.exists {
          case (pos, index) => pos == next || Math.abs(pos - next) == index + 1
        }
        if (intersects) backtrack
        else nqueens0(next::acc)
      }
    }
    nqueens0(Nil)
  }

}
