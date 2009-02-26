package scala.collections.immutable

import java.io.IOException
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.nio.ByteBuffer

import java.nio.ByteBuffer

private[immutable] object CompositeBinary {

  /**
   * Precalculated array containing all positive Int values of the
   * Fibonacci sequence.
   */
  private val fibonacci: Array[Int] = {
    def stream: Stream[Long] = {
      def tailAfter(a: Long, b: Long): Stream[Long] = {
        val c = a + b
        Stream.cons(c, tailAfter(b, c))
      }
      val first: Long = 0
      val second: Long = 1
      Stream.cons(first, Stream.cons(second, tailAfter(first, second)))
    }
    stream takeWhile { _ <= Integer.MAX_VALUE } map { _.asInstanceOf[Int] } toArray
  }

}

/**
 * A Binary object composed of two child Binary objects.
 *
 * @author <a href="http://www.richdougherty.com/">Rich Dougherty</a>
 */
@serializable
@SerialVersionUID(-8314444781809819098L)
private[immutable] final case class CompositeBinary private[immutable] (private var left: Binary, private val right: Binary) extends Binary with Serializable {

  override val length = left.length + right.length

  override def apply(i: Int) = if (i < left.length) left(i) else right(i - left.length)

  // Cache, because calculation would otherwise be O(log n).
  // Use Byte to save space since max depth is 47 (or 48 before rebalancing).
  private var cachedDepth: Byte = (Math.max(left.depth, right.depth) + 1).asInstanceOf[Byte]

  private[immutable] def depth = cachedDepth.asInstanceOf[Int]

  private[immutable] def isBalanced: Boolean = {
    import CompositeBinary.fibonacci
    ((depth + 2) >= fibonacci.length) || (length >= fibonacci(depth + 2))
  }

  private[immutable] def rebalance: Binary = {
    def process(array: Array[Binary]): Binary = {
      array.length match {
        case 0 => Binary.empty
        case 1 => array(0)
        case 2 => new CompositeBinary(array(0), array(1))
        case n => {
          val midPoint = n / 2
          val left = array.slice(0, midPoint)
          val right = array.slice(midPoint, n)
          new CompositeBinary(process(left), process(right))
        }
      }
    }
    // XXX: Avoid iterating over arrays twice.
    process(arrays(0, length).toList.toArray)
  }

  protected def forcedSlice0(from: Int, until: Int): Binary = {
    if (until <= left.length) {
      // Slice is completely contained by left component.
      left.forcedSlice(from, until)
    } else if (from >= left.length) {
      // Slice is completely contained by right component.
      right.forcedSlice(from - left.length, until - left.length)
    } else {
      new Binary {

        override def length = until - from

        override private[immutable] def depth = CompositeBinary.this.depth

        override def apply(i: Int) = {
          if (i < 0 || i >= length) throw new IndexOutOfBoundsException
          else CompositeBinary.this.apply(from + i)
        }

        protected def copyToArray0(from2: Int, until2: Int, dest: Array[Byte], destFrom: Int): Unit = {
          CompositeBinary.this.copyToArray0(from + from2, from + until2, dest, destFrom)
        }

        protected def arrays0(from2: Int, until2: Int): Iterable[ArrayBinary] = {
          CompositeBinary.this.arrays0(from + from2, from + until2)
        }

        protected def forcedSlice0(from2: Int, until2: Int): Binary = {
          CompositeBinary.this.forcedSlice0(from + from2, from + until2)
        }
      } 
    }
  }

  protected def copyToArray0(from: Int, until: Int, dest: Array[Byte], destFrom: Int): Unit = {
    //println("CompositeBinary"+(left.length, right.length)+".copyToArray0"+(from: Int, until: Int, dest: Array[Byte], destFrom: Int))
    import Math._
    if (from < left.length) {
      left.copyToArray(from, min(until, left.length), dest, destFrom)
    }
    if (until > left.length) {
      right.copyToArray(max(from, left.length) - left.length, until - left.length, dest, destFrom + left.length)
    }
  }

  protected def arrays0(from: Int, until: Int): Iterable[ArrayBinary] = {
    import Math._
    val leftArrays = if (from < left.length) {
      left.arrays(from, min(until, left.length))
    } else {
      Nil
    }
    val rightArrays = if (until > left.length) {
      right.arrays(max(from, left.length) - left.length, until - left.length)
    } else {
      Nil
    }
    leftArrays ++ rightArrays
  }

  override def elements: Iterator[Byte] = (left.elements ++ right.elements)

  private def writeObject(out: ObjectOutputStream): Unit = {
    out.writeInt(length)
    for (arrayBinary <- arrays) {
      out.write(arrayBinary.array, arrayBinary.offset, arrayBinary.length)
    }
  }

  private def readObject(in: ObjectInputStream): Unit = {
    // Read into left so it can be used as the replacement object.
    // See readResolve.
    val offset = 0
    val length = in.readInt()
    val array = new Array[Byte](length)
    var remaining = length
    while (remaining > 0) {
      val readLength = in.read(array, length - remaining, remaining)
      if (readLength == -1) {
        throw new IOException("Expected " + remaining + " more bytes.")
      }
      remaining -= readLength
    }
    left = new ArrayBinary(array, 0, length)
  }
  
  private def readResolve: AnyRef = left

}
