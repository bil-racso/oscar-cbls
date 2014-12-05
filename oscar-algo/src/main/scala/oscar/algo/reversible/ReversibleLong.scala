package oscar.algo.reversible

/**
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class ReversibleLong(node: ReversibleContext, value: Long) extends Reversible[Long](node, value) {

  /** Increments the reversible integer by one */
  def incr(): Long = {
    trail()
    pointer += 1
    pointer
  }

  /** Decrements the reversible integer by one */
  def decr(): Long = {
    trail()
    pointer -= 1
    pointer
  }

  /** Increments the reversible integer by i */
  def +=(i: Long): Long = {
    trail()
    pointer += i
    pointer
  }

  /** Decrements the reversible integer by i */
  def -=(i: Long): Long = {
    trail()
    pointer -= i
    pointer
  }
}

object ReversibleLong {
  def apply(value: Long)(implicit context: ReversibleContext) = new ReversibleLong(context, value)
  implicit def reversibleLong2Long(ri: ReversibleLong): Long = ri.getValue
}