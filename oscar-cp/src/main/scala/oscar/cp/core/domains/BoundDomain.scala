package oscar.cp.core.domains

import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPOutcome
import oscar.algo.reversible.ReversiblePointer
import scala.util.Random

/**
 *  @author Renaud Hartert
 *  @author Pierre Schaus
 */

class BoundDomain(override val context: ReversibleContext, val minValue: Int, val maxValue: Int) extends IntervalDomain {

  private val _maxValue = if (maxValue - minValue - 1 < Int.MaxValue) maxValue
  else sys.error("the domain contains more than Int.MaxValue values")

  private val _min = new ReversibleInt(context, minValue)
  private val _max = new ReversibleInt(context, _maxValue)

  override def size: Int = _max - _min + 1
  
  override def isEmpty: Boolean = _max.value < _min.value

  override def isBound: Boolean = _max.value == _min.value

  override def min: Int = {
    if (isEmpty) throw new NoSuchElementException("the domain is empty")
    else _min.value
  }

  override def max: Int = {
    if (isEmpty) throw new NoSuchElementException("the domain is empty")
    else _max.value
  }

  override def randomValue(rand: Random): Int = {
    if (isEmpty) throw new NoSuchElementException("the domain is empty")
    else {
      val minVal = _min.value
      minVal + rand.nextInt(_max.value - minVal + 1)
    }
  }

  /**
   * @param value
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */
  override def nextValue(value: Int): Int = {
    if (isEmpty || value > _max) value - 1
    else if (value < _min) _min
    else value
  }

  /**
   * @param value
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */
  override def prevValue(value: Int): Int = {
    if (isEmpty || value < _min) value + 1
    else if (value > _max) _max
    else value
  }

  override def hasValue(value: Int) = {
    assert(!isEmpty)
    value <= _max.value && value >= _min.value
  }

  override def iterator: Iterator[Int] = (_min.value to _max.value).iterator

  override def updateMax(value: Int): CPOutcome = {
    if (value < _min.value) {
      _max.value = _min.value - 1
      Failure
    } else if (value >= _max.value) Suspend
    else {
      _max.value = value
      Suspend
    }
  }

  override def updateMin(value: Int): CPOutcome = {
    if (value > _max.value) {
      _min.value = _max.value + 1
      Failure
    } else if (value <= _min.value) Suspend
    else {
      _min.value = value
      Suspend
    }
  }

  override def assign(value: Int): CPOutcome = {
    if (!hasValue(value)) {
      _max.value = value
      _min.value = value + 1
      Failure
    } else {
      _min.value = value
      _max.value = value
      Suspend
    }
  }

  override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    (oldMin to _min.value - 1).iterator ++ (_max.value + 1 to oldMax).iterator
  }

  override def toString: String = {
    if (isEmpty) "phi"
    else "{" + _min.value + ".." + _max.value + "}"
  }
}