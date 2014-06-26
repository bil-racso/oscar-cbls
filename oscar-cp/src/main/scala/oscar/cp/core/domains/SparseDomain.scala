package oscar.cp.core.domains

import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPOutcome
import scala.util.Random

/** 
 *  @author Renaud Hartert 
 *  @author Pierre Schaus
 */
class SparseDomain(s: ReversibleContext, val minValue: Int, val maxValue: Int) extends IntDomain {

  private val offset = minValue

  private val _min = new ReversibleInt(s, minValue)
  private val _max = new ReversibleInt(s, maxValue)
  // Values with a position strictly lower than size are possible
  private val _size = new ReversibleInt(s, maxValue - minValue + 1)

  private val values = Array.tabulate(size)(i => i)
  private val indexes = Array.tabulate(size)(i => i)

  override def size: Int = _size.value
  
  override def isBound: Boolean = _size.value == 1

  override def min: Int = {
    val v = _min.value
    if (hasValue(v)) v
    else updateMinValue()
  }

  override def max: Int = {    
    val v = _max.value
    if (hasValue(v)) v
    else updateMaxValue()
  }

  override def randomValue(rand: Random): Int = {
    val i = rand.nextInt(_size.value)
    values(i)
  }
  
  private def checkVal(v: Int): Boolean = {
    v >= offset &&
    v <= offset + values.size - 1
  }

  override def hasValue(v: Int): Boolean = {
    if (v < offset || v >= offset + indexes.size) false
    else indexes(v - offset) < _size.value
  }

  override def isEmpty = _size.value == 0

  @inline
  private def exchangePositions(val1: Int, val2: Int): Unit = {
    assert(checkVal(val1))
    assert(checkVal(val2))

    val v1 = val1 - offset
    val v2 = val2 - offset
    val i1 = indexes(v1)
    val i2 = indexes(v2)
    values(i1) = v2
    values(i2) = v1
    indexes(v1) = i2
    indexes(v2) = i1
  }

  private def updateMaxValue(): Int = {
    var i = 0
    var max = values(i)
    val s = _size.value
    while (i < s) {
      val v = values(i)
      if (v > max) max = v
      i += 1
    }
    val newMax = max + offset
    _max.value = newMax
    newMax
  }

  private def updateMinValue(): Int = {
    var i = 0
    var min = values(i)
    val s = _size.value
    while (i < s) {
      val v = values(i)
      if (v < min) min = v
      i += 1
    }
    val newMin = min + offset
    _min.value = newMin
    newMin
  }
  
  override def iterator: Iterator[Int] = {
    var iterIndex = 0
    new Iterator[Int] {
      def next(): Int = {
        val i = iterIndex
        iterIndex += 1
        values(i) + offset
      }
      def hasNext: Boolean = {
        iterIndex < _size.value
      }
    }
  }

  override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    (oldMin until minValue).iterator ++ delta(oldSize) ++ (maxValue + 1 to oldMax).iterator
  }

  private def delta(oldSize: Int): Iterator[Int] = {
    var ind = size
    new Iterator[Int] {
      override def next(): Int = {
        val v = values(ind)
        ind += 1
        v + offset
      }
      override def hasNext: Boolean = {
        ind < oldSize && ind < values.size
      }
    }
  }
  
  override def removeValue(v: Int): CPOutcome = {  
    assert(checkVal(v))
    if (!hasValue(v)) Suspend
    else {
      val s = _size.value - 1
      exchangePositions(v, values(s) + offset)
      _size.value = s
      if (s == 0) Failure
      else Suspend    
    }
  }

  /**
   * @param value
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */
  override def nextValue(v: Int): Int = {
    assert(checkVal(v))
    assert(!isEmpty)

    var cv = v
    while (cv <= max) {
      if (hasValue(cv)) {
        return cv
      }
      cv += 1
    }
    v - 1
  }

  /**
   * @param value
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */
  override def prevValue(v: Int): Int = {
    assert(checkVal(v))
    assert(!isEmpty)

    var cv = v
    while (cv >= min) {
      if (hasValue(cv)) {
        return cv
      }
      cv -= 1
    }
    v + 1
  }

  override def assign(v: Int): CPOutcome = {
    // we only have to put in first position this value and set the size to 1
    assert(checkVal(v));

    if (!hasValue(v)) Failure
    else {
      val value = values(0)
      val index = indexes(v - offset)
      indexes(v - offset) = 0
      values(0) = v - offset
      indexes(value) = index
      values(index) = value
      _min.value = v
      _max.value = v
      _size.value = 1
      Suspend
    }
  }

  override def updateMin(minv: Int): CPOutcome = {
    assert(checkVal(minv))
    assert(!isEmpty)

    val minVal = min // Lazy update
    if (minv <= minVal) Suspend
    else {
      val maxVal = max // Lazy update
      if (minv > maxVal) {
        _size.value = 0
        Failure
      }
      else if (minv == maxVal) assign(minv)
      else {
        var cv = minVal
        while (cv < minv) {
          removeValue(cv)
          cv += 1
        }
        if (hasValue(minv)) _min.value = minv
        Suspend
      }
    }
  }
  
  override def updateMax(maxv: Int): CPOutcome = {
    assert(checkVal(maxv))
    assert(!isEmpty)
    
    val maxVal = max // Lazy update
    if (maxv >= maxVal) Suspend
    else {
      val minVal = min // Lazy update
      if (maxv < minVal) {
        _size.value = 0
        Failure
      }
      else if (maxv == minVal) assign(maxv)
      else {
        var cv = maxVal
        while (cv > maxv) {
          removeValue(cv)
          cv -= 1
        }
        if (hasValue(maxv)) _max.value = maxv
        Suspend
      }
    }
  }
  
  override def toString: String = {
    if (isEmpty) "phi"
    else "{" + this.mkString(", ") + "}"
  }
}
