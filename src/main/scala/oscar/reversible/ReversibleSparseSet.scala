/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/


package oscar.reversible

import scala.collection.Iterable
import scala.collection.Iterator

/**
 * Initializes a set with all values min..max in it
 * @param s
 * @param min
 * @param max >= min
 * @author Pierre Schaus
 */
class ReversibleSparseSet(s: ReversibleSearchNode, val minValue: Int, val maxValue: Int) extends Iterable[Int] {

  val offset = minValue
  val _min = new ReversibleInt(s,minValue)
  val _max = new ReversibleInt(s,maxValue)
  val _size = new ReversibleInt(s,maxValue-minValue+1)
  
  val values = Array.tabulate(size)(i => i)
  val indexes = Array.tabulate(size)(i => i)
  
  
  override def size: Int = _size.value
  private def size_=(v: Int) {
    _size.value = v
  }
  
  def min: Int = {
    if (!hasValue(_min.value)) updateMinValRemoved(_min.value)
    _min.value
  }
  private def min_=(v: Int) {
    _min.value = v
  }

  def max: Int = {
    if (!hasValue(_max.value)) updateMaxValRemoved(_max.value)
    _max.value
  }
  
  
  private def max_=(v: Int) {
    _max.value = v
  }

  private def checkVal(v: Int) = {
    assert(v >= offset)
    assert(v <= offset + values.size - 1)
    true
  }
  
  def hasValue(v: Int) = {
		if (v < offset || v >= offset+indexes.size) false
		else indexes(v-offset) < size;
  }  
  
  override def isEmpty = size == 0

  /**
   * remove all elements in the set
   */
  def empty() {
    size = 0
  }

  private def exchangePositions(val1: Int, val2: Int) {
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
  
  private def updateBoundsValRemoved(v: Int) {
	updateMaxValRemoved(v)
	updateMinValRemoved(v)
  }

  private def updateMaxValRemoved(v: Int) {
    
    /*
    if (!isEmpty) {
      assert(!hasValue(v));
      var cv = v
      while (cv >= _min.value) {
        if (hasValue(cv)) {
          max = cv
          return
        }
        cv -= 1
      }
    }*/
    
    if (!isEmpty) max = iterator.max
  }

  private def updateMinValRemoved(v: Int) {
    
    /*
    if (!isEmpty) {
      var cv = v
      while (cv <= _max.value) {
        if (hasValue(cv)) {
          min = cv
          return
        }
        cv += 1
      }
    }*/
    
    if (!isEmpty) min = iterator.min
  }

  def removeValue(v: Int): Boolean = {
    assert(checkVal(v))
    if (!hasValue(v)) return false; //the value has already been removed
    exchangePositions(v, values(size - 1) + offset)
    _size.decr()
    return true;
  }

  /**
   * @param value
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */  
  def nextValue(v: Int): Int = {
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
  def prevValue(v: Int): Int = {
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

  def removeAllBut(v: Int) {
    // we only have to put in first position this value and set the size to 1
    assert(checkVal(v));
    assert(hasValue(v));
    val value = values(0)
    val index = indexes(v - offset)
    indexes(v - offset) = 0
    values(0) = v - offset
    indexes(value) = index
    values(index) = value
    min = v
    max = v
    size = 1
  } 

  def updateMin(minv: Int): Unit = {
    assert(checkVal(minv))
    assert(!isEmpty)
    if (minv < min) {
      return // the min does not change
    } else if (minv > max) {
      size = 0 // the set becomes empty since the new min is larger than the current max
    } else if (minv == max) {
      // the new min is equal to the current max hence only one value in the set
      removeAllBut(minv)
    } else {
      var cv = min
      while (cv < minv) {
        removeValue(cv)
        cv += 1
      }
      if (hasValue(minv)) {
        min = minv
      }
    }
  }

  def updateMax(maxv: Int): Unit = {
    assert(checkVal(max));
    assert(!isEmpty);
    if (maxv >= max) {
      return
    } else if (maxv < min) {
      size = 0 // the set becomes empty since the new max is smaller than the current min
    } else if (maxv == min) {
      // the new max is equal to the current min hence only one value in the set
      removeAllBut(maxv)
    } else {
      var cv = max
      while (cv > maxv) {
        removeValue(cv)
        cv -= 1
      }
      if (hasValue(maxv)) {
        max = maxv
      }
    }
  }

  def iterator: Iterator[Int] = {
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

  def delta(oldSize: Int): Iterator[Int] = {
    var ind = size
    new Iterator[Int] {
      def next(): Int = {
        val v = values(ind)
        ind += 1
        v + offset
      }
      def hasNext: Boolean = {
        ind < oldSize && ind < values.size
      }
    }
  }
  

  
}