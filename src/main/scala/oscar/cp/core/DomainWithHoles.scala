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
package oscar.cp.core;


import oscar.reversible._
import scala.collection.Iterator

/**
 * Creates a domain that can contain holes in it (by opposition to a simple range domain). <br>
 * The data structure used to represent the holes is created in a lazy way. <br>
 * It means that as long as a no hole is present, the internal representation of the domain is a range (a min and max value). <br>
 * @author Pierre Schaus pschaus@gmail.com
 */
class DomainWithHoles(val s: CPStore, minVal: Int, maxVal: Int) extends Iterable[Int] {

  private val _withHoles = new ReversibleBool(s, false)

  private var _values: ReversibleSparseSet = null; //instantiated and used lazily only if there are holes in the domain

  private val _min = new ReversibleInt(s, minVal)

  private val _max = new ReversibleInt(s, maxVal)

  private val _size = new ReversibleInt(s, maxVal - minVal + 1)

  private def createValueSet() {
    _values = new ReversibleSparseSet(s, min, max)
  }

  /**
   *
   * @return the size of the domain (number of value in it).
   */
  def getSize() = size

  override def size = 0 max _size.value

  /**
   *
   * @return true if the domain is empty, false otherwise
   */
  override def isEmpty = _size.value <= 0

  /**
   *
   * @return the largest value in the domain
   */
  def getMax() = max

  def max = _max.value

  private def max_=(v: Int) = _max.value = v

  private def min_=(v: Int) = _min.value = v

  private def size_=(v: Int) = _size.value = v

  /**
   *
   * @return the smallest value in the domain
   */
  def getMin() = min

  def min = _min.value

  /**
   * Test if a value is present in the domain
   * @param val
   * @return true if val is present in the domain, false otherwise
   */
  def hasValue(value: Int): Boolean = {
    if (value > max || value < min || isEmpty) return false;
    if (!_withHoles.value) {
      return true
    } else {
      return _values.hasValue(value)
    }
  }

  /**
   * remove value from the domain
   * @param val
   * @return Failure if the domain is empty, Suspend otherwise
   */
  def removeValue(value: Int): CPOutcome = {
    var minChanged = false
    var maxChanged = false
    if (value < min || value > max) {
      return CPOutcome.Suspend;
    } else if (min == value) {
      min = value + 1
      minChanged = true
    } else if (max == value) {
      max = value - 1
      maxChanged = true;
    } else if (!_withHoles.value) {
      //create the set of values since there are holes in the domain
      _withHoles.value = true
      createValueSet();
    }

    if (_withHoles.value) {
      if (_values.removeValue(value)) {
        _size.decr()
      }
    } else {
      _size.decr()
    }

    if (isEmpty) {
      return CPOutcome.Failure;
    } else {
      if (_withHoles.getValue()) {
        if (minChanged) min = _values.updateMin(min)
        if (maxChanged) max = _values.updateMax(max)
      }
      return CPOutcome.Suspend;
    }
  }

  /**
   * Remove all values from the domain except val
   * @param val , the only value to remain in the domain
   * @return Failure if the domain is empty, Suspend otherwise
   */
  def assign(value: Int): CPOutcome = {
    if (!hasValue(value)) {
      size = 0
      return CPOutcome.Failure;
    } else {
      size = 1
      min = value
      max = value
      if (_withHoles.value) {
        _values.removeAllBut(value);
      }
      return CPOutcome.Suspend;
    }
  }

  /**
   * Set the minimum of the domain to be >= val
   * @param val
   * @return Failure if the domain is empty, Suspend otherwise
   */
  def updateMin(value: Int): CPOutcome = {
    if (value <= min) return CPOutcome.Suspend;
    else if (value > max) return CPOutcome.Failure;
    else if (_withHoles.value) {
      min = _values.updateMin(value)
      size = _values.size
    } else {
      size = 0.max(max - value + 1)
      min = value
    }
    if (isEmpty) {
      return CPOutcome.Failure;
    } else {
      return CPOutcome.Suspend;
    }
  }

  /**
   * Set the maximum of the domain to be <= val
   * @param val
   * @return Failure if the domain is empty, Suspend otherwise
   */
  def updateMax(value: Int): CPOutcome = {
    if (value >= max) return CPOutcome.Suspend;
    else if (value < min) return CPOutcome.Failure;
    else if (_withHoles.value) {
      max = _values.updateMax(value)
      size = _values.size
    } else {
      _size.value = 0.max(value - min + 1);
      max = value
    }
    if (isEmpty) {
      return CPOutcome.Failure;
    } else {
      return CPOutcome.Suspend;
    }
  }

  def getNextValue(value: Int): Int = nextValue(value)

  /**
   * @param value
   * @return smallest value in the domain >= value, value is returned if no such value
   */
  def nextValue(value: Int): Int = {
    if (value >= max) {
      value
    } else if (value <= min) {
      return min
    } // min < val < max
    else if (!_withHoles.value) {
      value
    } else {
      _values.getNextValue(value)
    }
  }

  def getPrevValue(value: Int): Int = prevValue(value)

  /**
   * @param value
   * @return largest value in the domain < value, value is returned if no such value
   */
  def prevValue(value: Int): Int = {
    if (value <= min) {
      value
    } else if (value > max) {
      max
    } else if (!_withHoles.value) {
      value;
    } else {
      _values.getPreValue(value)
    }
  }

  def iterator: Iterator[Int] = {
    //println("iterator domain")
    if (_withHoles.value) {
      return _values.iterator
    } else {
      var curr: Int = min
      new Iterator[Int] {
        def next(): Int = {
          val res = curr
          curr += 1
          res
        }
        def hasNext: Boolean = {
          curr <= _max.value
        }
      }
    }
  }

}