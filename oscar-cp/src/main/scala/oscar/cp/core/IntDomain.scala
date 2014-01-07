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


import oscar.algo.reversible._
import scala.collection.Iterator
import oscar.algo.reversible.ReversibleInterval
import oscar.algo.reversible.ReversibleSparseSet



/**
 * Creates a domain that can contain holes in it (by opposition to a simple range domain). <br>
 * The data structure used to represent the holes is created in a lazy way. <br>
 * It means that as long as a no hole is present, the internal representation of the domain is a range (a min and max value). <br>
 * @author Pierre Schaus pschaus@gmail.com
 */
class IntDomain(val s: CPStore, minVal: Int, maxVal: Int) extends Iterable[Int] {

  private val _interval = new ReversibleInterval(s,minVal,maxVal)
  private val _withHoles = new ReversibleBool(s, false)

  private var _values: ReversibleSparseSet = null; //instantiated and used lazily only if there are holes in the domain

  //if (maxVal - minVal == 1) createValueSet()
  
  private def withHoles: Boolean = _withHoles.value

  private def createValueSet() {
    _values = new ReversibleSparseSet(s, min, max)
    _withHoles.value = true
  }

  /**
   *
   * @return the size of the domain (number of value in it).
   */
  def getSize() = size

  override def size = {
    if (withHoles) _values.size
    else _interval.size 
  }

  /**
   *
   * @return true if the domain is empty, false otherwise
   */
  override def isEmpty = {
    if(withHoles) _values.isEmpty
    else {
      _interval.isEmpty
    }
  }

  /**
   *
   * @return the largest value in the domain
   */
  def getMax(): Int = max

  def max: Int = {
    // lazy update of max 
    if (withHoles) _values.max
    else _interval.max
  }


  /**
   *
   * @return the smallest value in the domain
   */
  def getMin(): Int = min

  def min: Int = {
    // lazy update of min 
    if (withHoles) _values.min
    else _interval.min
  }

  /**
   * Test if a value is present in the domain
   * @param val
   * @return true if val is present in the domain, false otherwise
   */
  def hasValue(value: Int): Boolean = {
    if (withHoles) _values.hasValue(value)
    else _interval.hasValue(value)
  }

  /**
   * remove value from the domain
   * @param val
   * @return Failure if the domain is empty, Suspend otherwise
   */
  def removeValue(value: Int): CPOutcome = {
    if (!withHoles && value > _interval.min && value < _interval.max ) {
      createValueSet()
    }
    
    if (withHoles) {
      _values.removeValue(value)
      if (_values.isEmpty) CPOutcome.Failure
      else CPOutcome.Suspend
    }    
    else {
      if (_interval.isEmpty) {
        CPOutcome.Failure
      } else {
        _interval.removeValue(value)
        if (_interval.isEmpty) CPOutcome.Failure
        else CPOutcome.Suspend
      }
    }

  }

  /**
   * Remove all values from the domain except val
   * @param val , the only value to remain in the domain
   * @return Failure if the domain is empty, Suspend otherwise
   */
  def assign(value: Int): CPOutcome = {
    if (withHoles) {
      if (!_values.hasValue(value)) CPOutcome.Failure
      else {
        _values.removeAllBut(value)
        CPOutcome.Suspend
      }
    }    
    else {
      if (!_interval.hasValue(value)) CPOutcome.Failure
      else {
        _interval.assign(value)
        CPOutcome.Suspend
      }
    }
  }

  /**
   * Set the minimum of the domain to be >= val
   * @param val
   * @return Failure if the domain is empty, Suspend otherwise
   */
  def updateMin(value: Int): CPOutcome = {
    if (withHoles) {
      _values.updateMin(value)
      if (_values.isEmpty) CPOutcome.Failure
      else CPOutcome.Suspend
    }    
    else {
      _interval.updateMin(value)
      if (_interval.isEmpty) CPOutcome.Failure
      else CPOutcome.Suspend
    }
  }

  /**
   * Set the maximum of the domain to be <= val
   * @param val
   * @return Failure if the domain is empty, Suspend otherwise
   */
  def updateMax(value: Int): CPOutcome = {
    if (withHoles) {
      _values.updateMax(value)
      if (_values.isEmpty) CPOutcome.Failure
      else CPOutcome.Suspend
    }    
    else {
      _interval.updateMax(value)
      if (_interval.isEmpty) CPOutcome.Failure
      else CPOutcome.Suspend
    }
  }

  def getNextValue(value: Int): Int = {
      nextValue(value)
  }

  /**
   * @param value
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */
  def nextValue(value: Int): Int = {
      if (withHoles) _values.nextValue(value)
      else _interval.nextValue(value)
  }

  def getPrevValue(value: Int): Int = prevValue(value)

  /**
   * @param value
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */
  def prevValue(value: Int): Int = {
      if (withHoles) _values.prevValue(value)
      else _interval.prevValue(value)
  }

  def iterator: Iterator[Int] = {
    if (_withHoles.value) {
      return _values.iterator
    } else {
      _interval.iterator
    }
  }
  

  
  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    if (_withHoles.value) {
      (oldMin until _values.minValue).iterator ++ _values.delta(oldSize) ++ (_values.maxValue+1 to oldMax).iterator
    } else {
      (oldMin to min-1).iterator ++ (max+1 to oldMax).iterator
    }
  }

  override def toString(): String = {
    if (_withHoles.value) _values.toString
    else _interval.min+".."+_interval.max
  }  

}
