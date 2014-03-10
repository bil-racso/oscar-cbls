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

  private val interval = new ReversibleInterval(s,minVal,maxVal)
  private val withHoles = new ReversibleBool(s, false)

  // Instantiated and used lazily only if there are holes in the domain
  private var values: ReversibleSparseSet = null 
  
  private def createValueSet(): Unit = {
    values = new ReversibleSparseSet(s, interval.min, interval.max)
    withHoles.value = true
  }

  /**
   * @return the size of the domain (number of value in it).
   */
  def getSize() = size

  override def size = {
    if (withHoles) values.size
    else interval.size 
  }

  /**
   *
   * @return true if the domain is empty, false otherwise
   */
  override def isEmpty = {
    if(withHoles) values.isEmpty
    else interval.isEmpty
  }

  /**
   *
   * @return the largest value in the domain
   */
  def getMax(): Int = max

  def max: Int = {
    // lazy update of max 
    if (withHoles) values.max
    else interval.max
  }


  /**
   *
   * @return the smallest value in the domain
   */
  def getMin(): Int = min

  def min: Int = {
    // lazy update of min 
    if (withHoles) values.min
    else interval.min
  }

  /**
   * Test if a value is present in the domain
   * @param val
   * @return true if val is present in the domain, false otherwise
   */
  def hasValue(value: Int): Boolean = {
    if (withHoles) values.hasValue(value)
    else interval.hasValue(value)
  }

  /**
   * remove value from the domain
   * @param val
   * @return Failure if the domain is empty, Suspend otherwise
   */
  def removeValue(value: Int): CPOutcome = {
    
    // Lazy generation of the set of values
    if (!withHoles && value > interval.min && value < interval.max ) {
      createValueSet()
    }
    
    if (withHoles) {
      values.removeValue(value)
      if (values.isEmpty) CPOutcome.Failure
      else CPOutcome.Suspend
    }    
    else {
      if (interval.isEmpty) {
        CPOutcome.Failure
      } else {
        interval.removeValue(value)
        if (interval.isEmpty) CPOutcome.Failure
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
      if (!values.hasValue(value)) CPOutcome.Failure
      else {
        values.removeAllBut(value)
        CPOutcome.Suspend
      }
    }    
    else {
      if (!interval.hasValue(value)) CPOutcome.Failure
      else {
        interval.assign(value)
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
      values.updateMin(value)
      if (values.isEmpty) CPOutcome.Failure
      else CPOutcome.Suspend
    }    
    else {
      interval.updateMin(value)
      if (interval.isEmpty) CPOutcome.Failure
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
      values.updateMax(value)
      if (values.isEmpty) CPOutcome.Failure
      else CPOutcome.Suspend
    }    
    else {
      interval.updateMax(value)
      if (interval.isEmpty) CPOutcome.Failure
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
      if (withHoles) values.nextValue(value)
      else interval.nextValue(value)
  }

  def getPrevValue(value: Int): Int = prevValue(value)

  /**
   * @param value
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */
  def prevValue(value: Int): Int = {
      if (withHoles) values.prevValue(value)
      else interval.prevValue(value)
  }

  def iterator: Iterator[Int] = {
    if (withHoles) {
      return values.iterator
    } else {
      interval.iterator
    }
  }
  

  
  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    if (withHoles) {
      (oldMin until values.minValue).iterator ++ values.delta(oldSize) ++ (values.maxValue+1 to oldMax).iterator
    } else {
      (oldMin to min-1).iterator ++ (max+1 to oldMax).iterator
    }
  }

  override def toString(): String = {
    if (withHoles) values.mkString(", ")
    else interval.min+".."+interval.max
  }  
}
