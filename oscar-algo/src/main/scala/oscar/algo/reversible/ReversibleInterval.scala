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

package oscar.algo.reversible

/**
 * @author pschaus
 */
class ReversibleInterval(s: ReversibleContext, val minValue: Int, val maxValue: Int) {
  val _maxValue = if (maxValue == Int.MaxValue) Int.MaxValue - 10 else maxValue 
  private val _min = new ReversibleInt(s, minValue)

  private val _max = new ReversibleInt(s, _maxValue)

  private val _size = new ReversibleInt(s, _maxValue - minValue + 1)
  
  def size = 0 max _size.value
  
  @inline def min: Int = {
	assert(!isEmpty)
    _min.value
  }
  
  @inline def max: Int = {
    assert(!isEmpty)
    _max.value
  }
  

  @inline def isEmpty = {
    size <= 0
  }
  
  /**
   * @param value
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */ 
  def nextValue(value: Int): Int = {
    if (isEmpty || value > max) {
      value-1
    } else if (value < min) {
      return min
    } else value
  }
  
  /**
   * @param value
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */ 
  def prevValue(value: Int): Int = {
    if (isEmpty || value < min) {
      value+1
    } else if (value > max) {
      return max
    } else value
  }
  
  def removeValue(value: Int) {
    if (value == min) updateMin(value+1)
    if (!isEmpty && value == max) updateMax(value-1)
  }
  
  def hasValue(value: Int) = !isEmpty && value <= max && value >= min
  
  def iterator: Iterator[Int] = {
    (min to max).iterator
  }
  
  def updateMax(value: Int) {
    if (value >= max) return
    else if (value < min) _size.value = 0
    else {
      _size.value = value-min+1
      _max.value = value
    }
  }
  
  def updateMin(value: Int) {
    if (value <= min) return
    else if (value > max) _size.value = 0
    else {
      _size.value = max-value+1
      _min.value = value
    }    
    
  }
  
  def assign(value: Int) {
    if (hasValue(value)) {
      _min.value = value
      _max.value = value
      _size.value = 1
    }
  }

}
