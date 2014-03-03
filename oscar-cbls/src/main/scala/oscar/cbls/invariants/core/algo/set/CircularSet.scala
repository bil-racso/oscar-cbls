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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.invariants.core.algo.set

import collection.mutable.Set
import collection.Iterator

/**Circular set is a dedicated data structure to represent set of integers in an efficient way
 * although the memory footprint will not be efficient at all
 * stores elements from 0 to size-1
 * ensure that the maxsize is not so big because it consumes O(maxsize) memory.
  * @author renaud.delandtsheer@cetic.be
  * THIS IS EXPERIMENTAL STUFF
 */
class CircularIntSet(maxsize:Int) extends Set[Int]{

  private val containsvar:Array[Boolean] = new Array[Boolean](maxsize)
  private[set] val next:Array[Int] = new Array[Int](maxsize) //gives the id of the next element, so that they constitute a cycle in the array
  private[set] val prev:Array[Int] = new Array[Int](maxsize) //gives the id of the next element, so that they constitute a cycle in the array

  private var handle:Int = -1
  private var sizevar:Int = 0

  def -=(elem: Int):this.type = {
    containsvar(elem) = false
    if (handle == elem){
      if (sizevar == 1){
        handle = -1
        next(elem) = -1
        prev(elem) = -1
        sizevar = 0
        return this
      }else{
        handle = next(elem)
      }
    }

    next(prev(elem)) = next(elem)
    prev(next(elem)) = prev(elem)
    next(elem) = -1
    prev(elem) = -1
    sizevar -=1
    this
  }

  def +=(elem: Int):this.type = {
    if (sizevar == 0){
      containsvar(elem) = true
      next(elem) = elem
      prev(elem) = elem
      sizevar = 1
      handle = elem
    }else if (!containsvar(elem)){
      containsvar(elem) = true
      insertAfter(elem,handle)
    }
  this
  }

  private def insertAfter(elem:Int, newElem:Int){
    val elemAfter:Int = next(elem)
    next(elem) = newElem
    next(newElem) = elemAfter
    prev(elemAfter) = newElem
    prev(newElem) = elem
  }

  override def size:Int = sizevar

  def contains(elem: Int): Boolean = this.containsvar(elem)

  def iterator: Iterator[Int] = new CircularIntSetIterator(handle, this)
}

class CircularIntSetIterator(handle:Int, on:CircularIntSet) extends Iterator[Int]{
  var current = handle
  var initposition:Boolean = true

  def hasNext: Boolean = on.size>0 & (initposition ||on.next(current) != handle)

  def next(): Int = {
    initposition = false
    current = on.next(current)
    on.prev(current)
  }
}

class NonUpdateableInterfaceToIntSet(s:Set[Int]) extends Set[Int]{
  def +=(elem: Int): this.type = {throw new Exception("update attempt on NonUpdateableIntSet"); this}
  def -=(elem: Int): this.type = {throw new Exception("update attempt on NonUpdateableIntSet"); this}

  def contains(elem: Int): Boolean = s.contains(elem)
  def iterator: Iterator[Int] = s.iterator
  override def size:Int = s.size
}

