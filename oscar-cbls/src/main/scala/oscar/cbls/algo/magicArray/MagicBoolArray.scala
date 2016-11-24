package oscar.cbls.algo.magicArray

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


object MagicBoolArray {
  /**
   * create a Magical Array Of Boolean of given length
   * @param n the length
   * @return a Magical Array Of Boolean or null if length is less than zero
   */
  def apply(n:Int):MagicBoolArray ={
    require(n >= 0, "cannot create magic array of negative size")
    new MagicBoolArray(n)
  }
}

/**
 * This represents an array of boolean with O(1) setAll and O(1) clearAll
  * @author Jannou Brohée on 3/10/16.
  * @param length Maximum length of magical array
  */
class MagicBoolArray(val length:Int){

  private val threshold:Int = Int.MaxValue-1

  private var global:Int = 1

  private var internalArray:Array[Int] = Array.ofDim[Int](length)

  /**
    * Set the new value of element at specific index
    * @param id the index of the element
    * @param value the new element's value (true/false)
    * @note in O(1) // trivial
    */
  def update(id:Int, value:Boolean){
    assert(id<length && 0<=id)
    if(value) internalArray(id)=global
    else internalArray(id)=global-1
  }

  /**
    * Return the value of the element at specific index
    * @param id the index of the element
    * @return true or false
    * @note complexity is O(1)
    */
  def apply(id:Int): Boolean ={
    assert(0<=id && id<length)
    internalArray(id)>=global
  }

  /**
    * Sets the value of each element to "value"
    * @note complexity is O(1)
    */
  def all_=(value:Boolean): Unit ={
    if(value) {
      if (Math.abs(global) == threshold) {
        global = 0
        internalArray = Array.ofDim[Int](length)
      } else {
        global = -Math.abs(global)
      }
    }else{
      if(Math.abs(global)==threshold){
        global =1
        internalArray = Array.ofDim[Int](length)
      }else{
        global= Math.abs(global)+1
      }
    }
  }

  def all:Boolean = ???

  /**
    * Creates a new iterator over the indexes of elements which value is true.
    * this is a O(this.length) method
    * @return the new iterator
    */
  def indicesAtTrue:Iterator[Int] ={
    val ret=Array.newBuilder[Int]
    for(n <-0 until length){
      if(internalArray(n)>=global){
        ret.+=(n)
      }
    }
    ret.result().iterator
  }

  override def toString: String = "["+indicesAtTrue.mkString(",")+"]"
}


