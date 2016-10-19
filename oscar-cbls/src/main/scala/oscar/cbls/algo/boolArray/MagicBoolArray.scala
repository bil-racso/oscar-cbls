package oscar.cbls.algo.boolArray

import oscar.cbls.algo.quick.QList

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
  def apply(n:Int,initVal:Boolean = false):MagicBoolArray ={
    require(n >= 0, "cannot create magic array of negative size")
    new MagicBoolArray(n,initVal)
  }
}

class MagicBoolArrayWithFastIteratorOnTrueOverApproximated(override val length:Int,initVal:Boolean = false)
  extends MagicBoolArray(length,initVal){

  private var positionsAtTrueOverApproximated:QList[Int] = null
  private val isPositionInOverApproximationQList:MagicBoolArray = MagicBoolArray(length,false)
  private var overApproximationIsComplete:Boolean = initVal

  private var anyIndividualSetToFalse:Boolean = false

  private var nbTrue:Int = if(initVal) length else 0

  val nbIndicesAtTrue:Int = nbTrue
  /**
   * Sets the value of each element to "value"
   * @note complexity is O(1)
   */
  override def all_=(value : Boolean) : Unit = {
    super.all_=(value)
    if(value){
      positionsAtTrueOverApproximated = null
      isPositionInOverApproximationQList.all_=(false)
      overApproximationIsComplete = true
    }else{
      positionsAtTrueOverApproximated = null
      isPositionInOverApproximationQList.all_=(false)
      overApproximationIsComplete = false
    }
    nbTrue= if(value) length else 0
  }

  override def update(id : Int, value : Boolean):Boolean = {
    val oldValue = super.update(id, value)
    if(value){
      if(!oldValue){
        if (!overApproximationIsComplete) {
          if (!isPositionInOverApproximationQList.update(id,true)) {
            positionsAtTrueOverApproximated = QList(id, positionsAtTrueOverApproximated)
          }
        }
        nbTrue += 1
      }
    }else{
      if(oldValue){
        anyIndividualSetToFalse = true
        nbTrue -= 1
      }
    }
    oldValue
  }

  override def indicesAtTrue : Iterator[Int] = {
    if(overApproximationIsComplete){
      if(anyIndividualSetToFalse){
        super.indicesAtTrue
      }else{
        indices.toIterator
      }
    }else{
      if(anyIndividualSetToFalse){
        positionsAtTrueOverApproximated.filter(this(_)).toIterator
      }else{
        positionsAtTrueOverApproximated.toIterator
      }
    }
  }
}

/**
 * This represents an array of boolean with O(1) setAll and O(1) clearAll
 * @author Jannou Brohée on 3/10/16.
 * @param length Maximum length of magical array
 */
class MagicBoolArray(val length:Int,initVal:Boolean = false){

  private[this] val threshold:Int = Int.MaxValue-1

  private[this] var global:Int = 1

  private[this] val internalArray:Array[Int] = Array.fill[Int](length)(if(initVal) 1 else 0)

  val indices = 0 until length

  /**
   * Set the new value of element at specific index
   * @param id the index of the element
   * @param value the new element's value (true/false)
   * @return the old value
   * @note in O(1) // trivial
   */
  def update(id:Int, value:Boolean):Boolean = {
    assert(id<length && 0<=id)
    val oldInternalArray = internalArray(id)
    if(value) internalArray(id)=global
    else internalArray(id)=global-1
    oldInternalArray>=global
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
        resetArray()
      } else {
        global = -Math.abs(global)
      }
    }else{
      if(Math.abs(global)==threshold){
        global = 1
        resetArray()
      }else{
        global = Math.abs(global)+1
      }
    }
  }

  @inline
  private [this] def resetArray(){
    var i = internalArray.length
    while(i > 0){
      i -= 1
      internalArray(i) = 0
    }
  }

  /**
   * Creates a new iterator over the indexes of elements which value is true.
   * this is a O(this.length) method
   * @return the new iterator
   */
  def indicesAtTrue:Iterator[Int] ={
    val ret=Array.newBuilder[Int]
    for(n <-0 until length-1){
      if(internalArray(n)>=global){
        ret.+=(n)
      }
    }
    ret.result().iterator
  }

  override def toString: String = "["+indicesAtTrue.mkString(",")+"]"
}


