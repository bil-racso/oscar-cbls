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


/**
  * Created by  Jannou Brohée on 24/11/16.
  */

object MagicIntArray {
  def apply(length:Int, initValue:Int=0): MagicIntArray = new MagicIntArray(length,initValue,Array.tabulate(length)((id:Int)=> initValue))

  def apply(length:Int,initValue:Int, initValues:Array[Int]): MagicIntArray = {
    require(initValues.length==length)
    new MagicIntArray(length,initValue,initValues)
  }
}

/**
  * Handle checkpoint and rollback on an array of Int
  * @param lengthArray Int
  * @param initValue =0
  * @param initValues =Array.tabulate(lengthArray)((id:Int)=> initValue)
  */
class MagicIntArray(lengthArray :Int, initValue:Int,initValues:Array[Int]){
  private val internalIntArray : Array[Int] = initValues
  private val internalArraySavingChanges : Array[Int] = Array.tabulate(lengthArray)((id:Int)=> initValue)
  private val changesSaved : MagicBoolArray = MagicBoolArray(lengthArray)
  private var change:Boolean = false

  /**
    * Updates the n-th element to a new value
    * @param n the index of the element
    * @param value the new value
    * @param saved true to save the value at checkpoint, false to override it
    * @note O(1)
    */
  def update(n:Int, value:Int, saved:Boolean = change){
    assert(n<lengthArray && 0<=n)
    if(saved) {
      changesSaved(n)=true
      internalArraySavingChanges(n) = value
    }
    else internalIntArray(n)=value
  }

  /**
    * Return the value of the n-th element
    * @param n the index of the element
    * @param saved true if checkpoint, false otherwise
    * @return the value of the n-th element
    * @note O(1)
    */
  def apply(n:Int, saved:Boolean = change): Int ={
    assert(0<=n && n<lengthArray)
    if(saved && changesSaved(n) ) internalArraySavingChanges(n) else internalIntArray(n)
  }

  /**
    * Saves current value of the MagicArray
    * @note O(length)
    */
  def save(): Unit = {
    change = true
    for(indice <- changesSaved.indicesAtTrue) internalIntArray(indice)=internalArraySavingChanges(indice)
    reload()
  }


  /**
    * Recovers saved values and clears changes history
    * @note O(1)
    */
  def reload(): Unit = changesSaved.all=false

  /**
    * Drops the saved values
    * @note O(1)
    */
  def drop(): Unit = {
    reload()
    change = false
  }

  /**
    * Tests whether the n-th element have changed since save call
    * @param n
    * @return true if change, false otherwise
    * @note O(1)
    */
  def changedSinceSave(n:Int) :Boolean= changesSaved(n)

  /**
    * Tests whether values is saved
    * @return true if there are saved values, false otherwise
    * @note O(1)
    */
  def saveChanges():Boolean = change

  /**
    * @note O(lengthArray)
    * @return
    */
  override def toString: String = {
    var toReturn = "["
    for(n <- 0 until lengthArray-1)
      toReturn += "("+this(n)+" changed := "+this.changedSinceSave(n)+"),"
    toReturn += "("+this(lengthArray-1)+" changed := "+this.changedSinceSave(lengthArray-1)+")]"
    toReturn
  }
}
