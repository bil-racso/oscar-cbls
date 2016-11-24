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


  def apply(length:Int, initValue:Int=0): MagicIntArray = {
    new MagicIntArray(length,initValue,Array.tabulate(length)((id:Int)=> initValue))
  }

  def apply(length:Int,initValue:Int, initValues:Array[Int]): MagicIntArray = {
    require(initValues.length==length)
    new MagicIntArray(length,initValue,initValues)
  }

}

class MagicIntArray(lengthArray :Int, initValue:Int,initValues:Array[Int]){
  private val internalIntArray : Array[Int] = initValues
  private val internalArraySavingChanges : Array[Int] = Array.tabulate(lengthArray)((id:Int)=> initValue)
  private val changesSaved : MagicBoolArray = MagicBoolArray(lengthArray)
  private var change:Boolean = false



  /**
    * Set the new value of element at specific index
    * @param id the index of the element
    * @param value the new element's value
    */
  def update(id:Int, value:Int,saved:Boolean = change){
    assert(id<lengthArray && 0<=id)
    if(saved) {
      changesSaved(id)=true
      internalArraySavingChanges(id) = value
    }
    else internalIntArray(id)=value
  }

  /**
    * Return the value of the element at specific index
    * @param id the index of the element
    * @return the saved value at index
    */
  def apply(id:Int,saved:Boolean = change): Int ={
    assert(0<=id && id<lengthArray)
    if(saved && changesSaved(id) ) internalArraySavingChanges(id) else internalIntArray(id)
  }

  /**
    * Sets the value of each element to "value"
    * @note complexity is O(1)
    */
  def all_=(value:Int): Unit ={
    if(change) {

    }else{

    }
  }

  /**
    *
    * @return
    */
  def all:Int = ???


  def save(): Unit = {
    change = true
    for(indice <- changesSaved.indicesAtTrue) internalIntArray(indice)=internalArraySavingChanges(indice)
    reload()
  }


  def reload(): Unit = changesSaved.all=false


  def drop(): Unit = {
    reload()
    change = false
  }

  def changedSinceSave(id:Int) :Boolean= changesSaved(id)

  def saveChanges():Boolean = change

 // override def toString: String = "["+indicesAtTrue.mkString(",")+"]"





}
