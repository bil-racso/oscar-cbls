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
package oscar.cbls.invariants.core.propagation
import collection.immutable.SortedMap

trait DistributedStorageUtility{

  var storage:SortedMap[Int, AnyRef] = SortedMap.empty

  /**returns null if nothing was stored*/
  final def getStorageAt[T](index:Int,default:T=null)=
    storage.getOrElse(index,default).asInstanceOf[T]

  final def storeAt(index:Int,value:AnyRef){
    storage = storage + ((index,value))
  }
}

trait StorageUtilityManager{

  var NextStoragePlace:Int = 0

  def getStorageIndex:Int = {
    val toreturn = NextStoragePlace
    NextStoragePlace+=1
    toreturn
  }

}
