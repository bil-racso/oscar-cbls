/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.invariants.core.propagation
import collection.immutable.SortedMap

/**
 * integrate this trait to store someting in your class using the standard storing mechanism
 * @author renaud.delandtsheer@cetic.be
 */
trait DistributedStorageUtility {

  var storage: SortedMap[Int, AnyRef] = SortedMap.empty

  /**returns null if nothing was stored*/
  final def getStorageAt[T](index: Int, default: T = null) =
    storage.getOrElse(index, default).asInstanceOf[T]

  final def storeAt(index: Int, value: AnyRef) {
    storage = storage + ((index, value))
  }
}

/**
 * integrate this trait somewhere as he dictionary defining unique keys for the [[oscar.cbls.invariants.core.propagation.DistributedStorageUtility]]
 * @author renaud.delandtsheer@cetic.be
 */
trait StorageUtilityManager {
  var nextStoragePlace: Int = 0

  def getStorageIndex(): Int = {
    val toreturn = nextStoragePlace
    nextStoragePlace += 1
    toreturn
  }
}
