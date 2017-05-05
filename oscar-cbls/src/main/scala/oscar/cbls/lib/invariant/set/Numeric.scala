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
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 *            Yoann Guyot
 * ****************************************************************************
 */

package oscar.cbls.lib.invariant.set

import oscar.cbls.core.computation.{ChangingSetValue, IntInvariant, SetNotificationTarget, SetValue}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * Sum(i in on)(fun(i))
 * @param on is the set of integers to add
 * @param fun is an optional function Int -> Int to apply before summing elements. It is expected not to rely on any variable of the model.
 * @author renaud.delandtsheer@cetic.be
 * */
case class SetSum(on: SetValue, fun: (Int => Int) = (a: Int) => a)
  extends IntInvariant(on.value.foldLeft(0)((a, b) => a + fun(b)))
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(on)
  finishInitialization()

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    var delta = 0
    for (added <- addedValues) delta += fun(added)
    for (deleted <- removedValues) delta -= fun(deleted)
    this :+= delta
  }

  override def checkInternals() {
    var count = 0
    for (v <- on.value) count += fun(v)
    require(this.value == count, Some("this.value == count"))
  }
}

/**
 * PRod(i in on)(fun(i))
 * @param on is the set of integers to multiply
 * @author renaud.delandtsheer@cetic.be
 * */
case class SetProd(on: SetValue)
  extends IntInvariant
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(on)
  finishInitialization()

  var nonZeroProduct = on.value.foldLeft(1)(
    (acc, value) => if (value == 0) { acc } else { acc * value})

  if (on.value.contains(0)) {
    this := 0
  } else {
    this := nonZeroProduct
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]){
    for (deleted <- removedValues) if (deleted != 0) {nonZeroProduct /= deleted}
    for (added <- addedValues) if (added != 0) {nonZeroProduct *= added}
    if (newValue.contains(0)) {
      this := 0
    } else {
      this := nonZeroProduct
    }
  }

  override def checkInternals() {
    var countNZ = 1
    for (v <- on.value) if(v !=0) countNZ *= v
    require(nonZeroProduct == countNZ,
      Some("non zero product (" + nonZeroProduct + ") == product of non zero items (" + countNZ + ") and on is " + on))

    var count = 1
    for (v <- on.value) count *= v
    require(this.value == count,
      Some("this.value (" + this.value + ") == count (" + count + ") and on is " + on))
  }
}
