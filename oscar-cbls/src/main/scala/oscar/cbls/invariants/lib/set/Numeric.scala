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

package oscar.cbls.invariants.lib.set

import oscar.cbls.invariants.core.computation.{ IntVar, IntInvariant, IntSetVar }
import oscar.cbls.invariants.core.propagation.Checker

/**
 * Sum(i in on)(fun(i))
 * @param on is the set of integers to add
 * @param fun is an optional function Int -> Int to apply before summing elements. It is expected not to rely on any variable of the model.
 */
case class SetSum(on: IntSetVar, fun: (Int => Int) = ((a: Int) => a)) extends IntInvariant {

  var output: IntVar = null

  def myMax = Int.MaxValue
  def myMin = Int.MinValue
  registerStaticAndDynamicDependency(on)
  finishInitialization()

  override def setOutputVar(v: IntVar) {
    output = v
    output.setDefiningInvariant(this)
    output := on.value.foldLeft(0)((a, b) => a + fun(b))
  }

  @inline
  override def notifyInsertOn(v: IntSetVar, value: Int) {
    assert(v == on)
    output :+= fun(value)
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int) {
    assert(v == on)
    output :-= fun(value)
  }

  override def checkInternals(c: Checker) {
    var count = 0
    for (v <- on.value) count += fun(v)
    c.check(output.value == count, Some("output.value == count"))
  }
}

/**
 * PRod(i in on)(fun(i))
 * @param on is the set of integers to multiply
 * @param fun is an optional function Int -> Int to apply before multiplying elements. It is expected not to rely on any variable of the model.
 */
case class SetProd(on: IntSetVar, fun: (Int => Int) = ((a: Int) => a)) extends IntInvariant {

  var output: IntVar = null
  var NonZeroProduct: Int = 0

  registerStaticAndDynamicDependency(on)
  finishInitialization()

  def myMax = Int.MaxValue
  def myMin = Int.MinValue

  override def setOutputVar(v: IntVar) {
    output = v
    output.setDefiningInvariant(this)
    NonZeroProduct = on.value.foldLeft(1)(
      (acc, value) => if (value == 0) { acc } else { acc * fun(value) })
    if (on.value.contains(0)) {
      output := 0
    } else {
      output := NonZeroProduct
    }
  }

  @inline
  override def notifyInsertOn(v: IntSetVar, value: Int) {
    assert(v == on)
    if (value != 0) {
      NonZeroProduct *= fun(value)
    }
    if (on.value.contains(0)) {
      output := 0
    } else {
      output := NonZeroProduct
    }
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int) {
    assert(v == on, "The given set (IntSetVar) should be SetProd.on.")
    if (value != 0) {
      NonZeroProduct /= fun(value)
    }
    if (on.value.contains(0)) {
      /**
       * Nothing to do since 0 is already in output
       * or it will be added when its insertion in the set will be notified.
       */
    } else {
      output := NonZeroProduct
    }
  }

  override def checkInternals(c: Checker) {
    var count = 1
    for (v <- on.value) count *= v
    c.check(output.value == count,
      Some("output.value (" + output.value + ") == count (" + count + ")"))
  }
}
