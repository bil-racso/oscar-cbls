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
 * ****************************************************************************
 */

package oscar.cbls.invariants.lib.minmax

import collection.immutable.SortedSet
import oscar.cbls.invariants.core.algo.heap.{ ArrayMap, BinomialHeapWithMoveExtMem }
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.computation.Invariant._
import oscar.cbls.invariants.core.propagation.KeyForElementRemoval
import oscar.cbls.invariants.core.propagation.Checker

/**
 * Maintains Max(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 */
case class MaxArray(varss: Array[IntVar], ccond: IntSetVar = null, val default: Int = Int.MinValue)
  extends MiaxArray(varss, if (ccond == null) IntSetConst(SortedSet.empty[Int] ++ varss.indices) else ccond, default) {

  override def name: String = "MaxArray"

  override def Ord(v: IntVar): Int = -v.value

  override def ExtremumName: String = "Max"

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(output.value >= v.value,
        Some("output.value (" + output.value + ") >= " + v.name + ".value (" + v.value + ")"))
    }
  }
}

/**
 * Maintains Min(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 */
case class MinArray(varss: Array[IntVar], ccond: IntSetVar = null, val default: Int = Int.MaxValue)
  extends MiaxArray(varss, if (ccond == null) IntSetConst(SortedSet.empty[Int] ++ varss.indices) else ccond, default) {

  override def name: String = "MinArray"

  override def Ord(v: IntVar): Int = v.value

  override def ExtremumName: String = "Min"

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(output.value <= v.value,
        Some("output.value (" + output.value + ") <= " + v.name + ".value (" + v.value + ")"))
    }
  }
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n))
 */
abstract class MiaxArray(vars: Array[IntVar], cond: IntSetVar, default: Int) extends IntInvariant with Bulked[IntVar, (Int, Int)] {

  var keyForRemoval: Array[KeyForElementRemoval] = new Array(vars.size)
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.size, new ArrayMap(vars.size))
  var output: IntVar = null

  if (cond != null) {
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  val (myMin, myMax) = bulkRegister(vars)

  if (cond != null) {
    for (i <- cond.value) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  } else {
    for (i <- vars.indices) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  }

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntVar]) = {
    (bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => if (intvar.minVal < acc) intvar.minVal else acc),
      bulkedVar.foldLeft(Int.MinValue)((acc, intvar) => if (intvar.maxVal > acc) intvar.maxVal else acc))
  }

  def name: String
  def ExtremumName: String
  def Ord(v: IntVar): Int

  override def setOutputVar(v: IntVar) {
    output = v
    output.setDefiningInvariant(this)
    if (h.isEmpty) {
      output := default
    } else {
      output := vars(h.getFirst).value
    }
  }

  @inline
  override def notifyIntChanged(v: IntVar, index: Int, OldVal: Int, NewVal: Int) {
    //mettre a jour le heap
    h.notifyChange(index)
    output := vars(h.getFirst).value
  }

  @inline
  override def notifyInsertOn(v: IntSetVar, value: Int) {
    assert(v == cond)
    keyForRemoval(value) = registerDynamicDependency(vars(value), value)

    //mettre a jour le heap
    h.insert(value)
    output := vars(h.getFirst).value
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int) {
    assert(v == cond)

    unregisterDynamicDependency(keyForRemoval(value))
    keyForRemoval(value) = null

    //mettre a jour le heap
    h.delete(value)
    if (h.isEmpty) {
      output := default
    } else {
      output := vars(h.getFirst).value
    }
  }
}
