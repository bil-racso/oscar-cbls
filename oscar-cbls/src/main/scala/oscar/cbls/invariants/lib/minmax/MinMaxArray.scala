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
 *            Yoann Guyot
 ******************************************************************************/

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
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxArray(varss: Array[IntValue], ccond: SetValue = null, default: Int = Int.MinValue)
  extends MiaxArray(varss, if(ccond == null) CBLSSetConst(SortedSet.empty[Int] ++ varss.indices) else ccond, default) {

  override def Ord(v: IntValue): Int = -v.value

  override def ExtremumName: String = "Max"

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value >= v.value,
        Some("output.value (" + this.value + ") >= " + v.name + ".value (" + v.value + ")"))
    }
  }
}

/**
 * Maintains Min(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinArray(varss: Array[IntValue], ccond: SetValue = null, default: Int = Int.MaxValue)
  extends MiaxArray(varss, if(ccond == null) CBLSSetConst(SortedSet.empty[Int] ++ varss.indices) else ccond, default) {

  override def Ord(v: IntValue): Int = v.value

  override def ExtremumName: String = "Min"

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value <= v.value,
        Some("this.value (" + this.value + ") <= " + v.name + ".value (" + v.value + ")"))
    }
  }
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxArray(vars: Array[IntValue], cond: SetValue, default: Int) extends IntInvariant with Bulked[IntValue, Domain] with VaryingDependenciesInvariant {

  var keyForRemoval: Array[KeyForElementRemoval] = new Array(vars.size)
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.size, new ArrayMap(vars.size))

  if (cond != null) {
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  restrictDomain(bulkRegister(vars))

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

  //TODO: single pass please!
  override def performBulkComputation(bulkedVar: Array[IntValue]) = {
    (bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => if (intvar.min < acc) intvar.min else acc),
      bulkedVar.foldLeft(Int.MinValue)((acc, intvar) => if (intvar.max > acc) intvar.max else acc))
  }

  def ExtremumName: String
  def Ord(v: IntValue): Int

  if (h.isEmpty) {
    this := default
  } else {
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int) {
    //mettre a jour le heap
    h.notifyChange(index)
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    keyForRemoval(value) = registerDynamicDependency(vars(value), value)

    //mettre a jour le heap
    h.insert(value)
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)

    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    //mettre a jour le heap
    h.delete(value)
    if (h.isEmpty) {
      this := default
    } else {
      this := vars(h.getFirst).value
    }
  }
}
