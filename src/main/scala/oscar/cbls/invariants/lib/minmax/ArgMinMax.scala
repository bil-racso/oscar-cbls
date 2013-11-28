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
import oscar.cbls.invariants.core.computation.Invariant._
import oscar.cbls.invariants.core.propagation.{ Checker, KeyForElementRemoval }
import oscar.cbls.invariants.core.computation._

/**
 * Maintains {i in indices of (vars Inter cond) | vars[i] == max(vars(i in indices of (vars Inter cond))}
 * @param vars is an array of IntVar
 * @param cond is the condition, supposed fully acceptant if not specified
 * @param default is the value returned when cond is empty
 * update is O(log(n))
 */
case class ArgMaxArray(vars: Array[IntVar], cond: IntSetVar = null, default: Int = Int.MinValue)
  extends ArgMiaxArray(vars, cond, default) {

  override def name: String = "ArgMaxArray"

  override def Ord(v: IntVar): Int = -v.value

  override def ExtremumName: String = "Max of ArgMax"

  /**
   * returns an IntVar equal to the value of the returned indices.
   * not specified if cond is empty
   */
  def getMax: IntVar = Miax
}

/**
 * Maintains {i in indices of (vars Inter cond) | vars[i] == min(vars(i in indices of (vars Inter cond))}
 * @param vars is an array of IntVar
 * @param cond is the condition, supposed fully acceptant if not specified (must be specified if vars is bulked)
 * @param default is the value returned when cond is empty
 * update is O(log(n))
 */
case class ArgMinArray(vars: Array[IntVar], cond: IntSetVar = null, default: Int = Int.MaxValue)
  extends ArgMiaxArray(vars, cond, default) {

  override def name: String = "ArgMinArray"

  override def Ord(v: IntVar): Int = v.value

  override def ExtremumName: String = "Min of ArgMin"

  /**
   * returns an IntVar equal to the value of the returned indices.
   * not specified if cond is empty
   */
  def getMin: IntVar = Miax
}

/**
 * Maintains {i in indices of (varss Inter cond) | varss[i] == miax(varss(i in indices of (varss Inter cond))}
 * Extact ordering is specified by implementiing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, can be null
 * update is O(log(n))
 */
abstract class ArgMiaxArray(vars: Array[IntVar], cond: IntSetVar, default: Int) extends IntSetInvariant with Bulked[IntVar, (Int, Int)] {

  var keyForRemoval: Array[KeyForElementRemoval] = new Array(vars.size)
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.size, new ArrayMap(vars.size))
  var output: IntSetVar = null
  var Miax: IntVar = null

  if (cond != null) {
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  val (minOfMiax, maxOfMiax) = bulkRegister(vars)

  finishInitialization()

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

  Miax = new IntVar(model, (minOfMiax to maxOfMiax),
    if (cond != null && cond.value.isEmpty) default else vars(h.getFirst).value, ExtremumName)

  Miax.setDefiningInvariant(this)

  override def performBulkComputation(bulkedVar: Array[IntVar]) = {
    (bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => if (intvar.minVal < acc) intvar.minVal else acc),
      bulkedVar.foldLeft(Int.MinValue)((acc, intvar) => if (intvar.maxVal > acc) intvar.maxVal else acc))
  }

  def name: String
  def ExtremumName: String
  def Ord(v: IntVar): Int

  def myMin = vars.indices.start
  def myMax = vars.indices.end

  var cost:Long = 0

  override def setOutputVar(v: IntSetVar) {
    output = v
    //collecter les counts et le max
    output.setDefiningInvariant(this)
    val firsts = h.getFirsts
    output := firsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    Miax := (if (firsts.isEmpty) default else vars(h.getFirst).value)
  }

  @inline
  override def notifyIntChanged(v: IntVar, index: Int, OldVal: Int, NewVal: Int) {
    cost = cost - System.currentTimeMillis()
    //mettre a jour le heap
    h.notifyChange(index)

    if (vars(h.getFirst).value != Miax.getValue(true)) {
      Miax := vars(h.getFirst).value
      output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (OldVal == Miax.getValue(true)) {
      output.deleteValue(index)
      if (output.getValue(true).isEmpty) {
        output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
        if (output.getValue(true).isEmpty) {
          Miax := default
        } else {
          Miax := vars(h.getFirst).value
        }
      }
    } else if (NewVal == Miax.getValue(true)) {
      output.insertValue(index)
    }
    cost = cost + System.currentTimeMillis()
  }

  @inline
  override def notifyInsertOn(v: IntSetVar, value: Int) {
    cost = cost - System.currentTimeMillis()
    assert(v == cond && cond != null)
    keyForRemoval(value) = registerDynamicDependency(vars(value), value)

    //mettre a jour le heap
    h.insert(value)

    if (vars(h.getFirst).value != Miax.getValue(true)) {
      Miax := vars(h.getFirst).value
      output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (vars(value).value == Miax.getValue(true)) {
      output.insertValue(value)
      Miax := vars(h.getFirst).value
    }
    cost = cost + System.currentTimeMillis()
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int) {
    cost = cost - System.currentTimeMillis()
    assert(v == cond && cond != null)

    unregisterDynamicDependency(keyForRemoval(value))
    keyForRemoval(value) = null

    //mettre a jour le heap
    h.delete(value)

    if (h.isEmpty) {
      Miax := default
      output := SortedSet.empty[Int]
    } else if (vars(h.getFirst).value != Miax.getValue(true)) {
      Miax := vars(h.getFirst).value
      output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (vars(value).value == Miax.getValue(true)) {
      output.deleteValue(value)
      if (output.getValue(true).isEmpty) {
        output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
        Miax := vars(h.getFirst).value
      }
    }
    cost = cost + System.currentTimeMillis()
  }

  override def checkInternals(c: Checker) {
    var count: Int = 0
    for (i <- vars.indices) {
      if (cond == null || (cond != null && cond.value.contains(i))) {
        if (vars(i).value == this.Miax.value) {
          c.check(output.value.contains(i),
            Some("output.value.contains(" + i + ")"))
          count += 1
        } else {
          c.check(Ord(Miax.value) < Ord(vars(i).value),
            Some("Ord(" + Miax.value + ") < Ord(vars(" + i + ").value ("
              + vars(i).value + "))"))
        }
      }
    }
    c.check(output.value.size == count, Some("output.value.size == count"))
    h.checkInternals(c: Checker)
    c.check(h.getFirsts.length == output.value.size, Some("h.getFirsts.length == output.value.size"))
    if (cond != null)
      c.check(output.getValue(true).subsetOf(cond.getValue(true)), Some("output.getValue(true).subsetOf(cond.getValue(true))"))
  }
}
