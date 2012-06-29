/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
 ******************************************************************************/
/*
 * Copyright CETIC 2012 www.cetic.be
 *
 * This file is part of Asteroid.
 *
 * Asteroid is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Asteroid is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asteroid.
 * If not, see http://www.gnu.org/licenses/lgpl-2.1-standalone.html
 *
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 */

package oscar.cbls.invariants.lib.minmax

import collection.immutable.SortedSet
import oscar.cbls.invariants.core.algo.heap.{ArrayMap, BinomialHeapWithMoveExtMem}
import oscar.cbls.invariants.core.computation.IntSetConst._
import oscar.cbls.invariants.core.computation.Invariant._
import oscar.cbls.invariants.core.propagation.KeyForElementRemoval
import oscar.cbls.invariants.core.computation._

/** Maintains {i in indices of (varss Inter cond) | varss[i] == max(varss(i in indices of (varss Inter cond))}
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * @param default is the value returned when cond is empty
 * update is O(log(n))
 * */
case class ArgMaxArray(varss: Array[IntVar], ccond: IntSetVar = null,override val default:Int = Int.MinValue)
  extends ArgMiaxArray(varss, ccond ,default) {

  override def name: String = "ArgMaxArray"

  override def Ord(v: IntVar): Int = -v.getValue()

  override def ExtremumName: String = "Max of ArgMax"

  /**returns an IntVar equal to the value of the returned indices.
   * not specified if cond is empty
   */
  def getMax: IntVar = Miax
}

/** Maintains {i in indices of (varss Inter cond) | varss[i] == min(varss(i in indices of (varss Inter cond))}
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * @param default is the value returned when cond is empty
 * update is O(log(n))
 * */
case class ArgMinArray(varss: Array[IntVar], ccond: IntSetVar = null,override val default:Int = Int.MaxValue)
  extends ArgMiaxArray(varss, ccond,default) {

  override def name: String = "ArgMinArray"

  override def Ord(v: IntVar): Int = v.getValue()

  override def ExtremumName: String = "Min of ArgMin"

  /**returns an IntVar equal to the value of the returned indices.
   * not specified if cond is empty
   */
  def getMin: IntVar = Miax
}

/** Maintains {i in indices of (varss Inter cond) | varss[i] == miax(varss(i in indices of (varss Inter cond))}
 * Extact ordering is specified by implementiing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, can be null
 * update is O(log(n))
 * */
abstract case class ArgMiaxArray(var vars: Array[IntVar], cond: IntSetVar,default:Int) extends IntSetInvariant with Bulked[IntVar,(Int,Int)]{

  var keyForRemoval: Array[KeyForElementRemoval] = null
  var h: BinomialHeapWithMoveExtMem[Int] = null
  var output: IntSetVar = null
  var Miax: IntVar = null

  if(cond != null){
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  if(vars != null){
    registerStaticDependencyAll(vars)
  }

  finishInitialization()

  if(vars != null) BulkLoad(vars,performBulkComputation(vars))

  override def performBulkComputation(bulkedVar: Array[IntVar])={
    (bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => if (intvar.MinVal < acc) intvar.MinVal else acc),
      bulkedVar.foldLeft(Int.MinValue)((acc, intvar) => if (intvar.MaxVal > acc) intvar.MaxVal else acc))
  }

  override def BulkLoad(bulkedVar: Array[IntVar],bcr: (Int,Int)){
    vars = bulkedVar
    keyForRemoval = new Array(vars.size)
    h = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.size, new ArrayMap(vars.size))
    if(cond != null){
      for (i <- cond.getValue()) {
        h.insert(i)
        keyForRemoval.update(i, registerDynamicDependency(vars(i),i))
      }
    }else{
      for (i <- vars.indices) {
        h.insert(i)
        keyForRemoval.update(i, registerDynamicDependency(vars(i),i))
      }      
    }

    Miax = new IntVar(model,bcr._1,bcr._2,
      if (cond != null && cond.getValue().isEmpty) default else vars(h.getFirst).getValue(), ExtremumName)

    Miax.setDefiningInvariant(this)
  }

  def name: String
  def ExtremumName: String
  def Ord(v: IntVar): Int

  def MyMin = vars.indices.start
  def MyMax = vars.indices.end

  var cost:Long = 0;
  
  override def setOutputVar(v: IntSetVar) {
    output = v
    //collecter les counts et le max
    output.setDefiningInvariant(this)
    val firsts = h.getFirsts
    output := firsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    Miax := (if (firsts.isEmpty) default else vars(h.getFirst).getValue())
  }

  @inline
  override def notifyIntChanged(v: IntVar, index:Int, OldVal: Int, NewVal: Int) {
    cost = cost - System.currentTimeMillis()
    //mettre a jour le heap
    h.notifyChange(index)

    if (vars(h.getFirst).getValue() != Miax.getValue(true)) {
      Miax := vars(h.getFirst).getValue()
      output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (OldVal == Miax.getValue(true)) {
      output.deleteValue(index)
      if (output.getValue(true).isEmpty) {
        output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
        if (output.getValue(true).isEmpty){
          Miax := default
        }else {
          Miax := vars(h.getFirst).getValue()
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
    keyForRemoval.update(value, registerDynamicDependency(vars(value),value))

    //mettre a jour le heap
    h.insert(value)

    if (vars(h.getFirst).getValue() != Miax.getValue(true)) {
      Miax := vars(h.getFirst).getValue()
      output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (vars(value).getValue() == Miax.getValue(true)) {
      output.insertValue(value)
      Miax := vars(h.getFirst).getValue()
    }
    cost = cost + System.currentTimeMillis()
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int) {
    cost = cost - System.currentTimeMillis()
    assert(v == cond && cond != null)

    unregisterDynamicDependency(keyForRemoval(value))
    keyForRemoval.update(value, null)

    //mettre a jour le heap
    h.delete(value)

    if (h.isEmpty){
      Miax := default
      output := SortedSet.empty[Int]
    } else if (vars(h.getFirst).getValue() != Miax.getValue(true)) {
      Miax := vars(h.getFirst).getValue()
      output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
    } else if (vars(value).getValue() == Miax.getValue(true)) {
      output.deleteValue(value)
      if (output.getValue(true).isEmpty) {
        output := h.getFirsts.foldLeft(SortedSet.empty[Int])((acc, index) => acc + index)
        Miax := vars(h.getFirst).getValue()
      }
    }
    cost = cost + System.currentTimeMillis()
  }

  override def checkInternals() {
    var count: Int = 0;
    for (i <- vars.indices) {
      if (cond == null || (cond != null && cond.getValue().contains(i))) {
        if (vars(i).getValue() == this.Miax.getValue()) {
          assert(output.getValue().contains(i))
          count += 1
        } else {
          assert(Ord(Miax.getValue()) < Ord(vars(i).getValue()))
        }
      }
    }
    assert(output.getValue().size == count)
    h.checkInternals()
    assert(h.getFirsts.length == output.getValue().size)
    if (cond != null)
      assert(output.getValue(true).subsetOf(cond.getValue(true)))
  }
}
