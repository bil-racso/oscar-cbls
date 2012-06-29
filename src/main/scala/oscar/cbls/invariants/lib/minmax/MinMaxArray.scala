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
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.computation.IntSetConst._
import oscar.cbls.invariants.core.computation.Invariant._
import oscar.cbls.invariants.core.propagation.KeyForElementRemoval

/** Maintains Max(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * */
case class MaxArray(varss: Array[IntVar], ccond: IntSetVar = null, override val default: Int = Int.MinValue)
  extends MiaxArray(varss, if(ccond == null) IntSetConst(SortedSet.empty[Int] ++ varss.indices) else ccond, default) {

  override def name: String = "MaxArray"

  override def Ord(v: IntVar): Int = -v.getValue()

  override def ExtremumName: String = "Max"
}

/** Maintains Min(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * */
case class MinArray(varss: Array[IntVar], ccond: IntSetVar = null, override val default: Int = Int.MaxValue)
  extends MiaxArray(varss, if(ccond == null) IntSetConst(SortedSet.empty[Int] ++ varss.indices) else ccond, default) {

  override def name: String = "MinArray"

  override def Ord(v: IntVar): Int = v.getValue()

  override def ExtremumName: String = "Min"
}

/** Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n))
 * */
abstract case class MiaxArray(var vars: Array[IntVar], cond: IntSetVar, default: Int) extends IntInvariant with Bulked[IntVar,(Int,Int)]{

  var keyForRemoval: Array[KeyForElementRemoval] = null
  var h: BinomialHeapWithMoveExtMem[Int] = null
  var output: IntVar = null

  if(cond != null){ 
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }
  
  if(vars != null){
    for (v <- vars) registerStaticDependency(v)
  }

  finishInitialization()

  if(vars != null) BulkLoad(vars,performBulkComputation(vars))

  override def performBulkComputation(bulkedVar: Array[IntVar])={
    (bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => if (intvar.MinVal < acc) intvar.MinVal else acc),
      bulkedVar.foldLeft(Int.MinValue)((acc, intvar) => if (intvar.MaxVal > acc) intvar.MaxVal else acc))
  }

  var MyMin = 0
  var MyMax = 0

  override def BulkLoad(bulkedVar: Array[IntVar],bcr: (Int,Int)){
    vars = bulkedVar
    keyForRemoval = new Array(vars.size)
    h = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.size, new ArrayMap(vars.size))
    if (cond == null){

    }else{
    for (i <- cond.getValue()) {
      h.insert(i)
      keyForRemoval.update(i, registerDynamicDependency(vars(i),i))
    }
    }
    MyMin = bcr._1
    MyMax = bcr._2

  }

  def name: String
  def ExtremumName: String
  def Ord(v: IntVar): Int

  override def setOutputVar(v: IntVar) {
    output = v
    output.setDefiningInvariant(this)
    if(h.isEmpty){
      output := default
    }else{
      output := vars(h.getFirst).getValue()
    }
  }

  @inline
  override def notifyIntChanged(v: IntVar, index:Int, OldVal: Int, NewVal: Int) {
    //mettre a jour le heap
    h.notifyChange(index)
    output := vars(h.getFirst).getValue()
  }

  @inline
  override def notifyInsertOn(v: IntSetVar, value: Int) {
    assert(v == cond)
    keyForRemoval.update(value, registerDynamicDependency(vars(value),value))

    //mettre a jour le heap
    h.insert(value)
    output :=  vars(h.getFirst).getValue()
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int) {
    assert(v == cond)

    unregisterDynamicDependency(keyForRemoval(value))
    keyForRemoval.update(value, null)

    //mettre a jour le heap
    h.delete(value)
    if(h.isEmpty){
      output := default
    }else{
      output := vars(h.getFirst).getValue()
    }
  }
}
