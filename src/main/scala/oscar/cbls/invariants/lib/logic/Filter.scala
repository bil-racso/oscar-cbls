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
 ******************************************************************************/


package oscar.cbls.invariants.lib.logic
/**This package proposes a set of logic invariants, which are used to define the structure of the problem*/


import collection.immutable.SortedSet
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.checker

/** { i in index(values) | cond(values[i] }
 * @param values is an array of IntVar
 * @param cond is a function that selects values to be includes in the output set.
 * This ''cond'' function cannot depend on any IntVar, as updates to these IntVars will not trigger propagation of this invariant.
 */
case class Filter(var values:Array[IntVar], cond:(Int=>Boolean)) extends IntSetInvariant {
  var output:IntSetVar=null

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)
  finishInitialization()

  def myMin = values.indices.start
  def myMax = values.indices.end

  override def setOutputVar(v:IntSetVar){
    output = v
    output.setDefiningInvariant(this)
    output := values.indices.foldLeft(SortedSet.empty[Int])((acc:SortedSet[Int],indice:Int) => if(cond(values(indice).value)){acc+indice}else{acc})
  }

  @inline
  override def notifyIntChanged(v:IntVar,index:Int, OldVal:Int,NewVal:Int){
    val OldCond = cond(OldVal)
    val NewCond = cond(NewVal)
    if(OldCond  && !NewCond) output.deleteValue(index)
    else if(NewCond && !OldCond) output.insertValue(index)
  }

  override def checkInternals(c:checker){
    for(i <- values.indices){
      c.check(!cond(values(i).value) ||output.value.contains(i))
      c.check(cond(values(i).value) || !output.value.contains(i))
    }
  }
}
