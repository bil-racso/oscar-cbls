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

package oscar.cbls.constraints.lib.global

import collection.immutable.SortedMap
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.invariants.lib.numeric.Implicits._
import oscar.cbls.invariants.core.computation.IntVar._
import oscar.cbls.invariants.core.computation.{BulkLoad, Variable, IntVar}
;

/**Implement the AllDiff constraint on IntVars: all variables must have a different value.
 * @param variables the variable whose values should all be different.
 */
case class AllDiff(variables:Iterable[IntVar]) extends Constraint{

  registerStaticAndDynamicDependencyAllNoID(variables)
  registerConstrainedVariablesAll(variables)
  finishInitialization()

  //le degre global de violation est la somme des tailles -1 des ensembles de var ayant meme value
  // et on ne prend que les ensembles de cardinalite > 1
  private val Violation:IntVar = new IntVar(model,0,Int.MaxValue,0,"ViolationsOfAllDiff")
  Violation.setDefiningInvariant(this)

  private val N0:Int = variables.foldLeft(0)(
    (acc:Int,intvar:IntVar) => (if(intvar.MaxVal > acc) intvar.MaxVal else acc))

  private val offset:Int = - variables.foldLeft(0)(
    (acc:Int,intvar:IntVar) => (if(intvar.MinVal < acc) intvar.MinVal else acc))

  private val N = N0 + offset
  private val range = 0 to N

  /**the degree of violation of a variable is the number of other variables that have the same value as it. */
  private val Violations:SortedMap[IntVar,IntVar] = variables.foldLeft(
    SortedMap.empty[IntVar,IntVar])(
    (acc,intvar) => {
      val newvar = new IntVar(model,0,1,1,"Violation_AllDiff_"+intvar.name)
      acc + ((intvar,newvar))
    })

  private val ValueCount: Array[IntVar] = (for (i <- 0 to N) yield {
    val tmp = new IntVar(model, 0, 1, 0, "alldiff_count_of_value_" + (i - offset))
    tmp.setDefiningInvariant(this)
    tmp
  }).toArray

  for(v <- variables){
    val varval = v.getValue()
    ValueCount(varval + offset) :+= 1
  }

  {
    val listedVars:List[IntVar] = variables.toList
    val accesses:List[IntElement] = listedVars.map((i:IntVar) => IntElement(i plus offset, null))

    BulkLoad(accesses,ValueCount)
    for(varsaccess:((IntVar, IntElement)) <- listedVars.zipAll(accesses,null,null)){
      Violations(varsaccess._1) <== (varsaccess._2 minus 1)
    }
  }

  for(i <- range){
    Violation :+= 0.max(ValueCount(i).getValue(true) -1)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    ValueCount(OldVal+offset) :-= 1
    ValueCount(NewVal+offset) :+= 1

    val DeltaOldVal = (if(ValueCount(OldVal+offset).getValue(true) == 0) 0 else -1)
    val DeltaNewVal = (if(ValueCount(NewVal+offset).getValue(true) == 1) 0 else 1)
    Violation :+= (DeltaNewVal + DeltaOldVal)
  }

  /**The degree of violation of this constraint is the number of variables that should be changed
   * to ensure that the constraint is not violated.
   * @return an IntVar that can be incorporated in an invariant.
   */
  override def getViolation = Violation

  /**The degree of violation of a variable is the number of other variables that have the same value
   * @return an IntVar that can be incorporated in an invariant.
   */
  override def getViolation(v: Variable):IntVar = {
    val tmp:IntVar = Violations.getOrElse(v.asInstanceOf[IntVar],null)
    assert(tmp != null)
    tmp
  }

  override def checkInternals(){
    var MyValueCount:Array[Int] = (for(i <- 0 to N) yield 0).toArray
    for(v <- variables){MyValueCount(v.getValue() + offset) += 1}
    for(v <- range)assert(ValueCount(v).getValue(true) == MyValueCount(v))

    for (v <- variables)
      assert(getViolation(v).getValue()  == MyValueCount(v.getValue()+offset)-1
        ,"error on " + v + " " + getViolation(v).getValue() + " " + MyValueCount(v.getValue()+offset))

    var MyViol:Int = 0
    for(v <- range)MyViol += 0.max(MyValueCount(v) -1)
    assert(MyViol == Violation.getValue())
  }
}


