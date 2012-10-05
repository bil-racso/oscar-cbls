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

/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.constraints.lib.global

import collection.immutable.SortedMap
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation.IntConst
import oscar.cbls.invariants.core.computation.{Variable, IntVar}
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.algebra.Algebra._
import oscar.cbls.invariants.lib.logic.IntITE

/**Implement the AtLeast constraint on IntVars.
 * There is a set of minbounds, defined in the parameter bound as pair (value,minbound).
 * The variables should be such that there is at least ''minbound'' of them which have the value ''value''.
 *
 * @param variables the variable whose values are constrained
 * @param bounds map(value,minbound) specifying the minimal number of occurrence of ''value'' among the variables.
 * We use a map to ensure that there is no two bounds on the same value.
 */
case class AtLeast(variables:Iterable[IntVar], bounds:SortedMap[Int, IntVar]) extends Constraint{

  registerConstrainedVariablesAll(variables)
  registerStaticAndDynamicDependencyAllNoID(variables)
  finishInitialization()

  private val Violation:IntVar = new IntVar(model,0,Int.MaxValue,0,"ViolationsOfAtLeast")
  Violation.setDefiningInvariant(this)

  private val N0:Int = variables.foldLeft(0)((acc:Int,intvar:IntVar) => (if(intvar.MaxVal > acc) intvar.MaxVal else acc))
  private val offset:Int = - variables.foldLeft(0)((acc:Int,intvar:IntVar) => (if(intvar.MinVal < acc) intvar.MinVal else acc))
  private val N = N0 + offset
  private val range = 0 until N

  private val Violations:SortedMap[IntVar,IntVar] = variables.foldLeft(SortedMap.empty[IntVar,IntVar])((acc,intvar)
  => {
    val newvar = new IntVar(model,0,1,1,"Violation_AtLeast_"+intvar.name)
    acc + ((intvar,newvar))
  })

  private val ValueCount: Array[IntVar] = (for (i <- 0 to N) yield {
    val tmp = new IntVar(model, -1, variables.size, 0, "AtLeast_count_of_value_" + (i - offset))
    tmp.setDefiningInvariant(this)
    tmp
  }
    ).toArray

  private val BoundArray:Array[IntVar]= Array.tabulate(N)(v =>
    if(bounds.contains(v-offset)){
      bounds(v)
    }else{
      0
    })

  private val ViolationByVal:Array[IntVar] = (for(i <- -offset to N0) yield {
    if(bounds.contains(i)){
      IntITE(ValueCount(i+offset) - BoundArray(i+offset),Violation, 0).toIntVar
    }else{
      Violation
    }}).toArray

  for(v <- variables){
    val varval = v.value
    ValueCount(varval + offset) :+= 1
    Violations(v) <== IntElement(v + offset, ViolationByVal)
  }

  for(i <- range){
    Violation :+= 0.max(BoundArray(i).getValue(true) - ValueCount(i).getValue(true))
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    val NewBounded = bounds.contains(NewVal)
    val OldBounded = bounds.contains(OldVal)

    ValueCount(OldVal+offset) :-= 1
    ValueCount(NewVal+offset) :+= 1

    if(NewBounded){
      if (OldBounded){
        val DeltaOldVal = if(BoundArray(OldVal+offset).getValue(true) > ValueCount(OldVal+offset).getValue(true)) 1 else 0
        val DeltaNewVal = if(BoundArray(NewVal+offset).getValue(true) >= ValueCount(NewVal+offset).getValue(true)) -1 else 0
        Violation :+= (DeltaNewVal + DeltaOldVal)
      }else{
        val DeltaNewVal = if(BoundArray(NewVal+offset).getValue(true) >= ValueCount(NewVal+offset).getValue(true)) -1 else 0
        Violation :+= DeltaNewVal
      }
    }else{
      if (OldBounded){
        val DeltaOldVal = if(BoundArray(OldVal+offset).getValue(true) > ValueCount(OldVal+offset).getValue(true)) 1 else 0
        Violation :+= DeltaOldVal
      }
    }
  }

  /**the violation is the sum for all bounds of the number of missing variables to reach the bound
   */
  override def getViolation = Violation

  /**The violation of a variable is zero if the value of the variable is the one of a bound that is not reached,
   * otherwise, it is equal to the global violation degree.
   */
  override def getViolation(v: Variable):IntVar = {
    val tmp:IntVar = Violations.getOrElse(v.asInstanceOf[IntVar],null)
    assert(tmp != null)
    tmp
  }

  override def checkInternals(){
    var MyValueCount:Array[Int] = (for(i <- 0 to N) yield 0).toArray
    for(v <- variables){MyValueCount(v.value + offset) += 1}
    for(v <- range)assert(ValueCount(v).getValue(true) == MyValueCount(v),"" + ValueCount + MyValueCount)

    var MyViol:Int = 0
    for(v <- bounds.keys){
      MyViol += 0.max(bounds(v).value - MyValueCount(v+offset))
    }
    assert(Violation.value == MyViol)
    for(v <- variables){
      if(bounds.contains(v.value) && (MyValueCount(v.value + offset) <= bounds(v.value))){
        assert(getViolation(v).value == 0)
      }else{
        assert(getViolation(v).value == Violation.value)
      }
    }
  }
}

