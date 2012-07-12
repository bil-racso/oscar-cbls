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
import oscar.cbls.invariants.core.computation.{Variable, IntVar}
import oscar.cbls.invariants.lib.logic.IntElement._
import oscar.cbls.invariants.lib.logic.IntVar2IntVarFun._
import oscar.cbls.invariants.lib.logic.{IntElement, IntVar2IntVarFun}
import oscar.cbls.invariants.lib.numeric.Implicits._;

//TODO: check
/**Implements the AtMost constraint on IntVar.
 * There is a set of bounds, defined in the parameter bound as pair (value,bound).
 * The variables should be such that there is at most ''bound'' of them which have the value ''value''.
 * WARNING: not tested!
 * @param variables the variables that should be bounded
 * @param bounds map(value,bound) the bounds on the variables. We use a map to ensure that there is no two bounds on the same value.
 */
case class AtMost(variables:Iterable[IntVar], bounds:SortedMap[Int, Int]) extends Constraint {
  assert(variables.size < Int.MaxValue)

  registerConstrainedVariablesAll(variables)
  registerStaticAndDynamicDependencyAllNoID(variables)
  finishInitialization()

  private val Violation:IntVar = new IntVar(model,0,Int.MaxValue,0,"ViolationsOfAtMost")
  Violation.setDefiningInvariant(this)

  private val N0:Int = variables.foldLeft(0)((acc:Int,intvar:IntVar) => (if(intvar.MaxVal > acc) intvar.MaxVal else acc))
  private val offset:Int = - variables.foldLeft(0)((acc:Int,intvar:IntVar) => (if(intvar.MinVal < acc) intvar.MinVal else acc))
  private val N = N0 + offset
  private val range = 0 to N

  private val Violations:SortedMap[IntVar,IntVar] = variables.foldLeft(SortedMap.empty[IntVar,IntVar])((acc,intvar)
  => {
    val newvar = new IntVar(model,0,1,1,"Violation_AtMost_"+intvar.name)
    acc + ((intvar,newvar))
  })

  private val ValueCount:Array[IntVar] = (for(i <- 0 to N) yield {
    val tmp = new IntVar(model,-1,variables.size,(if(Bound(i) == -1) -1 else 0),"AtMost_count_of_value_" + (i-offset))
    tmp.setDefiningInvariant(this)
    tmp}
    ).toArray

  private val Bound:Array[Int]= new Array[Int](N)
  for(v <- range){Bound.update(v,bounds.getOrElse(v,-1))}

  for(v <- variables){
    val varval = v.getValue()
    if(Bound(varval) != -1){
      ValueCount(varval + offset) :+= 1
    }
    Violations(v) <== (IntElement(v plus offset, ValueCount) minus IntVar2IntVarFun(v,(v:Int) => Bound(v+offset)))
  }

  for(i <- range){
    Violation :+= 0.max(ValueCount(i).getValue(true) - Bound(i))
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    val NewBounded = (Bound(NewVal+offset) == -1)
    val OldBounded = (Bound(OldVal+offset) == -1)

    if (!OldBounded) ValueCount(OldVal+offset) := ValueCount(OldVal+offset).getValue(true) - 1
    if (!NewBounded) ValueCount(NewVal+offset) := ValueCount(NewVal+offset).getValue(true) + 1

    if(NewBounded){
      if (OldBounded){
        val DeltaOldVal = if(ValueCount(OldVal+offset).getValue(true) - Bound(OldVal+offset) == 0) 0 else -1
        val DeltaNewVal = if(ValueCount(NewVal+offset).getValue(true) - Bound(NewVal+offset) == 1) 0 else 1
        Violation :+= (DeltaNewVal + DeltaOldVal)
      }else{
        val DeltaNewVal = if(ValueCount(NewVal+offset).getValue(true) > Bound(NewVal+offset)) 1 else 0
        Violation :+= DeltaNewVal
      }
    }else{
      if (OldBounded){
        val DeltaOldVal = if(ValueCount(OldVal+offset).getValue(true) >= Bound(OldVal+offset)) -1 else 0
        Violation :+= DeltaOldVal
      }
    }
  }

  /**The violation of the constraint is the sum on all bound of the number of variable that are in excess.
   * the number of variable in excess is the max between zero and
   * (the number of variable that have the value of the bound minus the bound).
   */
  override def getViolation = Violation

  /**The violation of a variable is zero if its value is not the one of a bound.
   * If the variable has the value of a bound, its violation is the number of variable in excess for that bound.
   */
  override def getViolation(v: Variable):IntVar = {
    val tmp:IntVar = Violations.getOrElse(v.asInstanceOf[IntVar],null)
    assert(tmp != null)
    tmp
  }

}
