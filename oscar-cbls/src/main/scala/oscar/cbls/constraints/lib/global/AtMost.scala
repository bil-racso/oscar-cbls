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
/******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.cbls.constraints.lib.global

import collection.immutable.SortedMap
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation.{IdentityInt, InvariantHelper, Variable, CBLSIntVar}
import oscar.cbls.invariants.lib.logic.{DenseCount, IntElement, Int2Int}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.invariants.lib.numeric.Sum

/**Implements the AtMost constraint on IntVar.
  * There is a set of bounds, defined in the parameter bound as pair (value,bound).
  * The variables should be such that there is at most ''bound'' of them which have the value ''value''.
  *
  * @param variables the variables that should be bounded, only one occurrence of each variable is tolerated
  * @param bounds map(value,bound) the bounds on the variables.
  *               We use a map to ensure that there is no two bounds on the same value.
  *               also, the variables specifying the size of the bound is cloned inside this constraint,
  *               so you cannot query its violation directly.
  *               You must call it though the constraint system, which is what you should be doing anyway.
  * @author renaud.delandtsheer@cetic.be
  */
case class AtMost(variables:Iterable[CBLSIntVar], bounds:SortedMap[Int, CBLSIntVar]) extends Constraint {

  def myMapValues[B,C](s:SortedMap[Int,B],f:B=>C):SortedMap[Int,C] =
    s.foldLeft[SortedMap[Int,C]](SortedMap.empty)((acc,couple) => acc+((couple._1,f(couple._2))))

  val mbounds:SortedMap[Int, CBLSIntVar] = myMapValues(bounds, (v:CBLSIntVar) => IdentityInt(v).toIntVar(v.name))
  assert(variables.size < Int.MaxValue)

  model = InvariantHelper.findModel(variables)
  registerConstrainedVariables(variables)
  registerConstrainedVariables(mbounds.values)
  finishInitialization()

  private val countInvariant = DenseCount.makeDenseCount(variables.toArray)
  private val offset:Int = countInvariant.offset
  private val valueCount = countInvariant.counts //v => #occurrence of v+offset in variables

  private val noViolation:CBLSIntVar = 0
  private val violationByVal=Array.tabulate(valueCount.length)(_ => noViolation)

  for((value,bound) <- mbounds){
    violationByVal(value) = Max2(noViolation,valueCount(value) - bound).toIntVar
  }

  //the violation of each input variable
  private val Violations:SortedMap[CBLSIntVar,CBLSIntVar] = {
    val violationForArray = variables.foldLeft(SortedMap.empty[CBLSIntVar,CBLSIntVar])(
      (acc,intvar) => acc + ((intvar,violationByVal.element(intvar + offset).toIntVar("Violation_AtMost_"+intvar.name)))
    )
    mbounds.foldLeft(violationForArray)(
      (acc,boundAndVariable) => {
        val viol = violationByVal.element(boundAndVariable._1).toIntVar("Violation_AtMost_"+bounds(boundAndVariable._1).name)
        acc + ((boundAndVariable._2, viol))
      })
  }

  private val Violation:CBLSIntVar = new CBLSIntVar(model,(0 to Int.MaxValue), 0,"ViolationsOfAtMost")
  Violation <== Sum(mbounds.keys.map(violationByVal))

  /**The violation of the constraint is the sum on all bound of the number of variable that are in excess.
    * the number of variable in excess is the max between zero and
    * (the number of variable that have the value of the bound minus the bound).
    */
  override def violation = Violation

  /**The violation of a variable is zero if its value is not the one of a bound.
    * If the variable has the value of a bound, its violation is the number of variable in excess for that bound.
    */
  override def violation(v: Variable):CBLSIntVar = {
    Violations(v.asInstanceOf[CBLSIntVar])
  }

  override def checkInternals(c: Checker) {
    println(this)
    var checkBounds:SortedMap[Int, Int] = SortedMap.empty
    for(i <- mbounds.keys) checkBounds += ((i,0))
    for (v <- variables) if (checkBounds.isDefinedAt(v.value)) checkBounds += ((v.value,checkBounds(v.value) +1))

    for (v <- variables){
      /*The violation of a variable is zero if its value is not the one of a bound.
        * If the variable has the value of a bound, its violation is the number of variable in excess for that bound.
        */
      val violationOfV = violation(v)
      val expectedViolation =
        if (checkBounds.isDefinedAt(v.value)) 0.max(checkBounds(v.value) - mbounds(v.value).value)
        else 0
      c.check(violationOfV.value == expectedViolation, Some("" + violationOfV + " == expectedViolation (" + expectedViolation + ")"))
    }

    /*The violation of the constraint is the sum on all bound of the number of variable that are in excess.
      * the number of variable in excess is the max between zero and
      * (the number of variable that have the value of the bound minus the bound).*/
    var summedViolation = 0
    for(i <- mbounds.keys){
      if (checkBounds(i) > mbounds(i).value) summedViolation += (checkBounds(i) - mbounds(i).value)
    }
    c.check(summedViolation == violation.value, Some("summedViolation ("+summedViolation+") == violation.value ("+violation.value+")"))
  }
}

