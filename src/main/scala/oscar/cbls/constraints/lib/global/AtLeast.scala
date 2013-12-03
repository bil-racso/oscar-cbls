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
 *            Yoann Guyot
 ******************************************************************************/

package oscar.cbls.constraints.lib.global

import collection.immutable.SortedMap
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation.{InvariantHelper, Variable, IntVar}
import oscar.cbls.invariants.lib.logic.{DenseCount, IntElement, IntITE}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.invariants.lib.numeric.Sum

/**
 * Implement the AtLeast constraint on IntVars.
 * There is a set of minbounds, defined in the parameter bound as pair (value,minbound).
 * The variables should be such that there is at least ''minbound'' of them which have the value ''value''.
 *
 * @param variables the variable whose values are constrained
 * @param bounds map(value,minbound) specifying the minimal number of occurrence of ''value'' among the variables.
 * We use a map to ensure that there is no two bounds on the same value.
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class AtLeast(variables: Iterable[IntVar], bounds: SortedMap[Int, IntVar]) extends Constraint {

  model = InvariantHelper.findModel(variables)
  registerConstrainedVariables(variables)
  finishInitialization()

  private val countInvariant = DenseCount.makeDenseCount(variables.toArray)
  private val offset:Int = countInvariant.offset
  private val valueCount = countInvariant.counts //v => #occurrence of v+offset in variables

  private val Violation: IntVar = new IntVar(model, (0 to Int.MaxValue), 0, "ViolationsOfAtLeast")
  private val noViolation:IntVar = 0

  Violation <== Sum(bounds.toList.map((value_bound) => Max2(noViolation,value_bound._2 - valueCount(value_bound._1)).toIntVar))

  private val violationByVal=Array.tabulate(valueCount.length)(value => {
    if(bounds.contains(value + offset))
      IntITE(valueCount(value + offset) - bounds(value + offset), Violation, noViolation).toIntVar
    else Violation
    })

  //the violation of each input variable
  private val Violations:SortedMap[IntVar,IntVar] = variables.foldLeft(SortedMap.empty[IntVar,IntVar])((acc,intvar)
  => {
    val newVar = new IntVar(model,(0 to 1),1,"Violation_AtLeast_"+intvar.name)
    newVar <== violationByVal.element(intvar + offset)
    acc + ((intvar,newVar))
  })

  /**
   * the violation is the sum for all bounds of the number of missing variables to reach the bound
   */
  override def violation = Violation

  /**
   * The violation of a variable is zero if the value of the variable is the one of a bound that is not reached,
   * otherwise, it is equal to the global violation degree.
   */
  override def violation(v: Variable): IntVar = Violations(v.asInstanceOf[IntVar])

  override def checkInternals(c: Checker) {
    val (minMin,maxMax) = InvariantHelper.getMinMaxBounds(variables)
    var MyValueCount: SortedMap[Int,Int] = SortedMap.empty
    for(v <- variables){
      val oldCount = MyValueCount.getOrElse(v.value,0)
      MyValueCount += ((v.value,oldCount + 1))
    }

    for (v <- minMin to maxMax) {
      if (MyValueCount.isDefinedAt(v)){
        c.check(valueCount(v+offset).getValue(true) == MyValueCount(v),
          Some("ValueCount(" + v + "+offset).getValue(true) (" + valueCount(v).getValue(true)
            + ") == MyValueCount(" + v + ") (" + MyValueCount(v) + ")"))
      }else{
        c.check(valueCount(v+offset).getValue(true) == 0,
          Some("ValueCount(" + v + "+offset).getValue(true) (" + valueCount(v).getValue(true)
            + ") == 0"))
      }
    }

    var MyViol: Int = 0
    for (v <- bounds.keys) {
      MyViol += 0.max(bounds(v).value - MyValueCount.getOrElse(v + offset,0))
    }
    c.check(Violation.value == MyViol,
      Some("Violation.value (" + Violation.value + ") == MyViol (" + MyViol + ")"))

    for (v <- variables) {
      if (bounds.contains(v.value) && (MyValueCount(v.value + offset) <= bounds(v.value).value)) {
        c.check(violation(v).value == 0,
            Some("violation(" + v.name + ").value (" + violation(v).value + ") == 0"))
      } else {
        c.check(violation(v).value == Violation.value,
            Some("violation(" + v.name + ").value (" + violation(v).value
            + ") == Violation.value (" + Violation.value + ")"))
      }
    }
  }
}

