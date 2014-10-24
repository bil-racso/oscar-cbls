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
  *         by Gaël Thouvenin
  ******************************************************************************/

package oscar.cbls.constraints.lib.global

import collection.immutable.SortedMap
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation.{InvariantHelper, Variable, CBLSIntVar}
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.numeric.Sum2

/**Implements the Exactly constraint on IntVar.
  * There is a set of bounds, defined in the parameter bound as pair (value,bound).
  * The variables should be such that there is at  exactly ''bound'' of them which have the value ''value''.
  *
  * @param variables the variables that should be bounded
  * @param bounds map(value,bound) the bounds on the variables. We use a map to ensure that there is no two bounds on the same value.
  * @author gael.thouvenin@student.umons.ac.be
  */
case class Exactly(variables:Iterable[CBLSIntVar], bounds:SortedMap[Int, CBLSIntVar]) extends Constraint {
  model = InvariantHelper.findModel(variables)
  registerConstrainedVariables(variables)
  registerConstrainedVariables(bounds.values)
  finishInitialization()


  private val least = AtLeast(variables, bounds)
  private val most = AtMost(variables, bounds)

  /** returns the violation associated with variable v in this constraint
    * all variables that are declared as constraint should have an associated violation degree. */
  override def violation(v: Variable): CBLSIntVar = Sum2(least.violation(v), most.violation(v))

  /** returns the degree of violation of the constraint */
  override def violation: CBLSIntVar =  Sum2(least.violation, most.violation)

  override def checkInternals(c: Checker) { least.checkInternals(c); most.checkInternals(c)}
}


