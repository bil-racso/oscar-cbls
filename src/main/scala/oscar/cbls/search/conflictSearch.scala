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

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.search

import oscar.cbls.invariants.core.computation.IntVar
import oscar.cbls.constraints.core.ConstraintSystem

/**generic search procedure
 * selects most violated variable, and assigns value that minimizes overall violation*/
object conflictSearch extends SearchEngine{
  /**Performs a conflict search on the constraint system
   * selects most violated variable, and assigns value that minimizes overall violation
   *
   * Beware: the constraint system can only constraint IntVar, no other types of variables are tolerated
   * @param c the constraint system: the searched variables sill be the ones that are constrained by it
   * @param MaxIt the maximal number of iterations
   */
  def apply(c:ConstraintSystem, MaxIt: Int) {
    var it = 0;
    val Variables:Array[IntVar] = c.getConstrainedVariables.asInstanceOf[Iterable[IntVar]].toArray;
    val Violations:Array[IntVar] = Variables.clone().map(c.getViolation)
    while (!c.isTrue && it < MaxIt) {
      val MaxViolVarID = selectMax(Variables.indices,Violations(_:Int).value)
      val NewVal = selectMin(Variables(MaxViolVarID).getDomain)(c.getAssignVal(Variables(MaxViolVarID),_:Int),i => true)
      Variables(MaxViolVarID) := NewVal
      it = it + 1;
    }
  }
}
