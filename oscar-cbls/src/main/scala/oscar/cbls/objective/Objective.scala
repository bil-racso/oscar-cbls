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
package oscar.cbls.objective

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.invariants.core.computation._


case class Objective(Objective: CBLSIntVar) extends ObjectiveTrait {
  setObjectiveVar(Objective)
}

/**
 * a common class for modeling an objective, and querying its variations on different basic moves
  * @author renaud.delandtsheer@cetic.be
 */
trait ObjectiveTrait {
  var ObjectiveVar: CBLSIntVar = null

  def setObjectiveVar(v: CBLSIntVar) {
    ObjectiveVar = v
    ObjectiveVar.getPropagationStructure.registerForPartialPropagation(ObjectiveVar)
  }

  /**returns the value of the objective variable if the two variables a and b were swapped values.
   * This proceeds through explicit state change and restore.
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.invariants.core.computation.Store]]
   */
  def swapVal(a: CBLSIntVar, b: CBLSIntVar): Int = {
    a :=: b
    val NewVal = propagateObjective
    a :=: b
    NewVal
  }

  /**returns the value of the objective variable if variable a was assigned the value v.
   * This proceeds through explicit state change and restore.
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.invariants.core.computation.Store]]
   */
  def assignVal(a: CBLSIntVar, v: Int): Int = assignVal(List((a,v)))

  /**returns the value of the objective variable if the assignment described by parameter a was performed
   * This proceeds through explicit state change and restore.
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.invariants.core.computation.Store]]
   */
  def assignVal(a: Iterable[(CBLSIntVar, Int)]): Int = {
    //memorize
    val oldvals: Iterable[(CBLSIntVar, Int)] = a.foldLeft(List.empty[(CBLSIntVar, Int)])(
      (acc, IntVarAndInt) => ((IntVarAndInt._1, IntVarAndInt._1.value)) :: acc)
    //excurse
    for (assign <- a)
      assign._1 := assign._2
    val newObj = propagateObjective
    //undo
    for (assign <- oldvals)
      assign._1 := assign._2
    newObj
  }

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  def propagateObjective = ObjectiveVar.value
}
