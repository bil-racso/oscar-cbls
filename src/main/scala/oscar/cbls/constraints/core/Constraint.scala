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


package oscar.cbls.constraints.core

import oscar.cbls.invariants.core.computation.{Variable, IntVar, IntInvariant}
import oscar.cbls.invariants.lib.numeric.Step

/**A constraint is a function that computes a degree of violation that is managed as any invariant.
 * This degree of violation is obtained through the violation method.
 * Furthermore, each variable involved in the constraint also gets an associated violation.
 * This variable-specific violation quantifies the involvement of the variable in the overall violation of he constraint.
 * It can be obtained through the violation(v:Variable) method.
 * All these violation are stored as IntVar, so that they can be involved in the construction of complex formulas,
 * and managed as invariants.
 */
abstract class Constraint extends IntInvariant{

  /** returns the violation associated with variable v in this constraint
   * all variables that are declared as constraint should have an associated violation degree. */
  def violation(v:Variable):IntVar

  /**returns the degree of violation of the constraint*/
  def violation:IntVar

  /**facility to check that the constraint is enforced*/
  final def isTrue:Boolean = (violation.value == 0)

  def myMin = 0
  def myMax = 1

  /**the output var of a constraint is whether the constraint is true or not; it is not the violation degree*/
  override def setOutputVar(v: IntVar) {v <== Step(violation,0,0,1)}

  /**the variables that are constrained by the constraint.
   * This should be read only. If you want to declare more constrained variables,
   * use the registerConstrainedVariable method. */
  var constrainedVariables:List[Variable] = List.empty

  /**This should be called by the constraint to declare the set of constrained variables.
   * this should be done at the same time as the registration for invariant API
   * The sole purpose of this is to know which variable have an associated degree of violation
   * this is not correlated wit the registration for dependencies in the invariants.
   * eg: A constraint can constraint a variable,
   * but subcontract the computation and implementation of the constraint to invariants
   * notice that all variables sent here which are actually constants are not kept, as they are not variables, actually.
   * This is tested by looking that the variable has a model associated.
   * @param v the variable that is declared as constrained by the constraint
   */
  def registerConstrainedVariable(v:Variable){
    if (v.model != null) constrainedVariables = v :: constrainedVariables
  }

  def registerConstrainedVariables(v:Variable*){
    for (vv <- v){registerConstrainedVariable(vv)}
  }

  def registerConstrainedVariablesAll(v:Iterable[Variable]){
    for (vv <- v){registerConstrainedVariable(vv)}
  }
}
