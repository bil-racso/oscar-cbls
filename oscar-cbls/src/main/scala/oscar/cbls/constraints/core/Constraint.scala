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


package oscar.cbls.constraints.core

import oscar.cbls.invariants.core.algo.dll.DelayedPermaFilteredDoublyLinkedList
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation._
import oscar.cbls.invariants.lib.numeric.Step

/**A constraint is a function that computes a degree of violation that is managed as any invariant.
 * This degree of violation is obtained through the violation method.
 * Furthermore, each variable involved in the constraint also gets an associated violation.
 * This variable-specific violation quantifies the involvement of the variable in the overall violation of the constraint.
 * It can be obtained through the violation(v: Variable) method.
 * All these violation are stored as IntVar, so that they can be involved in the construction of complex formulas,
 * and managed as invariants.
  * @author renaud.delandtsheer@cetic.be
 */
abstract class Constraint extends IntValue with IndirectPropagationElement{

  /** the value of a constraint is its truthValue, not its violation
    * notice that you cal also use Constraint in an invariant-based expression
    * @return
    */
  override def value: Int = if(isTrue) 1 else 0

  override def domain: Domain = 0 to 1

  override def name: String = this.getClass.getSimpleName

  override protected def indirectPE: PropagationElement = truthValue

  /** returns the violation associated with variable v in this constraint
   * all variables that are declared as constraint should have an associated violation degree.
    * notice that you cannot create any new invariant or variable in this method
    * because they can only be created before the model is closed.
    * */
  def violation(v: Value): IntValue

  /** returns the degree of violation of the constraint
    * notice that you cannot create any new invariant or variable in this method
    * because they can only be created before the model is closed.
    * @return
    */
  def violation: IntValue

  /**facility to check that the constraint is enforced
    * */
  final def isTrue: Boolean = (violation.value == 0)

  final val truthValue = Step(violation,0,0,1)

  /**the variables that are constrained by the constraint.
   * This should be read only. If you want to declare more constrained variables,
   * use the registerConstrainedVariable method. */
  private var _constrainedVariables:List[AbstractVariable] = List.empty
  def constrainedVariables = _constrainedVariables
  
  /**This should be called by the constraint to declare the set of constrained variables.
   * This should be done at the same time as the registration for invariant API.
   * The sole purpose of this is to know which variables have an associated degree of violation.
   * This is not correlated with the registration for dependencies in the invariants.
   * e.g.: A constraint can constrain a variable,
   * but subcontract the computation and implementation of the constraint to invariants.
   * Notice that all variables sent here which are actually constants are not kept, as they are not variables, actually.
   * This is tested by looking that the variable has a model associated.
    *
    * notice that constants will simply not be registered, so they will never have a violation degree stored anywhere.
    *
   * @param v the variable that is declared as constrained by the constraint
   */
  def registerConstrainedVariable(v: Value){
    v match{
      case c:AbstractVariable if (c.model != null) => _constrainedVariables = c :: constrainedVariables
    }   //TODO unsure if constraints can handle constraints as input parameter...
  }

  def registerConstrainedVariables(v: Value*){
    registerConstrainedVariables(v.toSeq)
  }

  def registerConstrainedVariables(v: Iterable[Value]){
    for (vv <- v){registerConstrainedVariable(vv)}
  }

  def checkInternals(c: Checker) {}
}
