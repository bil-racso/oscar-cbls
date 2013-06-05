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
package oscar.cp.core


abstract class CPVarSet(val s: Store,val name: String = "") {

    def store = s
  
	def constraintDegree(): Int
	
    /**
     * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
     */
	def isBound: Boolean
	
	/**
	 * 
	 * @param v
	 * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
	 */
	def isBoundTo(v: Int): Boolean
	
	/**
     * Test if a value is in the domain
     * @param val
     * @return  true if the domain contains the value val, false otherwise
     */
	def hasValue(value: Int): Boolean
	

	
    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever the domain of the variable changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenDomainChanges(c: Constraint): Unit

    /**
     * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callValRequiredWhenRequiredValue(c: Constraint): Unit
	
	
    /**
     * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */	
	def callValExcludedWhenExcludedValue(c: Constraint): Unit
	
    /**
     * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callValRequiredWhenRequiredValue(c: Constraint, idx: Int): Unit
	
	
    /**
     * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */	
	def callValExcludedWhenExcludedValue(c: Constraint, idx: Int): Unit	
	
	def requires(v: Int): CPOutcome
	
	def excludes(v: Int): CPOutcome
	
	def value(): Set[Int]
	
	
	
    /**
     * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if val was in the domain, Failure otherwise
     */
	def assign(value: Int): CPOutcome

    /**
     * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
     */
	def updateMin(value: Int): CPOutcome
	
     /**
     * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
     */
	def updateMax(value: Int): CPOutcome
	
	
    /**
     * Remove val from the domain, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if the domain is not equal to the singleton {val}, Failure otherwise
     */
	def removeValue(value: Int): CPOutcome
	
	

}

object CPVarSetInt {
  
  
}
  
