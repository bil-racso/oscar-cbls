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

import oscar.reversible.ReversibleQueue
import oscar.reversible.ReversiblePointer
import oscar.cp.core.CPOutcome._ 


/**
 * @author Pierre Schaus
 */
abstract class CPVarSet(val s: CPStore,min: Int, max: Int, val name: String = "") {

    def store = s
    val dom = new SetDomain(s,min,max);
    

	val onDomainL2 = new ReversibleQueue[Constraint](s)

	val onRequiredL1    = new ReversiblePointer[PropagEventQueueVarSet](s,null)
	val onExcludedL1    = new ReversiblePointer[PropagEventQueueVarSet](s,null)
	val onRequiredIdxL1    = new ReversiblePointer[PropagEventQueueVarSet](s,null)
	val onExcludedIdxL1    = new ReversiblePointer[PropagEventQueueVarSet](s,null)
    
    
  
	def constraintDegree(): Int
	
    /**
     * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
     */
	def isBound: Boolean = dom.possibleSize == dom.requiredSize
	

	
	/**
     * Test if a value is in the possible values
     * @param val
     * @return  true if value is in the possible values false otherwise
     */
	def isPossibleValue(value: Int) = dom.isPossible(value)

	/**
     * Test if a value is in the required values
     * @param val
     * @return  true if value is in the required values false otherwise
     */
	def isRequiredValue(value: Int) = dom.isRequired(value)	

	
    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever the domain of the variable changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenDomainChanges(c: Constraint) {
		onDomainL2.setValue(new Queue[Constraint](onDomainL2.value,c));
	  
	}

    /**
     * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callValRequiredWhenRequiredValue(c: Constraint) {
	  //onRequiredL1.setValue(new PropagEventQueue(onRequiredL1.value,c,this,0));
	}
	
	
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
	
	def requires(v: Int): CPOutcome = {
	  Suspend
	}
	
	def excludes(v: Int): CPOutcome = {
	  Suspend
	}
	
	def value(): Set[Int] = dom.requiredSet
	
	def requiredSet(): Set[Int] = dom.requiredSet
	
	def possibleSet(): Set[Int] = dom.possibleSet
	
	
	
	

}

object CPVarSetInt {
  
  
}
  
