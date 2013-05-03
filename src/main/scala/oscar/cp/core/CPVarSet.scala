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
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound).
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenDomainChanges(c: Constraint): Unit

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the maximum or the minimum value of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenRequiredValue(c: Constraint): Unit

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the maximum of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenMaxChanges(c: Constraint): Unit

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the minimum of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenMinChanges(c: Constraint): Unit

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * one of the value is removed from the domain
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenDomainChanges(c: Constraint): Unit

    /**
     * Level 1 registration: ask that the valBind(CPVarInt) method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound).
     * @param c
     * @see oscar.cp.core.Constraint#valBind(CPVarInt)
     */
	def callValBindWhenBind(c: Constraint): Unit
	
	def callValBindWhenBind(c: Constraint, variable: CPVarInt, delta: Int): Unit

    /**
     * Level 1 registration: ask that the updateBounds(CPVarInt) method of the constraint c is called whenever
     * the minimum or maximum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateBounds(CPVarInt)
     */
	def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit
	
	def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPVarInt,delta: Int): Unit

    /**
     * Level 1 registration: ask that the updateMax(CPVarInt, int) method of the constraint c is called whenever
     * the maximum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateMax(CPVarInt, int)
     */
	def callUpdateMaxWhenMaxChanges(c: Constraint): Unit
	
	def callUpdateMaxWhenMaxChanges(c: Constraint, variable: CPVarInt, delta: Int): Unit

     /**
     * Level 1 registration: ask that the updateMin(CPVarInt, int) method of the constraint c is called whenever
     * the minimum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateMin(CPVarInt, int)
     */
	def callUpdateMinWhenMinChanges(c: Constraint): Unit
	
	def callUpdateMinWhenMinChanges(c: Constraint, variable: CPVarInt, delta: Int): Unit

    /**
     * Level 1 registration: ask that the valRemove(CPVarInt, int) method of the constraint c is called for each
     * value deletion from the domain
     * @param c
     * @see oscar.cp.core.Constraint#valRemove(CPVarInt, int)
     */
	def callValRemoveWhenValueIsRemoved(c: Constraint): Unit
	
	def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPVarInt, delta: Int): Unit

    /**
     * Level 1 registration: ask that the valRemoveIdx(CPVarInt, int, int) method of the constraint c is called for each
     * value deletion from the domain
     * @param c
     * @param idx, an index that will be given as parameter to valRemoveIdx(CPVarInt, int, int)
     * @see Constraint#valRemoveIdx(CPVarInt, int, int)
     */
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int): Unit
	
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit
	
    /**
     * Level 1 registration: ask that the updateMinIdx(CPVarInt, int, int) method of the constraint c is called whenever
     * the minimum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateMinIdx(CPVarInt, int, int)
     * @see Constraint#updateMinIdx(CPVarInt, int, int)
     */
	def callUpdateMinIdxWhenMinChanges(c: Constraint , idx: Int): Unit


	def callUpdateMinIdxWhenMinChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit

    /**
     * Level 1 registration: ask that the updateMaxIdx(CPVarInt, int, int) method of the constraint c is called whenever
     * the maximum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateMaxIdx(CPVarInt, int, int)
     * @see Constraint#updateMaxIdx(CPVarInt, int, int)
     */
	def callUpdateMaxIdxWhenMaxChanges(c: Constraint , idx: Int): Unit

	def callUpdateMaxIdxWhenMaxChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit

    /**
     * Level 1 registration: ask that the updateBoundsIdx(CPVarInt, int) method of the constraint c is called whenever
     * the minimum or maximum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateBoundsIdx(CPVarInt, int)
     * @see Constraint#updateBoundsIdx(CPVarInt, int)
     */
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint , idx: Int): Unit
	
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit

    /**
     * Level 1 registration: ask that the valBindIdx(CPVarInt, int) method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound).
     * @param c
     * @param idx, an index that will be given as parameter to valBindIdx(CPVarInt, int)
     * @see Constraint#valBindIdx(CPVarInt, int)
     */
	def callValBindIdxWhenBind(c: Constraint , idx: Int): Unit
	
	def callValBindIdxWhenBind(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit
	
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
  
