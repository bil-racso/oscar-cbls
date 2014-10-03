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
package oscar.cp.core;

import scala.util.Random

/**
 * Represents a view -x on variable x 
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPIntVarViewMinus(v: CPIntVar) extends CPIntVar(v.store) {
    
    
    def transform(v: Int) = -this.v.transform(v)    
	
	def isBound = v.isBound
	
	override def size = v.size
	
	override def isEmpty = v.isEmpty
	
	def constraintDegree = v.constraintDegree()
	
	def isBoundTo(value: Int): Boolean = v.isBoundTo(-value)
	
	def hasValue(value: Int): Boolean = v.hasValue(-value)
	
	def valueAfter(value: Int): Int = -v.valueBefore(-value)
	
	def valueBefore(value: Int): Int = -v.valueAfter(-value)
	
	def randomValue(rand: Random): Int = -v.randomValue(rand)
	
	def updateMin(value: Int) = v.updateMax(-value)
	
	def assign(value: Int) = v.assign(-value)

	def updateMax(value: Int) = v.updateMin(-value)
	
	def removeValue(value: Int) = v.removeValue(-value)
	
	def min = -v.max
	
	def max = -v.min
	
	def iterator = {
		v.iterator.map(-_)
	}
	
	override def toString() = "-("+v+")";
		
	def callPropagateWhenBind(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenBind(c)
	
	def callPropagateWhenBoundsChange(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenBoundsChange(c,trackDelta)
	
	def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenDomainChanges(c,trackDelta)
	
	// this method is useful when you have a view defined on a view
	def callValBindWhenBind(c: Constraint, variable: CPIntervalVar) = v.callValBindWhenBind(c, variable)
	
	def callValBindWhenBind(c: Constraint) = v.callValBindWhenBind(c,this)
	
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar) = v.callUpdateBoundsWhenBoundsChange(c, variable)
	
	def callUpdateBoundsWhenBoundsChange(c: Constraint) = v.callUpdateBoundsWhenBoundsChange(c,this)
	
	// this method is useful when you have a view defined on a view
	def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) = v.callValRemoveWhenValueIsRemoved(c,variable)
		
	def callValRemoveWhenValueIsRemoved(c: Constraint) = v.callValRemoveWhenValueIsRemoved(c,this)
	
	// this method is useful when you have a view defined on a view
	def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar,idx: Int) = v.callValBindIdxWhenBind(c, variable,idx)
	
	def callValBindIdxWhenBind(c: Constraint, idx: Int) = v.callValBindIdxWhenBind(c,this,idx)
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx);
		
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c,this,idx)
	

	
	// this method is useful when you have a view defined on a view
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) = v.callValRemoveIdxWhenValueIsRemoved(c,variable,idx)
	
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) = v.callValRemoveIdxWhenValueIsRemoved(c,this,idx)

	// ----------------------------------
	
	def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = v.delta(-oldMax,-oldMin,oldSize).map(-_)
	
	def changed(c: Constraint): Boolean = v.changed(c)
	
	def minChanged(c: Constraint): Boolean = v.maxChanged(c)
	
	def maxChanged(c: Constraint): Boolean = v.minChanged(c)
	
	def boundsChanged(c: Constraint): Boolean = v.boundsChanged(c)
	
	def oldMin(c: Constraint): Int = -v.oldMax(c)
	
	def oldMax(c: Constraint): Int = -v.oldMin(c)
	
	def oldSize(c: Constraint): Int = v.oldSize(c)
	
	def deltaSize(c: Constraint): Int = v.deltaSize(c)
	
	def delta(c: Constraint): Iterator[Int] = {
	  v.delta(c).map(-_)
	}
	
}
  
