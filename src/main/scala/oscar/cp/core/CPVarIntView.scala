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

/**
 * Represents a view on variable applying an offset on it.
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPVarIntView(v: CPVarInt,val b: Int) extends CPVarInt(v.s) {
	
	def isBound = v.isBound
	
	override def size = v.size
	
	override def isEmpty = v.isEmpty
	
	def constraintDegree = v.constraintDegree()
	
	def isBoundTo(value: Int): Boolean = v.isBoundTo(value-b)
	
	def hasValue(value: Int): Boolean = v.hasValue(value-b)
	
	def valueAfter(value: Int): Int = v.valueAfter(value-b) + b
	
	def valueBefore(value: Int): Int = v.valueBefore(value-b) + b
	
	def updateMin(value: Int) = v.updateMin(value-b);
	
	def assign(value: Int) = v.assign(value-b)

	def updateMax(value: Int) = v.updateMax(value-b)
	
	def removeValue(value: Int) = v.removeValue(value-b)
	
	def min = v.min +b
	
	def max = v.max + b
	
	def iterator = {
		v.iterator.map(_ + b)
	}
	
	override def toString() = "view with shift "+b+" on ("+v+")";
		
	def callPropagateWhenBind(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenBind(c)
	
	def callPropagateWhenBoundsChange(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenBoundsChange(c,trackDelta)
	
	def callPropagateWhenMaxChanges(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenMaxChanges(c,trackDelta)
	
	def callPropagateWhenMinChanges(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenMinChanges(c,trackDelta)
	
	def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenDomainChanges(c,trackDelta)
	
	// this method is useful when you have a view defined on a view
	def callValBindWhenBind(c: Constraint, variable: CPVarInt, delta: Int) = v.callValBindWhenBind(c, variable, b+delta)
	
	def callValBindWhenBind(c: Constraint) = v.callValBindWhenBind(c,this,b)
	
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPVarInt, delta: Int) = v.callUpdateBoundsWhenBoundsChange(c, variable, b+delta)
	
	def callUpdateBoundsWhenBoundsChange(c: Constraint) = v.callUpdateBoundsWhenBoundsChange(c,this,b)
	
	// this method is useful when you have a view defined on a view
	def callUpdateMaxWhenMaxChanges(c: Constraint, variable: CPVarInt, delta: Int) = v.callUpdateMaxWhenMaxChanges(c,variable,b+delta)
	
	def callUpdateMaxWhenMaxChanges(c: Constraint) = v.callUpdateMaxWhenMaxChanges(c,this,b)
	
	// this method is useful when you have a view defined on a view
	def callUpdateMinWhenMinChanges(c: Constraint, variable: CPVarInt, delta: Int) = v.callUpdateMinWhenMinChanges(c,variable,b+delta);
	
	def callUpdateMinWhenMinChanges(c: Constraint) = v.callUpdateMinWhenMinChanges(c,this,b);
	
	// this method is useful when you have a view defined on a view
	def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPVarInt, delta: Int) = v.callValRemoveWhenValueIsRemoved(c,variable,b+delta);
		
	def callValRemoveWhenValueIsRemoved(c: Constraint) = v.callValRemoveWhenValueIsRemoved(c,this,b);
	
	// this method is useful when you have a view defined on a view
	def callValBindIdxWhenBind(c: Constraint, variable: CPVarInt,idx: Int, delta: Int) = v.callValBindIdxWhenBind(c, variable,idx, b+delta);
	
	def callValBindIdxWhenBind(c: Constraint, idx: Int) = v.callValBindIdxWhenBind(c,this,idx,b);
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx, b+delta);	
		
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c,this,idx,b);
	
	// this method is useful when you have a view defined on a view
	def callUpdateMaxIdxWhenMaxChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) = v.callUpdateMaxIdxWhenMaxChanges(c,variable,idx,b+delta);	
	
	def callUpdateMaxIdxWhenMaxChanges(c: Constraint, idx: Int) = v.callUpdateMaxIdxWhenMaxChanges(c,this,idx,b);
	
	// this method is useful when you have a view defined on a view
	def callUpdateMinIdxWhenMinChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) = v.callUpdateMinIdxWhenMinChanges(c,variable,idx,b+delta);
	
	def callUpdateMinIdxWhenMinChanges(c: Constraint, idx: Int) = v.callUpdateMinIdxWhenMinChanges(c,this,idx,b);
	
	// this method is useful when you have a view defined on a view
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) = v.callValRemoveIdxWhenValueIsRemoved(c,variable,idx,b+delta);
	
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) = v.callValRemoveIdxWhenValueIsRemoved(c,this,idx,b);

	// ----------------------------------
	
	def changed(c: Constraint): Boolean = v.changed(c)
	
	def minChanged(c: Constraint): Boolean = v.minChanged(c)
	
	def maxChanged(c: Constraint): Boolean = v.maxChanged(c)
	
	def boundsChanged(c: Constraint): Boolean = v.boundsChanged(c)
	
	def oldMin(c: Constraint): Int = v.oldMin(c) + b
	
	def oldMax(c: Constraint): Int = v.oldMax(c) + b
	
	def oldSize(c: Constraint): Int = v.oldSize(c)
	
	def deltaSize(c: Constraint): Int = v.deltaSize(c)
	
	def delta(c: Constraint): Iterator[Int] = v.delta(c).map(_ + b)
	
}
  
