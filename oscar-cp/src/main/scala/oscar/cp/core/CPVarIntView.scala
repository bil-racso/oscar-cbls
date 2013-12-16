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
  
    def rootVar: CPVarInt = v.rootVar
    
    def offset: Int = b + v.offset
  
    val root = rootVar
    
    val btot = offset
	
	def isBound = root.isBound
	
	override def size = root.size
	
	override def isEmpty = root.isEmpty
	
	def constraintDegree = root.constraintDegree()
	
	def isBoundTo(value: Int): Boolean = root.isBoundTo(value-btot)
	
	def hasValue(value: Int): Boolean = root.hasValue(value-btot)
	
	def valueAfter(value: Int): Int = root.valueAfter(value-btot) + btot
	
	def valueBefore(value: Int): Int = root.valueBefore(value-btot) + btot
	
	def updateMin(value: Int) = root.updateMin(value-btot);
	
	def assign(value: Int) = root.assign(value-btot)

	def updateMax(value: Int) = root.updateMax(value-btot)
	
	def removeValue(value: Int) = root.removeValue(value-btot)
	
	def min = root.min + btot
	
	def max = root.max + btot
	
	def iterator = {
		root.iterator.map(_ + btot)
	}
	
	override def toString() = "view with shift "+btot+" on ("+root+")";
		
	def callPropagateWhenBind(c: Constraint, trackDelta: Boolean = false) = root.callPropagateWhenBind(c)
	
	def callPropagateWhenBoundsChange(c: Constraint, trackDelta: Boolean = false) = root.callPropagateWhenBoundsChange(c,trackDelta)
	
	def callPropagateWhenMaxChanges(c: Constraint, trackDelta: Boolean = false) = root.callPropagateWhenMaxChanges(c,trackDelta)
	
	def callPropagateWhenMinChanges(c: Constraint, trackDelta: Boolean = false) = root.callPropagateWhenMinChanges(c,trackDelta)
	
	def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) = root.callPropagateWhenDomainChanges(c,trackDelta)
	
	// this method is useful when you have a view defined on a view
	def callValBindWhenBind(c: Constraint, variable: CPVarInt, delta: Int) = root.callValBindWhenBind(c, variable, btot)
	
	def callValBindWhenBind(c: Constraint) = root.callValBindWhenBind(c,this,btot)
	
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPVarInt, delta: Int) = root.callUpdateBoundsWhenBoundsChange(c, variable, btot)
	
	def callUpdateBoundsWhenBoundsChange(c: Constraint) = root.callUpdateBoundsWhenBoundsChange(c,this,btot)
	
	// this method is useful when you have a view defined on a view
	def callUpdateMaxWhenMaxChanges(c: Constraint, variable: CPVarInt, delta: Int) = root.callUpdateMaxWhenMaxChanges(c,variable,btot)
	
	def callUpdateMaxWhenMaxChanges(c: Constraint) = root.callUpdateMaxWhenMaxChanges(c,this,btot)
	
	// this method is useful when you have a view defined on a view
	def callUpdateMinWhenMinChanges(c: Constraint, variable: CPVarInt, delta: Int) = root.callUpdateMinWhenMinChanges(c,variable,btot);
	
	def callUpdateMinWhenMinChanges(c: Constraint) = root.callUpdateMinWhenMinChanges(c,this,btot);
	
	// this method is useful when you have a view defined on a view
	def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPVarInt, delta: Int) = root.callValRemoveWhenValueIsRemoved(c,variable,btot);
		
	def callValRemoveWhenValueIsRemoved(c: Constraint) = root.callValRemoveWhenValueIsRemoved(c,this,btot);
	
	// this method is useful when you have a view defined on a view
	def callValBindIdxWhenBind(c: Constraint, variable: CPVarInt,idx: Int, delta: Int) = root.callValBindIdxWhenBind(c, variable,idx, btot);
	
	def callValBindIdxWhenBind(c: Constraint, idx: Int) = root.callValBindIdxWhenBind(c,this,idx,btot);
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) = root.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx, btot);	
		
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = root.callUpdateBoundsIdxWhenBoundsChange(c,this,idx,btot);
	
	// this method is useful when you have a view defined on a view
	def callUpdateMaxIdxWhenMaxChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) = root.callUpdateMaxIdxWhenMaxChanges(c,variable,idx,btot);	
	
	def callUpdateMaxIdxWhenMaxChanges(c: Constraint, idx: Int) = root.callUpdateMaxIdxWhenMaxChanges(c,this,idx,btot);
	
	// this method is useful when you have a view defined on a view
	def callUpdateMinIdxWhenMinChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) = root.callUpdateMinIdxWhenMinChanges(c,variable,idx,btot);
	
	def callUpdateMinIdxWhenMinChanges(c: Constraint, idx: Int) = root.callUpdateMinIdxWhenMinChanges(c,this,idx,btot);
	
	// this method is useful when you have a view defined on a view
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) = root.callValRemoveIdxWhenValueIsRemoved(c,variable,idx,btot);
	
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) = root.callValRemoveIdxWhenValueIsRemoved(c,this,idx,btot);

	// ----------------------------------
	
	def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = root.delta(oldMin-btot,oldMax-btot,oldSize).map(_ + btot)
	
	def changed(c: Constraint): Boolean = root.changed(c)
	
	def minChanged(c: Constraint): Boolean = root.minChanged(c)
	
	def maxChanged(c: Constraint): Boolean = root.maxChanged(c)
	
	def boundsChanged(c: Constraint): Boolean = root.boundsChanged(c)
	
	def oldMin(c: Constraint): Int = root.oldMin(c) + btot
	
	def oldMax(c: Constraint): Int = root.oldMax(c) + btot
	
	def oldSize(c: Constraint): Int = root.oldSize(c)
	
	def deltaSize(c: Constraint): Int = root.deltaSize(c)
	
	def delta(c: Constraint): Iterator[Int] = {
	  val sn = c.snapshotsVarInt(root)
	  root.delta(sn.oldMin,sn.oldMax,sn.oldSize).map(_ + btot)
	}
	
}
  
