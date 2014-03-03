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
 * @author Cyrille Dejemeppe Cyrille.Dejemeppe@gmail.com
 * @author Steven Gay steven.gay@uclouvain.be
 */
class CPIntVarViewTimes(v: CPIntVar,val a: Int) extends CPIntVar(v.store) {
    
    
    def transform(v: Int) = a * this.v.transform(v)    
	
	def isBound = v.isBound
	
	override def size = v.size
	
	override def isEmpty = v.isEmpty
	
	def constraintDegree = v.constraintDegree()
	
	def isBoundTo(value: Int): Boolean = v.isBoundTo(a * value)
	
	def hasValue(value: Int): Boolean = v.hasValue(a * value)

	// Scala's division always rounds to the integer closest to zero, but we need flooring/ceiling versions.
	// The following divisions are just a little faster than using the modulo version,
	// and safer+faster than using casting to Double and using Double's ceil/floor 
	@inline
	def floor_div(a: Int, b:Int) = { 
      val q = a / b
      if (a < 0 && q * b != a) q - 1
      else q
    }
	
	@inline
	def ceiling_div(a: Int, b:Int) = {
      val q = a / b
      if (a > 0 && q * b != a) q + 1
      else q
    }
	
	def valueAfter(value: Int): Int = v.valueAfter(floor_div(value, a)) * a
	
	def valueBefore(value: Int): Int = v.valueBefore(ceiling_div(value, a)) * a
	
	def updateMin(value: Int) = v.updateMin(ceiling_div(value, a))
	
	def updateMax(value: Int) = v.updateMax(floor_div(value, a))
	
	def assign(value: Int) = if (value % a == 0) v.assign(value / a) else CPOutcome.Failure

	def removeValue(value: Int) = if (value % a == 0) v.removeValue(value / a) else CPOutcome.Suspend
	
	def min = a * v.min 
	
	def max = a * v.max
	
	def iterator = {
		v.iterator.map(_ * a)
	}
	
	override def toString() = "view with multiplicator " + a + " on (" + v + ")";
		
	def callPropagateWhenBind(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenBind(c)
	
	def callPropagateWhenBoundsChange(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenBoundsChange(c,trackDelta)
	
	def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenDomainChanges(c,trackDelta)
	
	// this method is useful when you have a view defined on a view
	def callValBindWhenBind(c: Constraint, variable: CPIntVar) = v.callValBindWhenBind(c, variable)
	
	def callValBindWhenBind(c: Constraint) = v.callValBindWhenBind(c,this)
	
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar) = v.callUpdateBoundsWhenBoundsChange(c, variable)
	
	def callUpdateBoundsWhenBoundsChange(c: Constraint) = v.callUpdateBoundsWhenBoundsChange(c,this)
	
	// this method is useful when you have a view defined on a view
	def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) = v.callValRemoveWhenValueIsRemoved(c,variable)
		
	def callValRemoveWhenValueIsRemoved(c: Constraint) = v.callValRemoveWhenValueIsRemoved(c,this)
	
	// this method is useful when you have a view defined on a view
	def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar,idx: Int) = v.callValBindIdxWhenBind(c, variable,idx)
	
	def callValBindIdxWhenBind(c: Constraint, idx: Int) = v.callValBindIdxWhenBind(c,this,idx)
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx);
		
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c,this,idx)
	

	
	// this method is useful when you have a view defined on a view
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) = v.callValRemoveIdxWhenValueIsRemoved(c,variable,idx)
	
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) = v.callValRemoveIdxWhenValueIsRemoved(c,this,idx)

	// ----------------------------------
	
	def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
	  assert(oldMin % a == 0)
	  assert(oldMax % a == 0)
	  v.delta(oldMin / a, oldMax / a, oldSize).map(_ * a)
	}
	
	def changed(c: Constraint): Boolean = v.changed(c)
	
	def minChanged(c: Constraint): Boolean = v.minChanged(c)
	
	def maxChanged(c: Constraint): Boolean = v.maxChanged(c)
	
	def boundsChanged(c: Constraint): Boolean = v.boundsChanged(c)
	
	def oldMin(c: Constraint): Int = v.oldMin(c) * a
	
	def oldMax(c: Constraint): Int = v.oldMax(c) * a
	
	def oldSize(c: Constraint): Int = v.oldSize(c)
	
	def deltaSize(c: Constraint): Int = v.deltaSize(c)
	
	def delta(c: Constraint): Iterator[Int] = {
	  v.delta(c).map(_ * a)
	}
	
}
  
