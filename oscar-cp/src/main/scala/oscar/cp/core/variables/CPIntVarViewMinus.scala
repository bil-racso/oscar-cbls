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
package oscar.cp.core.variables

import scala.util.Random
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint

/**
 * Represents a view -x on variable x 
 * @author Pierre Schaus pschaus@gmail.com
 */
final class CPIntVarViewMinus(v: CPIntVar) extends CPIntVar {
    
  final override val store: CPStore = v.store
  
  final override val name: String = s"-${v.name}"
    
  def transform(v: Int) = -this.v.transform(v)    
	
	def isBound = v.isBound
	
	override def size = v.size
	
	override def isEmpty = v.isEmpty
	
	def constraintDegree = v.constraintDegree
	
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
  
  @inline final def restrict(newDomain: Array[Int], newSize: Int): Unit = {
    assert(newSize > 0 && newSize <= size)
    val mapped = new Array[Int](newSize)
    var i = newSize
    while (i > 0) {
      i -= 1
      mapped(i) = -newDomain(i)
    }
    v.restrict(mapped, newSize)
  }
	
	override def toString() = "-("+v+")"
		
	def callPropagateWhenBind(c: Constraint) = v.callPropagateWhenBind(c)
	
	def callPropagateWhenBoundsChange(c: Constraint) = v.callPropagateWhenBoundsChange(c)
	
	def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) = v.callPropagateWhenDomainChanges(c,trackDelta)
  
  
  final override def callPropagateWhenDomainChanges(c: Constraint, watcher: oscar.cp.core.Watcher) = v.callPropagateWhenDomainChanges(c,watcher)

	
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
	
	def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = v.delta(-oldMax,-oldMin,oldSize).map(-_)
  
  def fillDeltaArray(oldMin: Int, oldMax: Int, oldSize: Int, arr: Array[Int]): Int = {
    val m = v.fillDeltaArray(-oldMax,-oldMin,oldSize,arr)
    var i = 0
    while (i < m) {
      arr(i) = -arr(i)
      i += 1
    }
    m
  } 
	
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
  
  final override def fillDeltaArray(c: Constraint, arr: Array[Int]): Int = { 
    val m = v.fillDeltaArray(c,arr)
    var i = 0
    while (i < m) {
      arr(i) = -arr(i)
      i += 1
    }
    m
  }  
	
}
  
