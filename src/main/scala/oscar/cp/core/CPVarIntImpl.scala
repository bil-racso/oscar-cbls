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
import scala.collection._
import scala.collection.generic._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPVarIntImpl(st: CPStore, minimum: Int, maximum: Int, name: String = "") extends CPVarInt(st,name) {
  
	val dom = new IntDomain(s,minimum,maximum)
	val onMinL2    = new ReversiblePointer[ConstraintQueue](s,null)
	val onMaxL2    = new ReversiblePointer[ConstraintQueue](s,null)
	val onBoundsL2 = new ReversiblePointer[ConstraintQueue](s,null)
	val onBindL2   = new ReversiblePointer[ConstraintQueue](s,null)
	val onDomainL2 = new ReversiblePointer[ConstraintQueue](s,null)

	val onMinL1    = new ReversiblePointer[PropagEventQueueVarInt](s,null)
	val onMaxL1    = new ReversiblePointer[PropagEventQueueVarInt](s,null)
	val onBoundsL1 = new ReversiblePointer[PropagEventQueueVarInt](s,null)
	val onBindL1   = new ReversiblePointer[PropagEventQueueVarInt](s,null)
	val onDomainL1 = new ReversiblePointer[PropagEventQueueVarInt](s,null)

	val onMinIdxL1    = new ReversiblePointer[PropagEventQueueVarInt](s,null)
	val onMaxIdxL1    = new ReversiblePointer[PropagEventQueueVarInt](s,null)
	val onBoundsIdxL1 = new ReversiblePointer[PropagEventQueueVarInt](s,null)
	val onBindIdxL1   = new ReversiblePointer[PropagEventQueueVarInt](s,null)
	val onDomainIdxL1 = new ReversiblePointer[PropagEventQueueVarInt](s,null)
	
	/**
     * Builds a variable with domain defined by the range into the store s
     * @param r a scala range
     */
	def this(st: CPStore, r: Range) = this(st, r.start,if (r.isInclusive) r.end else r.end-1)	
	

	def rootVar: CPVarInt = this
    
    def offset: Int = 0
    
	
	def iterator = {
	  dom.iterator
	}
	
	
    /**
	 * 
	 * @return The number of propagation methods of L2 attached to changes of this variables.
	 */
	def constraintDegree() = {
		var tot = 0
		if (onMinL2.hasValue()) tot += onMinL2.value.size
		if (onMaxL2.hasValue()) tot += onMaxL2.value.size
		if (onBoundsL2.hasValue()) tot += onBoundsL2.value.size
		if (onBindL2.hasValue()) tot += onBindL2.value.size
		if (onDomainL2.hasValue()) tot += onDomainL2.value.size
		
		if (onMinL1.hasValue())    tot += onMinL1.value.size
		if (onMaxL1.hasValue())    tot += onMaxL1.value.size
		if (onBoundsL1.hasValue()) tot += onBoundsL1.value.size
		if (onBindL1.hasValue())   tot += onBindL1.value.size
		if (onDomainL1.hasValue()) tot += onDomainL1.value.size
		
		if (onMinIdxL1.hasValue())    tot += onMinIdxL1.value.size
		if (onMaxIdxL1.hasValue())    tot += onMaxIdxL1.value.size
		if (onBoundsIdxL1.hasValue()) tot += onBoundsIdxL1.value.size
		if (onBindIdxL1.hasValue())   tot += onBindIdxL1.value.size
		if (onDomainIdxL1.hasValue()) tot += onDomainIdxL1.value.size	
		tot
	}
	
    /**
     * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
     */
	def isBound = {
        assert(!s.isFailed())
		size == 1
	}
	
	/**
	 * 
	 * @param v
	 * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
	 */
	def isBoundTo(v: Int) = {
		isBound && value == v
	}	
	
	/**
     * Test if a value is in the domain
     * @param val
     * @return  true if the domain contains the value val, false otherwise
     */
	def hasValue(value: Int) = dom.hasValue(value)
	

	
    /**
     * @param val
     * @return the smallest value > val in the domain, None if there is not value > val in the domain
     */
	def valueAfter(value: Int): Int = {
		if (max < value+1) {
			println("error: no value after "+value+" maximum="+max)
			value
		} else {
			dom.getNextValue(value+1)
		}
	}	
	
    /**
     * @param val
     * @return the largest value < val in the domain, None if there is not value < val in the domain
     */
	def valueBefore(value: Int): Int = {
		if (min > value-1) {
			println("error: no value before "+value+" minimum="+min)
			value
		} else {
			dom.getPrevValue(value-1)
		}
	}

	/**
     * @return  the size of the domain
     */
	override def size = dom.size
	
    /**
     * @return true if the domain is empty, false otherwise
     */
	override def isEmpty = dom.isEmpty

    /**
     * @return  the minimum value in the domain
     */
	def min = {
	  assert(!dom.isEmpty)
	  dom.min
	}

	 /**
     * @return  the maximum value in the domain
     */
	def max = {
	  assert(!dom.isEmpty)
	  dom.max
	}

	override def toString(): String =  {
		if (isEmpty) name+" phi"
		else if (isBound) name + (if(name.isEmpty) "" else " ") + value
		else name + (if(name.isEmpty) "" else " ") + dom.toString()
	}
		
	
    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound).
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenBind(c: Constraint, trackDelta: Boolean = false) {
		onBindL2.setValue(new ConstraintQueue(onBindL2.value,c))
		if (trackDelta) c.addSnapshot(this)
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the maximum or the minimum value of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenBoundsChange(c: Constraint, trackDelta: Boolean = false) {
		onBoundsL2.setValue(new ConstraintQueue(onBoundsL2.value,c))
		if (trackDelta) c.addSnapshot(this)
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the maximum of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenMaxChanges(c: Constraint, trackDelta: Boolean = false) {
		onMaxL2.setValue(new ConstraintQueue(onMaxL2.value,c))
		if (trackDelta) c.addSnapshot(this)
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the minimum of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenMinChanges(c: Constraint, trackDelta: Boolean = false) {
		onMinL2.setValue(new ConstraintQueue(onMinL2.value,c))
		if (trackDelta) c.addSnapshot(this)
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * one of the value is removed from the domain
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) {
		onDomainL2.setValue(new ConstraintQueue(onDomainL2.value,c))
		if (trackDelta) c.addSnapshot(this)
	}

    /**
     * Level 1 registration: ask that the valBind(CPVarInt) method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound).
     * @param c
     * @see oscar.cp.core.Constraint#valBind(CPVarInt)
     */
	def callValBindWhenBind(c: Constraint) {
		callValBindWhenBind(c,this,0)
	}
	
	def callValBindWhenBind(c: Constraint, variable: CPVarInt, delta: Int) {
		onBindL1.setValue(new PropagEventQueueVarInt(onBindL1.value,c,variable,delta))
	}

    /**
     * Level 1 registration: ask that the updateBounds(CPVarInt) method of the constraint c is called whenever
     * the minimum or maximum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateBounds(CPVarInt)
     */
	def callUpdateBoundsWhenBoundsChange(c: Constraint) {
		callUpdateBoundsWhenBoundsChange(c,this,0)
	}
	
	def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPVarInt,delta: Int) {
		onBoundsL1.setValue(new PropagEventQueueVarInt(onBoundsL1.value,c,variable,delta))
	}

    /**
     * Level 1 registration: ask that the updateMax(CPVarInt, int) method of the constraint c is called whenever
     * the maximum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateMax(CPVarInt, int)
     */
	def callUpdateMaxWhenMaxChanges(c: Constraint) {
		callUpdateMaxWhenMaxChanges(c,this,0)
	}
	
	def callUpdateMaxWhenMaxChanges(c: Constraint, variable: CPVarInt, delta: Int) {
		onMaxL1.setValue(new PropagEventQueueVarInt(onMaxL1.value,c,variable,delta))
	}

     /**
     * Level 1 registration: ask that the updateMin(CPVarInt, int) method of the constraint c is called whenever
     * the minimum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateMin(CPVarInt, int)
     */
	def callUpdateMinWhenMinChanges(c: Constraint) {
		callUpdateMinWhenMinChanges(c,this,0)
	}
	
	def callUpdateMinWhenMinChanges(c: Constraint, variable: CPVarInt, delta: Int) {
		onMinL1.setValue(new PropagEventQueueVarInt(onMinL1.value,c,variable,delta))
	}

    /**
     * Level 1 registration: ask that the valRemove(CPVarInt, int) method of the constraint c is called for each
     * value deletion from the domain
     * @param c
     * @see oscar.cp.core.Constraint#valRemove(CPVarInt, int)
     */
	def callValRemoveWhenValueIsRemoved(c: Constraint) {
		callValRemoveWhenValueIsRemoved(c,this,0)
	}
	
	def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPVarInt, delta: Int) {
		onDomainL1.setValue(new PropagEventQueueVarInt(onDomainL1.value,c,variable,delta))
	}

    /**
     * Level 1 registration: ask that the valRemoveIdx(CPVarInt, int, int) method of the constraint c is called for each
     * value deletion from the domain
     * @param c
     * @param idx, an index that will be given as parameter to valRemoveIdx(CPVarInt, int, int)
     * @see Constraint#valRemoveIdx(CPVarInt, int, int)
     */
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) {
		callValRemoveIdxWhenValueIsRemoved(c,this,idx,0)
	}
	
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onDomainIdxL1.setValue(new PropagEventQueueVarInt(onDomainIdxL1.value,c,variable,idx,delta))
	}

    /**
     * Level 1 registration: ask that the updateMinIdx(CPVarInt, int, int) method of the constraint c is called whenever
     * the minimum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateMinIdx(CPVarInt, int, int)
     * @see Constraint#updateMinIdx(CPVarInt, int, int)
     */
	def callUpdateMinIdxWhenMinChanges(c: Constraint , idx: Int) {
		callUpdateMinIdxWhenMinChanges(c,this,idx,0)
	}


	def callUpdateMinIdxWhenMinChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onMinIdxL1.setValue(new PropagEventQueueVarInt(onMinIdxL1.value,c,variable,idx,delta))
	}

    /**
     * Level 1 registration: ask that the updateMaxIdx(CPVarInt, int, int) method of the constraint c is called whenever
     * the maximum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateMaxIdx(CPVarInt, int, int)
     * @see Constraint#updateMaxIdx(CPVarInt, int, int)
     */
	def callUpdateMaxIdxWhenMaxChanges(c: Constraint , idx: Int) {
		callUpdateMaxIdxWhenMaxChanges(c,this,idx,0)
	}

	def callUpdateMaxIdxWhenMaxChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onMaxIdxL1.setValue(new PropagEventQueueVarInt(onMaxIdxL1.value,c,variable, idx,delta))	
	}

    /**
     * Level 1 registration: ask that the updateBoundsIdx(CPVarInt, int) method of the constraint c is called whenever
     * the minimum or maximum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateBoundsIdx(CPVarInt, int)
     * @see Constraint#updateBoundsIdx(CPVarInt, int)
     */
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint , idx: Int) {
		callUpdateBoundsIdxWhenBoundsChange(c,this,idx,0)
	}
	
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onBoundsIdxL1.setValue(new PropagEventQueueVarInt(onBoundsIdxL1.value,c,variable,idx,delta))
	}

    /**
     * Level 1 registration: ask that the valBindIdx(CPVarInt, int) method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound).
     * @param c
     * @param idx, an index that will be given as parameter to valBindIdx(CPVarInt, int)
     * @see Constraint#valBindIdx(CPVarInt, int)
     */
	def callValBindIdxWhenBind(c: Constraint , idx: Int) {
		callValBindIdxWhenBind(c,this,idx,0)
	}
	
	def callValBindIdxWhenBind(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onBindIdxL1.setValue(new PropagEventQueueVarInt(onBindIdxL1.value,c,variable,idx,delta))
	}	
	
	
    /**
     * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if val was in the domain, Failure otherwise
     */
	def assign(value: Int): CPOutcome = {
		if (!hasValue(value)) return CPOutcome.Failure
		else if(isBound) return CPOutcome.Suspend
		else{ // more than one value
			val assignToMax = max == value
			val assignToMin = min == value
			// -------- AC3 notifications ------------
			if (!assignToMax)
                s.notifyL2(onMaxL2.value)
			if (!assignToMin)
                s.notifyL2(onMinL2.value)
			s.notifyL2(onBoundsL2.value)
			s.notifyL2(onDomainL2.value)
			s.notifyL2(onBindL2.value)
			// --------- AC5 notifications ------------
			s.notifyBindL1(onBindL1.value,this)
			s.notifyBindIdxL1(onBindIdxL1.value,this)
			s.notifyUpdateBoundsL1(onBoundsL1.value,this)
			s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value,this)
			if (value == dom.getMin()) {
				s.notifyUpdateMaxL1(onMaxL1.value,this,dom.getMax())
				s.notifyUpdateMaxIdxL1(onMaxIdxL1.value,this,dom.getMax())
			} 
			else if (value == dom.getMax()) {
				s.notifyUpdateMinL1(onMinL1.value,this,dom.getMin())
				s.notifyUpdateMinIdxL1(onMinIdxL1.value,this,dom.getMin())
			} else {
				s.notifyUpdateMaxL1(onMaxL1.value, this,dom.getMax())
				s.notifyUpdateMaxIdxL1(onMaxIdxL1.value,this, dom.getMax())
				s.notifyUpdateMinL1(onMinL1.value, this,dom.getMin())
				s.notifyUpdateMinIdxL1(onMinIdxL1.value,this, dom.getMin())
			}
			// must notify AC5 event before the actual removal
			if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
			    var i = dom.getMin()
			    while (i <= dom.getMax()) {
					if (i!= value && dom.hasValue(i)) {
						if (onDomainL1.hasValue()) {
							s.notifRemoveL1(onDomainL1.value,this,i)
						}
						if (onDomainIdxL1.hasValue()) {
							s.notifyRemoveIdxL1(onDomainIdxL1.value,this,i)
						}
					}
					i += 1
				}
			}
			// finally do the assignment
			return dom.assign(value)
		}
	}

    /**
     * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
     */
	def updateMin(value: Int): CPOutcome = {
		
		if (value > dom.getMax()) return CPOutcome.Failure
		if (value <= dom.getMin()) return CPOutcome.Suspend
		
		val omin = dom.min
		
		//must notif AC5 event with the removed values before the actual removal
		if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
			var i = dom.min
			while (i < value) {
				if(dom.hasValue(i)) {
					if (onDomainL1.hasValue())
						s.notifRemoveL1(onDomainL1.value,this,i)
					if (onDomainIdxL1.hasValue())
						s.notifyRemoveIdxL1(onDomainIdxL1.value,this,i)
				}
				i += 1
			}
		}
		
		val ok = dom.updateMin(value)
		assert(ok != CPOutcome.Failure)
	
		if (dom.size == 1) {
			assert(isBound)
			s.notifyBindL1(onBindL1.value,this)
			s.notifyBindIdxL1(onBindIdxL1.value,this)
			s.notifyL2(onBindL2.value)
		}
		s.notifyUpdateMinL1(onMinL1.value,this,omin)
		s.notifyUpdateMinIdxL1(onMinIdxL1.value,this,omin)
		s.notifyL2(onMinL2.value)
		s.notifyUpdateBoundsL1(onBoundsL1.value,this)
		s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value,this)
		s.notifyL2(onBoundsL2.value)
		s.notifyL2(onDomainL2.value)
		return CPOutcome.Suspend
	}
	
	
     /**
     * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
     */
	def updateMax(value: Int): CPOutcome = {	
		if (value < dom.min) return CPOutcome.Failure
		if (value >= dom.max) return CPOutcome.Suspend
		
		val omax = dom.max
		
		//must notifyAC3 the removed value before the actual removal
		if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
		   var i = omax
		   while (i > value) {
				if(dom.hasValue(i)){
					if (onDomainL1.hasValue())
						s.notifRemoveL1(onDomainL1.value, this, i)
					if (onDomainIdxL1.hasValue())
						s.notifyRemoveIdxL1(onDomainIdxL1.value, this, i)
				}
				i -= 1
			}
		}
		
		val ok = dom.updateMax(value)
		assert(ok != CPOutcome.Failure)
		
		if(dom.size == 1){
			assert(isBound)
			s.notifyBindL1(onBindL1.value,this)
			s.notifyBindIdxL1(onBindIdxL1.value,this)
			s.notifyL2(onBindL2.value)
		}
		s.notifyUpdateMaxL1(onMaxL1.value, this,omax)
		s.notifyUpdateMaxIdxL1(onMaxIdxL1.value,this,omax)
		s.notifyL2(onMaxL2.value)
		s.notifyUpdateBoundsL1(onBoundsL1.value,this)
		s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value,this)
		s.notifyL2(onBoundsL2.value)
		s.notifyL2(onDomainL2.value)
		return CPOutcome.Suspend
	}
	
	
    /**
     * Remove val from the domain, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if the domain is not equal to the singleton {val}, Failure otherwise
     */
	def removeValue(value: Int): CPOutcome = {
		val omin = dom.min
		val omax = dom.max
		val minRemoved = dom.min == value
		val maxRemoved = dom.max == value
		val indom = dom.hasValue(value)
		
		if (!indom) return CPOutcome.Suspend
		
		val ok = dom.removeValue(value)
		if (ok == CPOutcome.Failure) return CPOutcome.Failure
		
		
		if (minRemoved || maxRemoved) {
			s.notifyUpdateBoundsL1(onBoundsL1.value,this)
			s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value,this)
			s.notifyL2(onBoundsL2.value)
		}
		if (minRemoved) {
			s.notifyUpdateMinL1(onMinL1.value,this,omin)
			s.notifyUpdateMinIdxL1(onMinIdxL1.value,this,omin)
			s.notifyL2(onMinL2.value)
		}
		if (maxRemoved) {
			s.notifyUpdateMaxL1(onMaxL1.value,this,omax)
			s.notifyUpdateMaxIdxL1(onMaxIdxL1.value,this,omax)
			s.notifyL2(onMaxL2.value)
		}
		if (indom) {
			s.notifRemoveL1(onDomainL1.value,this,value)
			s.notifyRemoveIdxL1(onDomainIdxL1.value,this,value)
			s.notifyL2(onDomainL2.value)
		}
		if (isBound) {
			s.notifyBindL1(onBindL1.value,this)
			s.notifyBindIdxL1(onBindIdxL1.value,this)
			s.notifyL2(onBindL2.value)
		}
		return ok
	}
	
	// ----------------------------------
	
	
	def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
	  dom.delta(oldMin,oldMax,oldSize)
	}
	

	def changed(c: Constraint): Boolean = changed(c.snapshotsVarInt(this))
	
	def minChanged(c: Constraint): Boolean = minChanged(c.snapshotsVarInt(this))
	
	def maxChanged(c: Constraint): Boolean = maxChanged(c.snapshotsVarInt(this))
	
	def boundsChanged(c: Constraint): Boolean = boundsChanged(c.snapshotsVarInt(this))
	
	def oldMin(c: Constraint): Int = oldMin(c.snapshotsVarInt(this))
	
	def oldMax(c: Constraint): Int = oldMax(c.snapshotsVarInt(this))
	
	def oldSize(c: Constraint): Int = oldSize(c.snapshotsVarInt(this))
	
	def deltaSize(c: Constraint): Int = deltaSize(c.snapshotsVarInt(this))
	
	def delta(c: Constraint): Iterator[Int] = {
	  val sn = c.snapshotsVarInt(this)
	  delta(sn.oldMin,sn.oldMax,sn.oldSize)
	}
		
}
