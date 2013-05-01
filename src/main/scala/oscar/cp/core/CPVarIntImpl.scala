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
class CPVarIntImpl(st: Store, minimum: Int, maximum: Int, name: String = "") extends CPVarInt(st,name) {
  
	val dom = new DomainWithHoles(s,minimum,maximum);
	val onMinL2    = new ReversibleQueue[Constraint](s)
	val onMaxL2    = new ReversibleQueue[Constraint](s)
	val onBoundsL2 = new ReversibleQueue[Constraint](s)
	val onBindL2   = new ReversibleQueue[Constraint](s)
	val onDomainL2 = new ReversibleQueue[Constraint](s)

	val onMinL1    = new ReversiblePointer[PropagEventQueue](s,null)
	val onMaxL1    = new ReversiblePointer[PropagEventQueue](s,null)
	val onBoundsL1 = new ReversiblePointer[PropagEventQueue](s,null)
	val onBindL1   = new ReversiblePointer[PropagEventQueue](s,null)
	val onDomainL1 = new ReversiblePointer[PropagEventQueue](s,null)

	val onMinIdxL1    = new ReversiblePointer[PropagEventQueue](s,null)
	val onMaxIdxL1    = new ReversiblePointer[PropagEventQueue](s,null)
	val onBoundsIdxL1 = new ReversiblePointer[PropagEventQueue](s,null)
	val onBindIdxL1   = new ReversiblePointer[PropagEventQueue](s,null)
	val onDomainIdxL1 = new ReversiblePointer[PropagEventQueue](s,null)
	
	/**
     * Builds a variable with domain defined by the range into the store s
     * @param r a scala range
     */
	def this(st: Store, r: Range) = this(st, r.start,if (r.isInclusive) r.end else r.end-1)	
	

	def iterator = {
		val it = dom.iterator
		new Iterator[Int] {
		  def next = it.next
		  def hasNext = it.hasNext
		}
	}
	
	
    /**
	 * 
	 * @return The number of propagation methods of L2 attached to changes of this variables.
	 */
	def constraintDegree() = {
		var tot = 0
		if (onMinL2.hasValue()) tot += onMinL2.getValue().getSize();
		if (onMaxL2.hasValue()) tot += onMaxL2.getValue().getSize();
		if (onBoundsL2.hasValue()) tot += onBoundsL2.getValue().getSize();
		if (onBindL2.hasValue()) tot += onBindL2.getValue().getSize();
		if (onDomainL2.hasValue()) tot += onDomainL2.getValue().getSize();
		
		if (onMinL1.hasValue())    tot += onMinL1.getValue().getSize();
		if (onMaxL1.hasValue())    tot += onMaxL1.getValue().getSize();
		if (onBoundsL1.hasValue()) tot += onBoundsL1.getValue().getSize();
		if (onBindL1.hasValue())   tot += onBindL1.getValue().getSize();
		if (onDomainL1.hasValue()) tot += onDomainL1.getValue().getSize();
		
		if (onMinIdxL1.hasValue())    tot += onMinIdxL1.getValue().getSize();
		if (onMaxIdxL1.hasValue())    tot += onMaxIdxL1.getValue().getSize();
		if (onBoundsIdxL1.hasValue()) tot += onBoundsIdxL1.getValue().getSize();
		if (onBindIdxL1.hasValue())   tot += onBindIdxL1.getValue().getSize();
		if (onDomainIdxL1.hasValue()) tot += onDomainIdxL1.getValue().getSize();	
		tot
	}
	
    /**
     * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
     */
	def isBound = {
        assert(!s.isFailed());
		size == 1;
	}
	
	/**
	 * 
	 * @param v
	 * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
	 */
	def isBoundTo(v: Int) = {
		isBound && value == v;
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
			println("error: no value before "+value+" minimum="+min);
			value
		} else {
			dom.getPrevValue(value-1)
		}
	}

	/**
     * @return  the size of the domain
     */
	override def size = dom.getSize
	
    /**
     * @return true if the domain is empty, false otherwise
     */
	override def isEmpty = dom.isEmpty

    /**
     * @return  the minimum value in the domain
     */
	def min = dom.getMin

	 /**
     * @return  the maximum value in the domain
     */
	def max = dom.getMax

	override def toString(): String =  {
		if (isEmpty) name+" phi";
		else if (isBound) name+" "+value;
		else name+" "+dom.toString();
	}
	
    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound).
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenBind(c: Constraint) {
		onBindL2.setValue(new Queue[Constraint](onBindL2.getValue(),c));
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the maximum or the minimum value of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenBoundsChange(c: Constraint) {
		onBoundsL2.setValue(new Queue[Constraint](onBoundsL2.getValue(),c));
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the maximum of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenMaxChanges(c: Constraint) {
		onMaxL2.setValue(new Queue[Constraint](onMaxL2.getValue(),c));
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the minimum of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenMinChanges(c: Constraint) {
		onMinL2.setValue(new Queue[Constraint](onMinL2.getValue(),c));
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * one of the value is removed from the domain
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenDomainChanges(c: Constraint) {
		onDomainL2.setValue(new Queue[Constraint](onDomainL2.getValue(),c));
	}

    /**
     * Level 1 registration: ask that the valBind(CPVarInt) method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound).
     * @param c
     * @see oscar.cp.core.Constraint#valBind(CPVarInt)
     */
	def callValBindWhenBind(c: Constraint) {
		callValBindWhenBind(c,this,0);
	}
	
	def callValBindWhenBind(c: Constraint, variable: CPVarInt, delta: Int) {
		onBindL1.setValue(new PropagEventQueue(onBindL1.getValue(),c,variable,delta));
	}

    /**
     * Level 1 registration: ask that the updateBounds(CPVarInt) method of the constraint c is called whenever
     * the minimum or maximum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateBounds(CPVarInt)
     */
	def callUpdateBoundsWhenBoundsChange(c: Constraint) {
		callUpdateBoundsWhenBoundsChange(c,this,0);
	}
	
	def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPVarInt,delta: Int) {
		onBoundsL1.setValue(new PropagEventQueue(onBoundsL1.getValue(),c,variable,delta));
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
		onMaxL1.setValue(new PropagEventQueue(onMaxL1.getValue(),c,variable,delta))
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
		onMinL1.setValue(new PropagEventQueue(onMinL1.getValue(),c,variable,delta));
	}

    /**
     * Level 1 registration: ask that the valRemove(CPVarInt, int) method of the constraint c is called for each
     * value deletion from the domain
     * @param c
     * @see oscar.cp.core.Constraint#valRemove(CPVarInt, int)
     */
	def callValRemoveWhenValueIsRemoved(c: Constraint) {
		callValRemoveWhenValueIsRemoved(c,this,0);
	}
	
	def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPVarInt, delta: Int) {
		onDomainL1.setValue(new PropagEventQueue(onDomainL1.getValue(),c,variable,delta));
	}

    /**
     * Level 1 registration: ask that the valRemoveIdx(CPVarInt, int, int) method of the constraint c is called for each
     * value deletion from the domain
     * @param c
     * @param idx, an index that will be given as parameter to valRemoveIdx(CPVarInt, int, int)
     * @see Constraint#valRemoveIdx(CPVarInt, int, int)
     */
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) {
		callValRemoveIdxWhenValueIsRemoved(c,this,idx,0);
	}
	
	def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onDomainIdxL1.setValue(new PropagEventQueue(onDomainIdxL1.getValue(),c,variable,idx,delta));
	}

    /**
     * Level 1 registration: ask that the updateMinIdx(CPVarInt, int, int) method of the constraint c is called whenever
     * the minimum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateMinIdx(CPVarInt, int, int)
     * @see Constraint#updateMinIdx(CPVarInt, int, int)
     */
	def callUpdateMinIdxWhenMinChanges(c: Constraint , idx: Int) {
		callUpdateMinIdxWhenMinChanges(c,this,idx,0);
	}


	def callUpdateMinIdxWhenMinChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onMinIdxL1.setValue(new PropagEventQueue(onMinIdxL1.getValue(),c,variable,idx,delta));
	}

    /**
     * Level 1 registration: ask that the updateMaxIdx(CPVarInt, int, int) method of the constraint c is called whenever
     * the maximum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateMaxIdx(CPVarInt, int, int)
     * @see Constraint#updateMaxIdx(CPVarInt, int, int)
     */
	def callUpdateMaxIdxWhenMaxChanges(c: Constraint , idx: Int) {
		callUpdateMaxIdxWhenMaxChanges(c,this,idx,0);
	}

	def callUpdateMaxIdxWhenMaxChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onMaxIdxL1.setValue(new PropagEventQueue(onMaxIdxL1.getValue(),c,variable, idx,delta));	
	}

    /**
     * Level 1 registration: ask that the updateBoundsIdx(CPVarInt, int) method of the constraint c is called whenever
     * the minimum or maximum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateBoundsIdx(CPVarInt, int)
     * @see Constraint#updateBoundsIdx(CPVarInt, int)
     */
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint , idx: Int) {
		callUpdateBoundsIdxWhenBoundsChange(c,this,idx,0);
	}
	
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onBoundsIdxL1.setValue(new PropagEventQueue(onBoundsIdxL1.getValue(),c,variable,idx,delta));
	}

    /**
     * Level 1 registration: ask that the valBindIdx(CPVarInt, int) method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound).
     * @param c
     * @param idx, an index that will be given as parameter to valBindIdx(CPVarInt, int)
     * @see Constraint#valBindIdx(CPVarInt, int)
     */
	def callValBindIdxWhenBind(c: Constraint , idx: Int) {
		callValBindIdxWhenBind(c,this,idx,0);
	}
	
	def callValBindIdxWhenBind(c: Constraint, variable: CPVarInt, idx: Int, delta: Int) {
		onBindIdxL1.setValue(new PropagEventQueue(onBindIdxL1.getValue(),c,variable,idx,delta));
	}	
	
	
    /**
     * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if val was in the domain, Failure otherwise
     */
	def assign(value: Int): CPOutcome = {
		if (!hasValue(value)) return CPOutcome.Failure;
		else if(isBound) return CPOutcome.Suspend;
		else{ // more than one value
			val assignToMax = max == value
			val assignToMin = min == value
			// -------- AC3 notifications ------------
			if (!assignToMax)
                s.notifyL2(onMaxL2.getValue());
			if (!assignToMin)
                s.notifyL2(onMinL2.getValue());
			s.notifyL2(onBoundsL2.getValue());
			s.notifyL2(onDomainL2.getValue());
			s.notifyL2(onBindL2.getValue());
			// --------- AC5 notifications ------------
			s.notifyBindL1(onBindL1.getValue(),this);
			s.notifyBindIdxL1(onBindIdxL1.getValue(),this);
			s.notifyUpdateBoundsL1(onBoundsL1.getValue(),this);
			s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.getValue(),this);
			if (value == dom.getMin()) {
				s.notifyUpdateMaxL1(onMaxL1.getValue(),this,dom.getMax());
				s.notifyUpdateMaxIdxL1(onMaxIdxL1.getValue(),this,dom.getMax());
			} 
			else if (value == dom.getMax()) {
				s.notifyUpdateMinL1(onMinL1.getValue(),this,dom.getMin());
				s.notifyUpdateMinIdxL1(onMinIdxL1.getValue(),this,dom.getMin());
			} else {
				s.notifyUpdateMaxL1(onMaxL1.getValue(), this,dom.getMax());
				s.notifyUpdateMaxIdxL1(onMaxIdxL1.getValue(),this, dom.getMax());
				s.notifyUpdateMinL1(onMinL1.getValue(), this,dom.getMin());
				s.notifyUpdateMinIdxL1(onMinIdxL1.getValue(),this, dom.getMin());
			}
			// must notify AC5 event before the actual removal
			if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
			    var i = dom.getMin()
			    while (i <= dom.getMax()) {
					if (i!= value && dom.hasValue(i)) {
						if (onDomainL1.hasValue()) {
							s.notifRemoveL1(onDomainL1.getValue(),this,i);
						}
						if (onDomainIdxL1.hasValue()) {
							s.notifyRemoveIdxL1(onDomainIdxL1.getValue(),this,i);
						}
					}
					i += 1
				}
			}
			// finally do the assignment
			return dom.assign(value);
		}
	}

    /**
     * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
     */
	def updateMin(value: Int): CPOutcome = {
		
		if (value > dom.getMax()) return CPOutcome.Failure;
		if (value <= dom.getMin()) return CPOutcome.Suspend;
		
		val omin = dom.getMin();
		
		//must notif AC5 event with the removed values before the actual removal
		if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
			var i = dom.getMin()
			while (i < value) {
				if(dom.hasValue(i)) {
					if (onDomainL1.hasValue())
						s.notifRemoveL1(onDomainL1.getValue(),this,i);
					if (onDomainIdxL1.hasValue())
						s.notifyRemoveIdxL1(onDomainIdxL1.getValue(),this,i);
				}
				i += 1
			}
		}
		
		val ok = dom.updateMin(value);
		assert(ok != CPOutcome.Failure);
		
		if (dom.getSize()==1) {
			assert(isBound);
			s.notifyBindL1(onBindL1.getValue(),this);
			s.notifyBindIdxL1(onBindIdxL1.getValue(),this);
			s.notifyL2(onBindL2.getValue());
		}
		s.notifyUpdateMinL1(onMinL1.getValue(),this,omin);
		s.notifyUpdateMinIdxL1(onMinIdxL1.getValue(),this,omin);
		s.notifyL2(onMinL2.getValue());
		s.notifyUpdateBoundsL1(onBoundsL1.getValue(),this);
		s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.getValue(),this);
		s.notifyL2(onBoundsL2.getValue());
		s.notifyL2(onDomainL2.getValue());
		return CPOutcome.Suspend;
	}
	
	
     /**
     * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
     */
	def updateMax(value: Int): CPOutcome = {	
		if (value < dom.getMin()) return CPOutcome.Failure;
		if (value >= dom.getMax()) return CPOutcome.Suspend;
		
		val omax = dom.getMax();
		
		//must notifyAC3 the removed value before the actual removal
		if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
		   var i = omax
		   while (i > value) {
				if(dom.hasValue(i)){
					if (onDomainL1.hasValue())
						s.notifRemoveL1(onDomainL1.getValue(), this, i);
					if (onDomainIdxL1.hasValue())
						s.notifyRemoveIdxL1(onDomainIdxL1.getValue(), this, i);
				}
				i -= 1
			}
		}
		
		val ok = dom.updateMax(value);
		assert(ok != CPOutcome.Failure)
		
		if(dom.getSize()==1){
			assert(isBound);
			s.notifyBindL1(onBindL1.getValue(),this);
			s.notifyBindIdxL1(onBindIdxL1.getValue(),this);
			s.notifyL2(onBindL2.getValue());
		}
		s.notifyUpdateMaxL1(onMaxL1.getValue(), this,omax);
		s.notifyUpdateMaxIdxL1(onMaxIdxL1.getValue(),this,omax);
		s.notifyL2(onMaxL2.getValue());
		s.notifyUpdateBoundsL1(onBoundsL1.getValue(),this);
		s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.getValue(),this);
		s.notifyL2(onBoundsL2.getValue());
		s.notifyL2(onDomainL2.getValue());
		return CPOutcome.Suspend;
	}
	
	
    /**
     * Remove val from the domain, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if the domain is not equal to the singleton {val}, Failure otherwise
     */
	def removeValue(value: Int): CPOutcome = {
		val omin = dom.getMin();
		val omax = dom.getMax();
		val minRemoved = dom.getMin() == value;
		val maxRemoved = dom.getMax() == value;
		val indom = dom.hasValue(value);
		
		if (!indom) return CPOutcome.Suspend;
		
		val ok = dom.removeValue(value);
		if (ok == CPOutcome.Failure) return CPOutcome.Failure;
		
		
		if (minRemoved || maxRemoved) {
			s.notifyUpdateBoundsL1(onBoundsL1.getValue(),this);
			s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.getValue(),this);
			s.notifyL2(onBoundsL2.getValue());
		}
		if (minRemoved) {
			s.notifyUpdateMinL1(onMinL1.getValue(),this,omin);
			s.notifyUpdateMinIdxL1(onMinIdxL1.getValue(),this,omin);
			s.notifyL2(onMinL2.getValue());
		}
		if (maxRemoved) {
			s.notifyUpdateMaxL1(onMaxL1.getValue(),this,omax);
			s.notifyUpdateMaxIdxL1(onMaxIdxL1.getValue(),this,omax);
			s.notifyL2(onMaxL2.getValue());
		}
		if (indom) {
			s.notifRemoveL1(onDomainL1.getValue(),this,value);
			s.notifyRemoveIdxL1(onDomainIdxL1.getValue(),this,value);
			s.notifyL2(onDomainL2.getValue());
		}
		if (isBound) {
			s.notifyBindL1(onBindL1.getValue(),this);
			s.notifyBindIdxL1(onBindIdxL1.getValue(),this);
			s.notifyL2(onBindL2.getValue());
		}
		return ok;
	}
		
}
