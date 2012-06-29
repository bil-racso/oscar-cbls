/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
 ******************************************************************************/
package oscar.cp.core;

import java.util.Collections;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import oscar.cp.constraints.Abs;
import oscar.cp.constraints.Diff;
import oscar.cp.constraints.DiffReif;
import oscar.cp.constraints.DiffReifVar;
import oscar.cp.constraints.Eq;
import oscar.cp.constraints.EqReif;
import oscar.cp.constraints.EqReifVar;
import oscar.cp.constraints.Gr;
import oscar.cp.constraints.GrEq;
import oscar.cp.constraints.GrEqCteReif;
import oscar.cp.constraints.GrEqVarReif;
import oscar.cp.constraints.Le;
import oscar.cp.constraints.LeEq;
import oscar.cp.constraints.LeEqCteReif;
import oscar.cp.constraints.Minus;
import oscar.cp.constraints.MulCte;
import oscar.cp.constraints.MulVar;
import oscar.cp.constraints.Opposite;
import oscar.cp.constraints.Sum;
import oscar.cp.util.ArrayUtils;
import oscar.cp.util.NumberUtils;
import oscar.reversible.ReversiblePointer;
import oscar.reversible.ReversibleQueue;

import scala.collection.immutable.Range;

/**
 * Finite Domain Integer Variables <br>
 * All the methods assume as prerequisite that the store of the variable creation is not failed (!getStore().isFailed()). <br>
 * In the following specs, this variable is called x for notation purposes.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class CPVarInt implements Iterator<Integer>, Iterable<Integer>{
	
	private Store s;
	
	private Domain dom;
	
	private String name = "";
	
	private int currIteratorVal;
	
	private ReversibleQueue<Constraint> onMinL2;
	private ReversibleQueue<Constraint> onMaxL2;
	private ReversibleQueue<Constraint> onBoundsL2;
	private ReversibleQueue<Constraint> onBindL2;
	private ReversibleQueue<Constraint> onDomainL2;
	
	
	private ReversiblePointer<PropagEventQueue> onMinL1;
	private ReversiblePointer<PropagEventQueue> onMaxL1;
	private ReversiblePointer<PropagEventQueue> onBoundsL1;
	private ReversiblePointer<PropagEventQueue> onBindL1;
	private ReversiblePointer<PropagEventQueue> onDomainL1;
	
	private ReversiblePointer<PropagEventQueue> onMinIdxL1;
	private ReversiblePointer<PropagEventQueue> onMaxIdxL1;
	private ReversiblePointer<PropagEventQueue> onBoundsIdxL1;
	private ReversiblePointer<PropagEventQueue> onBindIdxL1;
	private ReversiblePointer<PropagEventQueue> onDomainIdxL1;

    protected CPVarInt(Store s) {
        this.s = s;
    }
	
    /**
     * Builds a variable with domain defined by the range into the store s
     * @param r a scala range
     */
	public CPVarInt(Store s, Range r) {
		this(s,r.start(),r.isInclusive() ? r.end() : r.end()-1);
	}


    /**
     * Builds a variable with domain {min,...,max} into the store s
     * @param s
     * @param min
     * @param max >= min
     */
	public CPVarInt(Store s,int min, int max) {
        this.s = s;
		dom = new DomainWithHoles(s,min,max);
		onMinL2    = new ReversibleQueue<Constraint>(s);
		onMaxL2    = new ReversibleQueue<Constraint>(s);
		onBoundsL2 = new ReversibleQueue<Constraint>(s);
		onBindL2   = new ReversibleQueue<Constraint>(s);
		onDomainL2 = new ReversibleQueue<Constraint>(s);
		
		onMinL1    = new ReversiblePointer<PropagEventQueue>(s,null);
		onMaxL1    = new ReversiblePointer<PropagEventQueue>(s,null);
		onBoundsL1 = new ReversiblePointer<PropagEventQueue>(s,null);
		onBindL1   = new ReversiblePointer<PropagEventQueue>(s,null);
		onDomainL1 = new ReversiblePointer<PropagEventQueue>(s,null);
		
		onMinIdxL1    = new ReversiblePointer<PropagEventQueue>(s,null);
		onMaxIdxL1    = new ReversiblePointer<PropagEventQueue>(s,null);
		onBoundsIdxL1 = new ReversiblePointer<PropagEventQueue>(s,null);
		onBindIdxL1   = new ReversiblePointer<PropagEventQueue>(s,null);
		onDomainIdxL1 = new ReversiblePointer<PropagEventQueue>(s,null);
	}
	
	/**
	 * 
	 * @return The number of propagation methods of L2 attached to changes of this variables.
	 */
	public int getConstraintDegree() {
		int tot = 0;
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
		return tot;
	}


    /**
     * @see CPVarInt(Store,int,int)
     */
	public CPVarInt(Store s,int min, int max,String name) {
		this(s,min,max);
		this.name = name;
	}

    /**
     * Builds a variable with a domain composed of the set of values in vals
     * @param s
     * @param vals
     */
	public CPVarInt(Store s,int ...vals){
		this(s,ArrayUtils.min(vals),ArrayUtils.max(vals));
		if (vals.length == 0) {
			throw new RuntimeException("VarInt:empty set of values");
		}
		Set<Integer> values = new TreeSet<Integer>();
		for (Integer v: vals) {
			values.add(v);
		}
		for(int i = ArrayUtils.min(vals); i <= ArrayUtils.max(vals); i++) {
			if (!values.contains(i)) {
				removeValue(i);
			}
		}	
	}

    /**
     * @see  CPVarInt(Store,int [])
     */
	public CPVarInt(Store s,int [] vals, String name) {
		this(s,vals);
		this.name = name;
	}

    /**
     * Builds a variable with a domain composed of the set of values in vals
     * @param s
     * @param vals
     */
	public CPVarInt(Store s,Set<Integer> vals) {
		this(s,Collections.min(vals),Collections.max(vals));
		for (int i = Collections.min(vals); i <= Collections.max(vals); i++) {
			if (!vals.contains(i)) {
				removeValue(i);
			}
		}	
	}

    /**
     * @see  CPVarInt(Store,Set<Integer>)
     */
	public CPVarInt(Store s,Set<Integer> values, String name) {
		this(s,values);
		this.name = name;
	}
	
	public String getName() {
		return name;
	}

    /**
     * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
     */
	public boolean isBound() {
        assert(!s.isFailed());
		return getSize() == 1;
	}
	
	/**
	 * 
	 * @param v
	 * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
	 */
	public boolean isBoundTo(int v) {
		return isBound() && getValue() == v;
	}

    /**
     *
     * @return  the store in which this variable was created
     */
	public Store getStore() {
		return s;
	}

    /**
     * Test if a value is in the domain
     * @param val
     * @return  true if the domain contains the value val, false otherwise
     */
	public boolean hasValue(int val) {
		return dom.hasValue(val);
	}

    /**
     * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if val was in the domain, Failure otherwise
     */
	public CPOutcome assign(int val) {
		if (!hasValue(val)) return CPOutcome.Failure;
		else if(isBound()) return CPOutcome.Suspend;
		else{ // more than one value
			boolean assignToMax = getMax()==val;
			boolean assignToMin = getMin()==val;
			// -------- AC3 notifications ------------
			if(!assignToMax)
                s.notifyL2(onMaxL2.getValue());
			if(!assignToMin)
                s.notifyL2(onMinL2.getValue());
			s.notifyL2(onBoundsL2.getValue());
			s.notifyL2(onDomainL2.getValue());
			s.notifyL2(onBindL2.getValue());
			// --------- AC5 notifications ------------
			s.notifyBindL1(onBindL1.getValue(),this);
			s.notifyBindIdxL1(onBindIdxL1.getValue(),this);
			s.notifyUpdateBoundsL1(onBoundsL1.getValue(),this);
			s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.getValue(),this);
			if (val == dom.getMin()) {
				s.notifyUpdateMaxL1(onMaxL1.getValue(),this,dom.getMax());
				s.notifyUpdateMaxIdxL1(onMaxIdxL1.getValue(),this,dom.getMax());
			} 
			else if (val == dom.getMax()) {
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
				for (int i = dom.getMin(); i <= dom.getMax(); i++) {
					if (i!= val && dom.hasValue(i)) {
						if (onDomainL1.hasValue()) {
							s.notifRemoveL1(onDomainL1.getValue(),this,i);
						}
						if (onDomainIdxL1.hasValue()) {
							s.notifyRemoveIdxL1(onDomainIdxL1.getValue(),this,i);
						}
					}
				}
			}
			// finally do the assignment
			return dom.assign(val);
		}
	}

    /**
     * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
     * @param val
     * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
     */
	public CPOutcome updateMin(int val) {
		
		if (val > dom.getMax()) return CPOutcome.Failure;
		if (val <= dom.getMin()) return CPOutcome.Suspend;
		
		int omin = dom.getMin();
		
		//must notifyAC3 the removed value before the actual removal
		if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
			for (int i = dom.getMin(); i < val; i++) {
				if(dom.hasValue(i)) {
					if (onDomainL1.hasValue())
						s.notifRemoveL1(onDomainL1.getValue(),this,i);
					if (onDomainIdxL1.hasValue())
						s.notifyRemoveIdxL1(onDomainIdxL1.getValue(),this,i);
				}
			}
		}
		
		CPOutcome ok = dom.updateMin(val);
		assert(ok != CPOutcome.Failure);
		
		if (dom.getSize()==1) {
			assert(isBound());
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
	public CPOutcome updateMax(int val) {	
		if (val < dom.getMin()) return CPOutcome.Failure;
		if (val >= dom.getMax()) return CPOutcome.Suspend;
		
		int omax = dom.getMax();
		
		//must notifyAC3 the removed value before the actual removal
		if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
			for (int i = omax; i > val; i--) {
				if(dom.hasValue(i)){
					if (onDomainL1.hasValue())
						s.notifRemoveL1(onDomainL1.getValue(), this, i);
					if (onDomainIdxL1.hasValue())
						s.notifyRemoveIdxL1(onDomainIdxL1.getValue(), this, i);
				}
			}
		}
		
		CPOutcome ok = dom.updateMax(val);
		assert ok != CPOutcome.Failure;
		
		if(dom.getSize()==1){
			assert(isBound());
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
	public CPOutcome removeValue(int val) {
		int omin = dom.getMin();
		int omax = dom.getMax();
		boolean min = dom.getMin() == val;
		boolean max = dom.getMax() == val;
		boolean indom = dom.hasValue(val);
		
		if (!indom) return CPOutcome.Suspend;
		
		CPOutcome ok = dom.removeValue(val);
		if (ok == CPOutcome.Failure) return CPOutcome.Failure;
		
		
		if (min || max) {
			s.notifyUpdateBoundsL1(onBoundsL1.getValue(),this);
			s.notifyUpdateBoundsIdxL1(onBoundsIdxL1.getValue(),this);
			s.notifyL2(onBoundsL2.getValue());
		}
		if (min) {
			s.notifyUpdateMinL1(onMinL1.getValue(),this,omin);
			s.notifyUpdateMinIdxL1(onMinIdxL1.getValue(),this,omin);
			s.notifyL2(onMinL2.getValue());
		}
		if (max) {
			s.notifyUpdateMaxL1(onMaxL1.getValue(),this,omax);
			s.notifyUpdateMaxIdxL1(onMaxIdxL1.getValue(),this,omax);
			s.notifyL2(onMaxL2.getValue());
		}
		if (indom) {
			s.notifRemoveL1(onDomainL1.getValue(),this,val);
			s.notifyRemoveIdxL1(onDomainIdxL1.getValue(),this,val);
			s.notifyL2(onDomainL2.getValue());
		}
		if (isBound()) {
			s.notifyBindL1(onBindL1.getValue(),this);
			s.notifyBindIdxL1(onBindIdxL1.getValue(),this);
			s.notifyL2(onBindL2.getValue());
		}
		return ok;
	}

    /**
     * @return  the size of the domain
     */
	public int getSize() {
		return dom.getSize();
	}

    /**
     * @return true if the domain is empty, false otherwise
     */
	public boolean isEmpty() {
		return dom.isEmpty();
	}

    /**
     * @return  the minimum value in the domain
     */
	public int getMin() {
		return dom.getMin();
	}

    /**
     * @return  the maximum value in the domain
     */
	public int getMax() {
		return dom.getMax();
	}

    /**
     * @return the unique value in the domain
     * @throws RuntimeException  is the variable is not bound
     */
	public int getValue() throws RuntimeException {
		if (!isBound()) {
			throw new RuntimeException("the variable is not bound");
		} else {
			return getMin();
		}
	}

    /**
     * @param val
     * @return the smallest value > val in the domain
     * @throws RuntimeException is there is not value > val in the domain
     */
	public int getValueAfter(int val) throws RuntimeException {
		if (getMax() < val+1) {
			throw new RuntimeException("no value after "+val+" maximum="+getMax());
		} else {
			return dom.getNextValue(val+1);
		}
	}

    /**
     * @param val
     * @return the largest value < val in the domain
     * @throws RuntimeException is there is not value < val in the domain
     */
	public int getValueBefore(int val) throws RuntimeException {
		if (getMin() > val-1) {
			throw new RuntimeException("no value before "+val+" minimum="+getMin());
		} else {
			return dom.getPrevValue(val-1);
		}
	}
	
	
	@Override
	public String toString() {
		if (isEmpty()) return getName()+" phi";
		else if (isBound()) return getName()+" "+getValue();
		else return getName()+" "+dom.toString();//+" card:"+getSize()+" min:"+getMin()+" max:"+getMax();
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound()).
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	public void callPropagateWhenBind(Constraint c) {
		onBindL2.setValue(new Queue<Constraint>(onBindL2.getValue(),c));
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the maximum or the minimum value of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	public void callPropagateWhenBoundsChange(Constraint c) {
		onBoundsL2.setValue(new Queue<Constraint>(onBoundsL2.getValue(),c));
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the maximum of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	public void callPropagateWhenMaxChanges(Constraint c) {
		onMaxL2.setValue(new Queue<Constraint>(onMaxL2.getValue(),c));
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * the minimum of the domain changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	public void callPropagateWhenMinChanges(Constraint c) {
		onMinL2.setValue(new Queue<Constraint>(onMinL2.getValue(),c));
	}

    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
     * one of the value is removed from the domain
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	public void callPropagateWhenDomainChanges(Constraint c) {
		onDomainL2.setValue(new Queue<Constraint>(onDomainL2.getValue(),c));
	}

    /**
     * Level 1 registration: ask that the valBind(CPVarInt) method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound()).
     * @param c
     * @see oscar.cp.core.Constraint#valBind(CPVarInt)
     */
	public void callValBindWhenBind(Constraint c) {
		callValBindWhenBind(c,this,0);
	}
	
	protected void callValBindWhenBind(Constraint c,CPVarInt var,int delta) {
		onBindL1.setValue(new PropagEventQueue(onBindL1.getValue(),c,var,delta));
	}

    /**
     * Level 1 registration: ask that the updateBounds(CPVarInt) method of the constraint c is called whenever
     * the minimum or maximum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateBounds(CPVarInt)
     */
	public void callUpdateBoundsWhenBoundsChange(Constraint c) {
		callUpdateBoundsWhenBoundsChange(c,this,0);
	}
	
	protected void callUpdateBoundsWhenBoundsChange(Constraint c, CPVarInt var, int delta) {
		onBoundsL1.setValue(new PropagEventQueue(onBoundsL1.getValue(),c,var,delta));
	}

    /**
     * Level 1 registration: ask that the updateMax(CPVarInt, int) method of the constraint c is called whenever
     * the maximum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateMax(CPVarInt, int)
     */
	public void callUpdateMaxWhenMaxChanges(Constraint c) {
		callUpdateMaxWhenMaxChanges(c,this,0);
	}
	
	protected void callUpdateMaxWhenMaxChanges(Constraint c, CPVarInt var, int delta) {
		onMaxL1.setValue(new PropagEventQueue(onMaxL1.getValue(),c,var,delta));
	}

     /**
     * Level 1 registration: ask that the updateMin(CPVarInt, int) method of the constraint c is called whenever
     * the minimum value of the domain changes.
     * @param c
     * @see oscar.cp.core.Constraint#updateMin(CPVarInt, int)
     */
	public void callUpdateMinWhenMinChanges(Constraint c) {
		callUpdateMinWhenMinChanges(c,this,0);
	}
	
	protected void callUpdateMinWhenMinChanges(Constraint c, CPVarInt var, int delta) {
		onMinL1.setValue(new PropagEventQueue(onMinL1.getValue(),c,var,delta));
	}

    /**
     * Level 1 registration: ask that the valRemove(CPVarInt, int) method of the constraint c is called for each
     * value deletion from the domain
     * @param c
     * @see oscar.cp.core.Constraint#valRemove(CPVarInt, int)
     */
	public void callValRemoveWhenValueIsRemoved(Constraint c) {
		callValRemoveWhenValueIsRemoved(c,this,0);
	}
	
	protected void callValRemoveWhenValueIsRemoved(Constraint c, CPVarInt var, int delta) {
		onDomainL1.setValue(new PropagEventQueue(onDomainL1.getValue(),c,var,delta));
	}

    /**
     * Level 1 registration: ask that the valRemoveIdx(CPVarInt, int, int) method of the constraint c is called for each
     * value deletion from the domain
     * @param c
     * @param idx, an index that will be given as parameter to valRemoveIdx(CPVarInt, int, int)
     * @see Constraint#valRemoveIdx(CPVarInt, int, int)
     */
	public void callValRemoveIdxWhenValueIsRemoved(Constraint c, int idx) {
		callValRemoveIdxWhenValueIsRemoved(c,this,idx,0);
	}
	
	protected void callValRemoveIdxWhenValueIsRemoved(Constraint c, CPVarInt var, int idx, int delta) {
		onDomainIdxL1.setValue(new PropagEventQueue(onDomainIdxL1.getValue(),c,var,idx,delta));
	}

    /**
     * Level 1 registration: ask that the updateMinIdx(CPVarInt, int, int) method of the constraint c is called whenever
     * the minimum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateMinIdx(CPVarInt, int, int)
     * @see Constraint#updateMinIdx(CPVarInt, int, int)
     */
	public void callUpdateMinIdxWhenMinChanges(Constraint c, int idx) {
		callUpdateMinIdxWhenMinChanges(c,this,idx,0);
	}


	protected void callUpdateMinIdxWhenMinChanges(Constraint c, CPVarInt var, int idx, int delta) {
		onMinIdxL1.setValue(new PropagEventQueue(onMinIdxL1.getValue(),c,var,idx,delta));
	}

    /**
     * Level 1 registration: ask that the updateMaxIdx(CPVarInt, int, int) method of the constraint c is called whenever
     * the maximum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateMaxIdx(CPVarInt, int, int)
     * @see Constraint#updateMaxIdx(CPVarInt, int, int)
     */
	public void callUpdateMaxIdxWhenMaxChanges(Constraint c, int idx) {
		callUpdateMaxIdxWhenMaxChanges(c,this,idx,0);
	}

	protected void callUpdateMaxIdxWhenMaxChanges(Constraint c, CPVarInt var, int idx, int delta) {
		onMaxIdxL1.setValue(new PropagEventQueue(onMaxIdxL1.getValue(),c,var, idx,delta));	
	}

    /**
     * Level 1 registration: ask that the updateBoundsIdx(CPVarInt, int) method of the constraint c is called whenever
     * the minimum or maximum value of the domain changes
     * @param c
     * @param idx, an index that will be given as parameter to updateBoundsIdx(CPVarInt, int)
     * @see Constraint#updateBoundsIdx(CPVarInt, int)
     */
	public void callUpdateBoundsIdxWhenBoundsChange(Constraint c, int idx) {
		callUpdateBoundsIdxWhenBoundsChange(c,this,idx,0);
	}
	
	protected void callUpdateBoundsIdxWhenBoundsChange(Constraint c, CPVarInt var, int idx, int delta) {
		onBoundsIdxL1.setValue(new PropagEventQueue(onBoundsIdxL1.getValue(),c,var,idx,delta));
	}

    /**
     * Level 1 registration: ask that the valBindIdx(CPVarInt, int) method of the constraint c is called whenever
     * the domain of the variable is a singleton (i.e. isBound()).
     * @param c
     * @param idx, an index that will be given as parameter to valBindIdx(CPVarInt, int)
     * @see Constraint#valBindIdx(CPVarInt, int)
     */
	public void callValBindIdxWhenBind(Constraint c, int idx) {
		callValBindIdxWhenBind(c,this,idx,0);
	}
	
	protected void callValBindIdxWhenBind(Constraint c, CPVarInt var, int idx, int delta) {
		onBindIdxL1.setValue(new PropagEventQueue(onBindIdxL1.getValue(),c,var,idx,delta));
	}
	
	/**
	 * Return a random value in the domain of the variable (uniform distribution)
	 * @return
	 */
	public int getRandomValue() {
		int ind = s.getRandom().nextInt(getSize());
		int cpt = 0;
		for (int v: this) {
			if (cpt == ind) {
				return v;
			}
			cpt++;
		}
		return 0;
	}
	
	/**
	 * weight[i] is the weight for value i
	 */
	public int getRandomValue(int [] weight) {
		int totWeight = 0;
		for (int v: this) {
			totWeight += weight[v];
		}
		int ind = s.getRandom().nextInt(totWeight);
		int currWeight = 0;
		for (int v: this) {
			int w = weight[v];
			if (ind < currWeight+w && ind >= currWeight) {
				return v;
			}
			currWeight += w;
		}
		return 0;
	}

    /**
     * @return  an iterator on the values of the domain in increasing order. This iterator should not be used to delete value and the domain should not be modified during the iteration process.
     */
	public Iterator<Integer> iterator() {
		currIteratorVal = getMin();
		return this;
	}

    /**
     * @return true if there is still values to iterate on, false otherwise
     */
	public boolean hasNext() {
		return currIteratorVal <= getMax();
	}

    /**
     * @return the next value of the iterator
     */
	public Integer next() {
//		if (!hasNext()) { //for the jython wrapper
//			throw Py.StopIteration("");
//		}
		int res = currIteratorVal;
		for (int i = res+1; i <= getMax()+1; i++) {
			currIteratorVal = i;
			if (hasValue(i)) {
				break;
			}
		}
		return res;
	}

    /**
     * @return a variable in the same store representing: - x
     */
	public CPVarInt opposite() {
		CPVarInt y = new CPVarInt(s,-getMax(),-getMin());
		s.post(new Opposite(this,y));
		return y;
	}

    /**
     * @param d
     * @return  a variable in the same store representing: x - d
     */
	public CPVarInt minus(int d) {
		return new CPVarIntView(this,-d);
	}

    /**
     * @param y a variable in the same store as x
     * @return a variable in the same store representing: x - y
     */
	public CPVarInt minus(CPVarInt y) {
		CPVarInt c = new CPVarInt(s,getMin() - y.getMax(),getMax() - y.getMin());
		s.post(new Minus(this,y,c));
		return c;
	}

    /**
     * @param d
     * @return  a variable in the same store representing: x + d
     */
	public CPVarInt plus(int d) {
		if (d == 0) {
			return this;
		}
		return new CPVarIntView(this,d);
	}

    /**
     * @param y
     * @return a variable in the same store representing: x + y
     */
	public CPVarInt plus(CPVarInt y) {
		CPVarInt c;
		if (this.getSize() * y.getSize() <= 500) {
			Set<Integer> vals = new java.util.HashSet<Integer>();
			for (int v1: this) {
				for (int v2: y) {
					vals.add(v1+v2);
				}
			}
			c = new CPVarInt(getStore(),vals);
		}
		else {
			c = new CPVarInt(getStore(),getMin() + y.getMin(),getMax() + y.getMax());
		}
		CPOutcome ok = s.post(new Sum(new CPVarInt[]{this,y},c));
        assert (ok != CPOutcome.Failure);
		return c;
	}

    /**
     * @param c
     * @return a variable in the same store representing: x * c
     */
	public CPVarInt mul(int c) {
		if (c == 1) {
			return this;
		}
		int a = c > 0 ? getMin()*c : getMax()*c;
		int b = c > 0 ? getMax()*c : getMin()*c;
		CPVarInt y = new CPVarInt(getStore(),a,b);
		CPOutcome ok = s.post(new MulCte(this,c,y));
		assert(ok != CPOutcome.Failure);
		return y;

	}

    /**
     * @param y a variable in the same store as x
     * @return a variable in the same store representing: x * y
     */
	public CPVarInt mul(CPVarInt y) {
		int a = getMin();
		int b = getMax();
		int c = y.getMin();
		int d = y.getMax();
		int [] t = new int[]{NumberUtils.safeMul(a,c),NumberUtils.safeMul(a,d),NumberUtils.safeMul(b,c),NumberUtils.safeMul(b,d)}; 
		CPVarInt z = new CPVarInt(getStore(),ArrayUtils.min(t), ArrayUtils.max(t));
		CPOutcome ok = s.post(new MulVar(this,y,z));
        assert(ok != CPOutcome.Failure);
		return z;
	}

    /**
     * @return a variable in the same store representing: |x|
     */
	public CPVarInt abs() {
		CPVarInt c = new CPVarInt(getStore(),0,Math.max(Math.abs(getMin()), Math.abs(getMax())));
		CPOutcome ok = s.post(new Abs(this,c));
        assert(ok != CPOutcome.Failure);
		return c;
	}

    /**
     * Reified constraint
     * @param v
     * @return  a boolean variable b in the same store linked to x by the relation x == v <=> b == true
     */
	public CPVarBool isEq(int v) {
		CPVarBool b = new CPVarBool(getStore());
		CPOutcome ok = s.post(new EqReif(this,v,b));
        assert(ok != CPOutcome.Failure);
		return b;
	}
	
    /**
     * Reified constraint
     * @param y a variable
     * @return a boolean variable b in the same store linked to x by the relation x == y <=> b == true
     */
	public CPVarBool isEq(CPVarInt y) {
		CPVarBool b = new CPVarBool(getStore());
		CPOutcome ok = s.post(new EqReifVar(this,y,b));
        assert(ok != CPOutcome.Failure);
		return b;
	}	

    /**
     * Reified constraint
     * @param v
     * @return  a boolean variable b in the same store linked to x by the relation x != v <=> b == true
     */
	public CPVarBool isDiff(int v) {
		CPVarBool b = new CPVarBool(getStore());
		CPOutcome ok = s.post(new DiffReif(this,v,b));
        assert ok != CPOutcome.Failure;
		return b;
	}
	
    /**
     * Reified constraint
     * @param y
     * @return  a boolean variable b in the same store linked to x by the relation x != y <=> b == true
     */
	public CPVarBool isDiff(CPVarInt y) {
		CPVarBool b = new CPVarBool(getStore());
		CPOutcome ok = s.post(new DiffReifVar(this,y,b));
        assert ok != CPOutcome.Failure;
		return b;
	}

    /**
     * Reified constraint
     * @param v
     * @return  a boolean variable b in the same store linked to x by the relation x >= v <=> b == true
     */
	public CPVarBool isGrEq(int v) {
		CPVarBool b = new CPVarBool(getStore());
		CPOutcome ok = s.post(new GrEqCteReif(this,v,b));
        assert (ok != CPOutcome.Failure);
		return b;
	}	
	
    /**
     * Reified constraint
     * @param v
     * @return  a boolean variable b in the same store linked to x by the relation x <= v <=> b == true
     */
	public CPVarBool isLeEq(int v) {
		CPVarBool b = new CPVarBool(getStore());
		CPOutcome ok = s.post(new LeEqCteReif(this,v,b));
        assert (ok != CPOutcome.Failure);
		return b;
	}

    /**
     * Reified constraint
     * @param y a variable in the same store as x
     * @return  a boolean variable b in the same store linked to x by the relation x >= y <=> b == true
     */
	public CPVarBool isGrEq(CPVarInt y) {
		CPVarBool b = new CPVarBool(getStore());
		CPOutcome ok = s.post(new GrEqVarReif(this,y,b));
        assert (ok != CPOutcome.Failure);
		return b;
	}

    /**
     *
     * @param cp
     * @param n
     * @param min
     * @param max >= min
     * @param name the name of the ith variable is name+i
     * @return an array of length n composed of variables in the store cp with domains min..max
     */
	public static CPVarInt[] getArray(Store cp,int n, int min, int max, String name) {
        assert(n > 0);
        assert(max >= min);
		CPVarInt[] res = new CPVarInt[n];
		for (int i = 0; i < res.length; i++) {
			res[i] = new CPVarInt(cp,min,max,""/*name+i*/);
		}
		return res;
	}

    /**
     * @see CPVarInt#getArray(Store, int, int, int, String)
     */
	public static CPVarInt[] getArray(Store cp,int n, int min, int max) {
		return getArray(cp,n,min,max,"");
	}

    /**
     * @param cp
     * @param n1
     * @param n2
     * @param min
     * @param max
     * @return a 2d array dim n1 x n2 composed of variables in the store cp with domains min..max
     */
	public static CPVarInt[][] getArray(Store cp,int n1, int n2, int min, int max) {
		CPVarInt[][] res = new CPVarInt[n1][n2];
		for (int i = 0; i < res.length; i++) {
			res[i] = getArray(cp, n2, min,max);
		}
		return res;
	}

	/**
	 * Should not be used !
	 */
	public void remove() {
		// generated to implement the iterator
		throw new RuntimeException("not implemented");
	}
	
	

	
	//--------------------methods for the scala wrapper--------------------
	/**
	 * Scala wrapper: x + y
	 */
	public CPVarInt $plus(CPVarInt y) {
		return this.plus(y);
	}
	/**
	 * Scala wrapper : x - y
	 */
	public CPVarInt $minus(CPVarInt y) {
		return this.minus(y);
	}
	/**
	 * Scala wrapper= x + d
	 */
	public CPVarInt $plus(int d) {
		return this.plus(d);
	}
	/**
	 * Scala wrapper: x - d
	 */
	public CPVarInt $minus(int d) {
		return this.minus(d);
	}
	/**
	 * Scala wrapper: x * y
	 */
	public CPVarInt $times(CPVarInt y) {
		return this.mul(y);
	}
	/**
	 * Scala wrapper: v * x
	 */
	public CPVarInt $times(int v) {
		return this.mul(v);
	}
	/**
	 * Scala wrapper: -x
	 */
	public CPVarInt $bang() {
		return opposite();
	}
	/**
	 * Scala wrapper: x != v
	 */
	public Constraint $bang$eq(int v) {
		return new Diff(this,v);
	}
	/**
	 * Scala wrapper: x != v
	 */
	public Constraint $bang$eq(CPVarInt x) {
		return new Diff(this,x);
	}
	/**
	 * Scala wrapper: x == v
	 */
	public Constraint $eq$eq(int v) {
		return new Eq(this,v);
	}
	/**
	 * Scala wrapper:  x == y
	 */
	public Constraint $eq$eq(CPVarInt y) {
		return new Eq(this,y);
	}
	

	
	// -------------------- ----------------------------
	
	/**
	 * Scala wrapper: x >= v
	 */
	public Constraint $greater$eq(int v) {
		return new GrEq(this,v);
	}
	/**
	 * Scala wrapper: x >= y
	 */
	public Constraint $greater$eq(CPVarInt y) {
		return new GrEq(this,y);
	}
	/**
	 * Scala wrapper: x > v
	 */
	public Constraint $greater(int v) {
		return new Gr(this,v);
	}
	/**
	 * Scala wrapper: x > y
	 */
	public Constraint $greater(CPVarInt y) {
		return new Gr(this,y);
	}
	/**
	 * Scala wrapper:  x <= v
	 */
	public Constraint $less$eq(int v) {
		return new LeEq(this,v);
	}
	/**
	 * Scala wrapper: x <= y
	 */	
	public Constraint $less$eq(CPVarInt y) {
		return new LeEq(this,y);
	}
	/**
	 * Scala wrapper: x < v
	 */
	public Constraint $less(int v) {
		return new Le(this,v);
	}
	/**
	 * Scala wrapper: x < y
	 */
	public Constraint $less(CPVarInt y) {
		return new Le(this,y);
	}
	
	// -------------------- reified constraints ----------------------------
	
	/**
	 * Scala wrapper: b <=> x == v
	 */
	public CPVarBool $eq$eq$eq(int v) {
		return this.isEq(v);
	}
	/**
	 * Scala wrapper: b <=> x != v
	 */
	public CPVarBool $bang$eq$eq(int v) {
		return this.isDiff(v);
	}
	/**
	 * Scala wrapper: b <=> x == y
	 */
	public CPVarBool $eq$eq$eq(CPVarInt y) {
		return this.isEq(y);
	}
	/**
	 * Scala wrapper: b <=> x != v
	 */
	public CPVarBool $bang$eq$eq(CPVarInt y) {
		return this.isDiff(y);
	}
	/**
	 * Scala wrapper: b <=> x >= v
	 */
	public CPVarBool $greater$eq$eq(int v) {
		return this.isGrEq(v);
	}
	/**
	 * Scala wrapper: b <=> x <= v
	 */
	public CPVarBool $less$eq$eq(int v) {
		return this.isLeEq(v);
	}
	
	
	/**
	 * Scala wrapper: b <=> x >=y
	 */
	public CPVarBool $greater$eq$eq(CPVarInt y) {
		return this.isGrEq(y);
	}
	/**
	 * Scala wrapper: b <=> x <= y
	 */
	public CPVarBool $less$eq$eq(CPVarInt y) {
		return y.isGrEq(this);
	}
	
	/**
	 * Scala wrapper: b <=> x > y
	 */
	public CPVarBool $greater$greater$eq(CPVarInt y) {
		return this.isGrEq(y.plus(1));
	}
	/**
	 * Scala wrapper: b <=> x < y
	 */
	public CPVarBool $less$less$eq(CPVarInt y) {
		return y.isGrEq(this.plus(1));
	}

	
	/**
	 * Scala wrapper: b <=> x > v
	 */
	public CPVarBool $greater$greater$eq(int v) {
		return this.isGrEq(v+1);
	}
	/**
	 * Scala wrapper: b <=> x < y
	 */
	public CPVarBool $less$less$eq(int v) {
		return this.isLeEq(v-1);
	}		

	
	
	
	
//	//--------------------methods for the jython wrapper--------------------
	
//	public CPVarInt __add__(CPVarInt x) {
//		return this.plus(x);
//	}
//	
//	public CPVarInt __radd__(CPVarInt x) {
//		return this.plus(x);
//	}
//	
//	public CPVarInt __add__(int b) {
//		return this.plus(b);
//	}
//	
//	public CPVarInt __radd__(int b) {
//		return this.plus(b);
//	}	
//	
//	public CPVarInt __sub__(CPVarInt x) {
//		return this.minus(x);
//	}
//	
//	public CPVarInt __rsub__(CPVarInt x) {
//		return this.minus(x);
//	}
//	
//	public CPVarInt __sub__(int b) {
//		return this.minus(b);
//	}
//	
//	public CPVarInt __rsub__(int b) {
//		return this.minus(b);
//	}
//	
//	public CPVarInt __mul__(CPVarInt x) {
//		return this.mul(x);
//	}
//	
//	public CPVarInt __rmul__(CPVarInt x) {
//		return this.mul(x);
//	}
//	
//	public CPVarInt __mul__(int b) {
//		return this.mul(b);
//	}
//	
//	public CPVarInt __rmul__(int b) {
//		return this.mul(b);
//	}
//	
//	public CPVarInt __neg__() {
//		return opposite();
//	}
//	
//	public CPVarInt __pos__() {
//		return this;
//	}	
//	
//	public Iterator<Integer>__iter__() {
//		return iterator();
//	}
//	
//	public Constraint __ne__(int v) {
//		return new Diff(this,v);
//	}
//	
//	public Constraint __ne__(CPVarInt x) {
//		return new Diff(this,x);
//	}
//	
//	public Constraint __eq__(int v) {
//		return new Eq(this,v);
//	}
//	
//	public Constraint __eq__(CPVarInt x) {
//		return new Eq(this,x);
//	}
//	
//	public Constraint __ge__(int v) {
//		return new GrEq(this,v);
//	}
//	
//	public Constraint __ge__(CPVarInt x) {
//		return new GrEq(this,x);
//	}
//	
//	public Constraint __gt__(int v) {
//		return new Gr(this,v);
//	}
//	
//	public Constraint __gt__(CPVarInt x) {
//		return new Gr(this,x);
//	}
//	
//	public Constraint __le__(int v) {
//		return new LeEq(this,v);
//	}
//	
//	public Constraint __le__(CPVarInt x) {
//		return new LeEq(this,x);
//	}
//	
//	public Constraint __lt__(int v) {
//		return new Le(this,v);
//	}
//	
//	public Constraint __lt__(CPVarInt x) {
//		return new Le(this,x);
//	}

	
} // end of class

