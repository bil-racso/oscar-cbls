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
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPBoolVar;
import oscar.cp.core.CPIntVar;
import oscar.cp.core.CPIntervalVar;
import oscar.cp.core.Constraint;

/**
 * Reified Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class EqReif extends Constraint {

	CPIntVar x;
	int v;
	CPBoolVar b;

    /**
     * x is equal to v if and only if b is true i.e.
     * links x,v and b by the relation (x == v) <=> b
     * @param x
     * @param v
     * @param b
     * @see DiffReif
     */
	public EqReif(CPIntVar x, int v, CPBoolVar b) {
		super(x.store(),"EqReif");
		this.x = x;
		this.v = v;
		this.b = b;
		idempotent_$eq(true);
		//priorityBindL1_$eq(CPStore.MAXPRIORL1());
		//priorityL2_$eq(CPStore.MAXPRIORL2());
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (x.isBound())
			return valBind(x);
		else if (b.isBound())
			return valBind(b);
		else {
			x.callValBindWhenBind(this);
			b.callValBindWhenBind(this);
			
			//x.addAC5Bounds(this);
			//x.callValRemoveWhenValueIsRemoved(this);
			
			x.callPropagateWhenDomainChanges(this,false);
			//x.callPropagateWhenBind(this,false);
			//b.callPropagateWhenBind(this,false);
			return propagate();
		}
	}
	
	@Override
	public CPOutcome updateBounds(CPIntervalVar x) {
		if (x.getMax() < v || x.getMin() > v) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome propagate() {
		//if (x.isBound()) return valBind(x);
		//if (b.isBound()) return valBind(b);
		
		if (!x.hasValue(v)) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome valRemove(CPIntVar variable, int val) {		
		if (variable == x && val == v) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		return CPOutcome.Suspend;
	}
	

	@Override
	public CPOutcome valBind(CPIntervalVar var) {
		deactivate();
		if (var == b) {	
			if (b.isFalse()) {
				//x != v
				if (x.removeValue(v) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else {
				//x == v
				if (x.assign(v) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}				
			}
			return CPOutcome.Success;
		}
		else {
			if (x.min() == v) {
				if (b.assign(1) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			else {
				if (b.assign(0) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			return CPOutcome.Success;
		}
	}

}

