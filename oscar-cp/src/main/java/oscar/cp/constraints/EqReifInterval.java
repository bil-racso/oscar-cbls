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
import oscar.cp.core.CPIntervalVar;
import oscar.cp.core.Constraint;

/**
 * Reified Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class EqReifInterval extends Constraint {

	CPIntervalVar x;
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
	public EqReifInterval(CPIntervalVar x, int v, CPBoolVar b) {
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
		b.callPropagateWhenBind(this, false);
		x.callPropagateWhenBoundsChange(this, false);
		return propagate();
	}
	

	
	@Override
	public CPOutcome propagate() {
		if (b.isFalse()) {
			//x != v
			if (x.getMax() == v) {
				if (x.updateMax(v-1) == CPOutcome.Failure) return CPOutcome.Failure;
				else return CPOutcome.Success;
			}
			else if (x.getMin() == v) {
				if (x.updateMin(v+1) == CPOutcome.Failure) return CPOutcome.Failure;
				else return CPOutcome.Success;
			}
			else return CPOutcome.Suspend;
		}
		if (b.isTrue()) {
			if (x.assign(v) == CPOutcome.Failure) return CPOutcome.Failure;
		}
		if (x.getMax() < v || x.getMin() > v) {
			b.assign(0);
			return CPOutcome.Success;
		}
		if (x.isBoundTo(v)) {
			b.assign(1);
		}
		return CPOutcome.Suspend;
	}

}

