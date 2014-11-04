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
import oscar.cp.core.CPIntervalVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.CPStore;

/**
 * Reified Greater or Equal Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class GrEqVarReif extends Constraint {

	CPIntervalVar x, y;
	CPBoolVar b;

    /**
     * Constraint x greater or equal to y if and only if b is true <br>
     * x >= y <=> b
     * @param x
     * @param y
     * @param b
     */
	public GrEqVarReif(CPIntervalVar x, CPIntervalVar y, CPBoolVar b) {
		super(x.store(),"GrEqVarReif");
		this.x = x;
		this.y = y;
		this.b = b;
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		
		if (x.isBound()) {
			if (s().post(new LeEqCteReif(y, x.value(), b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else if (y.isBound()) {
			if (s().post(new GrEqCteReif(x, y.value(), b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		
		CPOutcome oc = propagate();
		if (oc == CPOutcome.Suspend){
			if (!b.isBound()) b.callValBindWhenBind(this);
			if (!x.isBound()) x.callPropagateWhenBoundsChange(this);
			if (!y.isBound()) y.callPropagateWhenBoundsChange(this);
			if (b.isBound()) {
				oc = valBind(b);
			}
		}
		return oc;
	}
	
	@Override
	public CPOutcome propagate() {
		if (x.getMin() >= y.getMax()) {
			if (b.assign(1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else if (x.getMax() < y.getMin()) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else {
			return CPOutcome.Suspend;
		}
	}
	
	
	protected int getPriorityBindL1(){
		return CPStore.MAXPRIORL1()-1;
	}
		
	@Override
	public CPOutcome valBind(CPIntervalVar var) {
		if (b.getValue() == 0) {
			//x < y
			if (s().post(new Le(x,y)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			//x >= v
			if (s().post(new GrEq(x,y)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}				
		}
		return CPOutcome.Success;
	}

}

