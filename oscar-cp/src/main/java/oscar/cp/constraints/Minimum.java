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

import oscar.algo.reversible.ReversibleInt;
import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Minimum extends Constraint {
	
	
	private CPIntVar [] x;
	private CPIntVar y;
	private ReversibleInt maxval;
	private ReversibleInt maxvalsupport;
	
	private ReversibleInt minval;
	private ReversibleInt minvalsupport;
	
	/**
	 * Constraint y = min(x)
	 * @param x
	 * @param y
	 */
	public Minimum(CPIntVar [] x, CPIntVar y) {
		super(x[0].store(),"Minimum");
		this.x = x;
		this.y = y;
		maxval = new ReversibleInt(s(), 0);
		maxvalsupport = new ReversibleInt(s(), 0);
		minval = new ReversibleInt(s(), 0);
		minvalsupport = new ReversibleInt(s(), 0);
	}
	
	private void updateSupport() {
		int min = Integer.MAX_VALUE;
		int max = Integer.MAX_VALUE;
		for (int i = 0; i < x.length; i++) {
			int m = x[i].getMin();
			int M = x[i].getMax();
			
			if (m < min) {
				minvalsupport.setValue(i);
				minval.setValue(m);
				min = m;
			}
			if (M < max) {
				maxvalsupport.setValue(i);
				maxval.setValue(M);
				max = M;
			}
		}
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		int ymin = y.getMin();
		for (int i=0; i < x.length; i++) {			
			if (x[i].updateMin(ymin) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		updateSupport();
		if (y.updateMin(minval.getValue()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (y.updateMax(maxval.getValue()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound() && (x[i].getMin() < y.getMax())) {
				x[i].callUpdateBoundsIdxWhenBoundsChange(this, i);
			}
		}
		if (!y.isBound()) {
			y.callUpdateBoundsWhenBoundsChange(this);
		}	
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome updateBoundsIdx(CPIntVar x, int idx) {
		if (idx == minvalsupport.getValue() || idx == maxvalsupport.getValue()) {
			updateSupport();
			if (y.updateMin(minval.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.updateMax(maxval.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		if (x.isBound() && x.min() == minval.getValue()) {
			if (y.assign(minval.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		
		return CPOutcome.Suspend;
	}
	
	
	@Override
	public CPOutcome updateBounds(CPIntVar y) {
		int ymin = y.getMin();
		for (int i=0; i < x.length; i++) {			
			if (x[i].updateMin(ymin) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Suspend;
	}

}
