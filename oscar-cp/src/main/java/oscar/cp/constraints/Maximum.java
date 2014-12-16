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
import oscar.cp.core.CPIntervalVar;
import oscar.cp.core.Constraint;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Maximum extends Constraint {
	
	
	private CPIntervalVar [] x;
	private CPIntervalVar y;
	private ReversibleInt maxval;
	private ReversibleInt maxvalsupport;
	
	private ReversibleInt minval;
	private ReversibleInt minvalsupport;
	
	/**
	 * Constraint y = max(x)
	 * @param x
	 * @param y
	 */
	public Maximum(CPIntervalVar [] x, CPIntervalVar y) {
		super(x[0].store(),"Maximum");
		this.x = x;
		this.y = y;
		maxval = new ReversibleInt(s(), 0);
		maxvalsupport = new ReversibleInt(s(), 0);
		minval = new ReversibleInt(s(), 0);
		minvalsupport = new ReversibleInt(s(), 0);
	}
	
	private void updateSupport() {
		int min = Integer.MIN_VALUE;
		int max = Integer.MIN_VALUE;
		for (int i = 0; i < x.length; i++) {
			int m = x[i].getMin();
			int M = x[i].getMax();
			
			if (m > min) {
				minvalsupport.setValue(i);
				minval.setValue(m);
				min = m;
			}
			if (M > max) {
				maxvalsupport.setValue(i);
				maxval.setValue(M);
				max = M;
			}
		}
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		
		for (int i=0; i < x.length; i++) {			
			if (x[i].updateMax(y.getMax()) == CPOutcome.Failure) {
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
			if (!x[i].isBound() && (x[i].getMax() > y.getMin())) {
				x[i].callUpdateBoundsIdxWhenBoundsChange(this, i);
			}
		}
		if (!y.isBound()) {
			y.callUpdateBoundsWhenBoundsChange(this);
		}	
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome updateBoundsIdx(CPIntervalVar x, int idx) {
		if (idx == minvalsupport.getValue() || idx == maxvalsupport.getValue()) {
			updateSupport();
			if (y.updateMin(minval.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.updateMax(maxval.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		if (x.isBound() && x.getValue() == maxval.getValue()) {
			if (y.assign(maxval.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		
		return CPOutcome.Suspend;
	}
	
	
	@Override
	public CPOutcome updateBounds(CPIntervalVar y) {
		for (int i=0; i < x.length; i++) {			
			if (x[i].updateMax(y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Suspend;
	}

}
