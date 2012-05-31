/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.constraints;

import scampi.cp.core.CPOutcome;
import scampi.cp.core.CPPropagStrength;
import scampi.cp.core.Constraint;
import scampi.cp.core.CPVarInt;
import scampi.reversible.ReversibleInt;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Minimum extends Constraint {
	
	
	private CPVarInt [] x;
	private CPVarInt y;
	private ReversibleInt maxval;
	private ReversibleInt maxvalsupport;
	
	private ReversibleInt minval;
	private ReversibleInt minvalsupport;
	
	/**
	 * Constraint y = min(x)
	 * @param x
	 * @param y
	 */
	public Minimum(CPVarInt [] x, CPVarInt y) {
		super(x[0].getStore(),"Minimum");
		this.x = x;
		this.y = y;
		maxval = new ReversibleInt(s);
		maxvalsupport = new ReversibleInt(s);
		minval = new ReversibleInt(s);
		minvalsupport = new ReversibleInt(s);
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
	protected CPOutcome setup(CPPropagStrength l) {
		
		for (int i=0; i < x.length; i++) {			
			if (x[i].updateMin(y.getMin()) == CPOutcome.Failure) {
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
	protected CPOutcome updateBoundsIdx(CPVarInt x, int idx) {
		if (idx == minvalsupport.getValue() || idx == maxvalsupport.getValue()) {
			updateSupport();
			if (y.updateMin(minval.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.updateMax(maxval.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		if (x.isBound() && x.getValue() == minval.getValue()) {
			if (y.assign(minval.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		
		return CPOutcome.Suspend;
	}
	
	
	@Override
	protected CPOutcome updateBounds(CPVarInt y) {
		for (int i=0; i < x.length; i++) {			
			if (x[i].updateMin(y.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Suspend;
	}

}
