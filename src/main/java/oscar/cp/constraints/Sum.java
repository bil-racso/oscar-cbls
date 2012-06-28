/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * Sum Constraint: x[0]+x[1]+...+x[n] = y
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Sum extends Constraint {
	
	private CPVarInt [] x;
	private CPVarInt y;

	public Sum(CPVarInt [] x, CPVarInt y) {
		super(x[0].getStore(),"Sum");
		this.x = x;
		this.y = y;
		//setIdempotent();
	}

    /**
     * y == sum(x)
     * @param x
     * @param y
     */
	public Sum(CPVarInt [] x, int y) {
		this(x,new CPVarInt(x[0].getStore(),y,y));
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) 
				x[i].callPropagateWhenBoundsChange(this);
		}
		if (!y.isBound())
			y.callPropagateWhenBoundsChange(this);
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome propagate() {
		int maxsumx = 0;
		int minsumx = 0;
		for (int i = 0; i < x.length; i++) {
			maxsumx += x[i].getMax();
			minsumx += x[i].getMin();
		}
		
		if (y.updateMax(maxsumx) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		if (y.updateMin(minsumx) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		for (int i = 0; i < x.length; i++) {
			int maxsumxi = maxsumx - x[i].getMax();
			int minsumxi = minsumx - x[i].getMin();
			int maxi = y.getMax() - minsumxi;
			int mini = y.getMin() - maxsumxi;
			if (x[i].updateMax(maxi) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x[i].updateMin(mini) ==  CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
			
		return CPOutcome.Suspend;
	}
	
	

}
