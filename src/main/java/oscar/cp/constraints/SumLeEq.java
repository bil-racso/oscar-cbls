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
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * Sum Less Or Equal Constraint: x[0]+x[1]+...+x[n] <= y
 * @author Pierre Schaus pschaus@gmail.com
 */
public class SumLeEq extends Constraint {
	
	private CPVarInt [] x;
	private CPVarInt y;

	public SumLeEq(CPVarInt [] x, CPVarInt y) {
		super(x[0].getStore(),"SumLeq");
		this.x = x;
		this.y = y;
	}

    /**
     * sum(x) <= y
     * @param x
     * @param y
     */
	public SumLeEq(CPVarInt [] x, int y) {
		this(x,new CPVarInt(x[0].getStore(),y,y));
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) 
				x[i].callPropagateWhenMinChanges(this);
		}
		if (!y.isBound())
			y.callPropagateWhenMaxChanges(this);
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
		
		if (maxsumx <= y.getMin()) {
			return CPOutcome.Success;
		}
		
		if (y.updateMax(maxsumx) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		for (int i = 0; i < x.length; i++) {
			int minsumxi = minsumx - x[i].getMin();
			int maxi = y.getMax() - minsumxi;
			if (x[i].updateMax(maxi) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Suspend;
	}
	
	

}
