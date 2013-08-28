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
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * Weighted Sum / Scala Product Constraint: w[0]*x[0] + w[1]*x[1] + ... + w[n]*x[n] = y  
 * @author Pierre Schaus pschaus@gmail.com
 */
public class WeightedSum extends Constraint {
	
	private int [] w;
	private CPVarInt [] x;
	private CPVarInt y;
	

    /**
     * sum_i w[i]*x[i] == y
     * @param w
     * @param x
     * @param y
     */
	public WeightedSum(int [] w, CPVarInt [] x, CPVarInt y) {
		super(x[0].s(),"WeightedSum");
		
		if (w.length != x.length) {
			throw new RuntimeException("w and x must have the same length");
		}
		
		this.w = w;
		this.x = x;
		this.y = y;
	}

    /**
     * sum_i w[i]*x[i] == s
     * @param w
     * @param x
     * @param s
     */
    public WeightedSum(int [] w, CPVarInt [] x, int sum) {
        this(w,x,CPVarInt.apply(x[0].s(),sum,sum));
    }

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) 
				x[i].callPropagateWhenBoundsChange(this,false);
		}
		if (!y.isBound())
			y.callPropagateWhenBoundsChange(this,false);
		return CPOutcome.Suspend;
	}
	

	
	
	@Override
	public CPOutcome propagate() {
		int maxsumx = 0;
		int minsumx = 0;
		for (int i = 0; i < x.length; i++) {
			
			maxsumx += w[i] >= 0 ? w[i] * x[i].getMax() : w[i] * x[i].getMin();
			minsumx += w[i] >= 0 ? w[i] * x[i].getMin() : w[i] * x[i].getMax();
		}
		if (y.updateMax(maxsumx) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		if (y.updateMin(minsumx) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		//w1 x1 + w2 x2 = y
	    //x1 = (y - w2 x2)/w1
		
		for (int i = 0; i < x.length; i++) {
			if (x[i].isBound()) continue;
			if (w[i] != 0) { //if w[i] == 0 we cannot prune x[i]
				int maxsumxi = maxsumx - (w[i] >= 0 ? w[i] * x[i].getMax() : w[i] * x[i].getMin());
				int minsumxi = minsumx - (w[i] >= 0 ? w[i] * x[i].getMin() : w[i] * x[i].getMax());
				if (w[i] >= 0) {
					int maxi = (int) Math.floor(((double)(y.getMax() - minsumxi))/w[i]);
					int mini = (int) Math.ceil(((double)(y.getMin() - maxsumxi))/w[i]);
				
					//System.out.println("i="+i+" maxi:"+maxi+" mini:"+mini);
					if (x[i].updateMax(maxi) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
					if (x[i].updateMin(mini) ==  CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
					
				} else {
					int maxi = (int) Math.floor(((double)(y.getMin() - maxsumxi))/w[i]);
					int mini = (int) Math.ceil(((double)(y.getMax() - minsumxi))/w[i]);
				
					//System.out.println("i="+i+" maxi:"+maxi+" mini:"+mini);
					if (x[i].updateMax(maxi) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
					if (x[i].updateMin(mini) ==  CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
					
				}
			}
		}
		
		return CPOutcome.Suspend;
	}
	
	

}
