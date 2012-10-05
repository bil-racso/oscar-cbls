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
import oscar.cp.core.Store;
import oscar.cp.util.NumberUtils;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Square extends Constraint {

	private CPVarInt x;
	private CPVarInt y;

	/**
	 * x*x == y
	 * @param x
	 * @param y
	 * @see  CPVarInt#mul(cp.core.CPVarInt)
	 */
	public Square(CPVarInt x, CPVarInt y) {
		super(x.s(),"Square");
		this.x = x;
		this.y = y;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (y.updateMin(0) == CPOutcome.Failure) { 
			return CPOutcome.Failure;
		}
		CPOutcome ok = propagate();
		if (ok != CPOutcome.Suspend) {
			return ok;
		}
		if (!x.isBound()) {
			x.callPropagateWhenBoundsChange(this);
		}
		if (!y.isBound()) {
			y.callPropagateWhenBoundsChange(this);
		}
		return CPOutcome.Suspend;
	}

	@Override
	public CPOutcome propagate() {
		
		    // propagation of y
		    
	
			int mx = x.getMin();
			int Mx = x.getMax();
			int mx2 = mx*mx;
			int Mx2 = Mx*Mx;

			//propagate y (which is not bound)
			if (mx >= 0) { // x will be positive
				if (y.updateMin(mx2) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (y.updateMax(Mx2) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else if (Mx <= 0) { // x is non positive
				if (y.updateMin(Mx2) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (y.updateMax(mx2) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else if (x.hasValue(0)) {
				//y min is already >= 0 (post does it)
				if (y.updateMax(Math.max(mx2, Mx2)) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else {
				Integer a = (Integer) x.valueBefore(0);
				Integer b = (Integer) x.valueAfter(0);
				int a2 = a*a;
				int b2 = b*b;
				if (y.updateMin(Math.min(a2, b2)) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (y.updateMax(Math.max(a2, b2)) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			//propagate x (which is not bound)
			int my = y.getMin();
			int My = y.getMax();
			int my2 = my*my;
			int My2 = My*My;

			int rootm = (int) (Mx <= 0 ? Math.ceil(Math.sqrt(my)) : Math.sqrt(my));
			int rootM = (int) Math.sqrt(My);

			if (mx >= 0) {
				if (x.updateMin(rootm) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (x.updateMax(rootM) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else if (Mx <= 0) {
				if (x.updateMax(-rootm) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (x.updateMin(-rootM) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else {
				if (x.updateMin(-rootM) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}

				if (x.updateMax(rootM) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				/*
				for (int v = -rootm+1; v < rootm; v++) {
					if (x.removeValue(v) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				}*/
			}
		
		return CPOutcome.Suspend;
	}

}
