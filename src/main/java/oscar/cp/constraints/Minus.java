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
 * Minus Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Minus extends Constraint {
	
	private CPVarInt x;
	private CPVarInt y;
	private CPVarInt z;

    /**
     * x - y == z
     * @param x
     * @param y
     * @param z
     * @see CPVarInt#minus(cp.core.CPVarInt)
     */
	public Minus(CPVarInt x, CPVarInt y, CPVarInt z) {
		super(x.s());
		this.x = x;
		this.y = y;
		this.z = z;
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
			
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (!x.isBound()) {
			x.callPropagateWhenBoundsChange(this);
		}
		if (!y.isBound()) {
			y.callPropagateWhenBoundsChange(this);
		}
		if (!z.isBound()) {
			z.callPropagateWhenBoundsChange(this);
		}
		return CPOutcome.Suspend;
	}
	
	
	private CPOutcome prune(CPVarInt A, CPVarInt B, CPVarInt C) {
		//prune var C = A-B 
		if (C.updateMax(A.getMax()-B.getMin()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (C.updateMin(A.getMin()-B.getMax()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome propagate() {
		//x-y=z		
		
		//prune z (= x -y)
		if (prune(x,y,z) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		//prune y (=x-z)
		if (prune(x,z,y) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		
		//prune x (=z+y)
		if (x.updateMax(z.getMax()+y.getMax()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (x.updateMin(z.getMin()+y.getMin()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
					
		return CPOutcome.Suspend;
	}
}
