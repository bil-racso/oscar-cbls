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
 * Opposite Constraint y = -x
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Opposite extends Constraint {
	
	private CPVarInt x;
	private CPVarInt y;

    /**
     * y == -x
     * @param x
     * @param y
     * @see cp.core.CPVarInt#opposite()
     */
	public Opposite(CPVarInt x, CPVarInt y) {
		super(x.s(),"Opposite");
		this.x = x;
		this.y = y;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
			
		if (y.updateMax(-x.getMin()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		if (y.updateMin(-x.getMax()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		if (x.updateMax(-y.getMin()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		if (x.updateMin(-y.getMax()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		if (!x.isFull()) {
			for (int v = x.getMin(); v <= x.getMax(); v++) {
				if (!x.hasValue(v)) {
					if (y.removeValue(-v) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				}
			}
		}
		if (!y.isFull()) {
			for (int v = y.getMin(); v <= y.getMax(); v++) {
				if (!y.hasValue(v)) {
					if (x.removeValue(-v) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				}
			}
		}
		
		if (!x.isBound()) { //then y is also not bind
			x.callValBindWhenBind(this);
			y.callValBindWhenBind(this);
			
			x.callUpdateBoundsWhenBoundsChange(this);
			y.callUpdateBoundsWhenBoundsChange(this);
			 
			if (l == CPPropagStrength.Strong) {
			 y.callValRemoveWhenValueIsRemoved(this);
			 x.callValRemoveWhenValueIsRemoved(this);
			}
		}
		
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome valBind(CPVarInt var) {
		if (var == x) {
			if (y.assign(-x.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			if (x.assign(-y.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Success;
	}
	
	@Override
	public CPOutcome updateBounds(CPVarInt var) {
		if (var == x) {
			if (y.updateMax(-x.min()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.updateMin(-x.max()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			if (x.updateMax(-y.min()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMin(-y.max()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Suspend;
	}	
	
	@Override
	public CPOutcome valRemove(CPVarInt var, int val) {
		if (var == x) {
			if (y.removeValue(-val) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			if (x.removeValue(-val) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Suspend;
	}

}
