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
import oscar.cp.core.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Eq extends Constraint {

	CPIntVar x, y;

    /**
     * Constraint x and y to take the same value
     * @param x
     * @param y
     */
	public Eq(CPIntVar x, CPIntVar y) {
		super(x.s(),"Eq");
		this.x = x;
		this.y = y;
	}

    /**
     * Constraint x to take value v
     * @param x
     * @param v
     */
	public Eq(CPIntVar x, int v) {
		this(x,CPIntVar.apply(x.s(),v,v));
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
        if (y.isBound()) {
            if (x.assign(y.getValue()) == CPOutcome.Failure) {
                return CPOutcome.Failure;
            }
            return CPOutcome.Success;
        }
        if (x.isBound()) {
            if (y.assign(x.getValue()) == CPOutcome.Failure) {
                return CPOutcome.Failure;
            }
            return CPOutcome.Success;
        }
		if (x.updateMax(y.getMax()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (x.updateMin(y.getMin()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (y.updateMax(x.getMax()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (y.updateMin(x.getMin()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (l == CPPropagStrength.Strong && (!x.isRange() || !y.isRange())) {
			for (int v = x.min(); v < x.max(); v++) {
				if (x.hasValue(v)) {
					if(!y.hasValue(v)) {
						if(x.removeValue(v) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
					}
				}
			}
			for (int v = y.min(); v < y.max(); v++) {
				if (y.hasValue(v)) {
					if(!x.hasValue(v)) {
						if(y.removeValue(v) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
					}
				}
			}
		}
		if (!x.isBound() && !y.isBound()){
			if (!(x.min() >= 0  && x.max() <= 1 && y.min() >= 0 && y.max() <= 1)) {
				x.callUpdateBoundsWhenBoundsChange(this);
				y.callUpdateBoundsWhenBoundsChange(this);
			}
			
			x.callValBindWhenBind(this);
			y.callValBindWhenBind(this);
			
			if (l == CPPropagStrength.Strong) {
				x.callValRemoveWhenValueIsRemoved(this);
				y.callValRemoveWhenValueIsRemoved(this);
			}
			
		} else {
			return CPOutcome.Success;
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome valBind(CPIntVar var) {
		if (var == x){
			if (y.assign(x.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else {
			if (x.assign(y.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
	}
	
	@Override
	public CPOutcome updateBounds(CPIntVar var) {
		if (var == y) {
			if(x.updateMax(y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if(x.updateMin(y.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Suspend;
		} else {
			if(y.updateMax(x.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if(y.updateMin(x.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Suspend;
		}
	}
	
	@Override
	public CPOutcome valRemove(CPIntVar var, int val) {
		if (var == x) {
			if (y.removeValue(val) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Suspend;
		} else {
			if (x.removeValue(val) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Suspend;
		}
	}
}



