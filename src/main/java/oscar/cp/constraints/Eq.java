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
 * Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Eq extends Constraint {

	CPVarInt x, y;

    /**
     * Constraint x and y to take the same value
     * @param x
     * @param y
     */
	public Eq(CPVarInt x, CPVarInt y) {
		super(x.getStore(),"Eq");
		this.x = x;
		this.y = y;
	}

    /**
     * Constraint x to take value v
     * @param x
     * @param v
     */
	public Eq(CPVarInt x, int v) {
		this(x,new CPVarInt(x.getStore(),v,v));
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
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
		if(x.updateMax(y.getMax()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if(x.updateMin(y.getMin()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if(y.updateMax(x.getMax()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if(y.updateMin(x.getMin()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (l == CPPropagStrength.Strong) {
			for(Integer v:x) {
				if(!y.hasValue(v)) {
					if(x.removeValue(v) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				}
			}
			for(Integer v:y) {
				if(!x.hasValue(v)) {
					if(y.removeValue(v) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				}
			}
		}
		if(!x.isBound() && !y.isBound()){
			x.callUpdateBoundsWhenBoundsChange(this);
			y.callUpdateBoundsWhenBoundsChange(this);
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
	protected CPOutcome valBind(CPVarInt var) {
		if(var == x){
			if (y.assign(x.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else{
			if (x.assign(y.getValue()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
	}
	
	@Override
	protected CPOutcome updateBounds(CPVarInt var) {
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
	protected CPOutcome valRemove(CPVarInt var, int val) {
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



