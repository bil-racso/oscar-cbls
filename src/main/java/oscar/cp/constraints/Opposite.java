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
		super(x.getStore());
		this.x = x;
		this.y = y;
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
			
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
		
		for (int v = x.getMin(); v <= x.getMax(); v++) {
			if (!x.hasValue(v)) {
				if (y.removeValue(-v) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
		}
		
		for (int v = y.getMin(); v <= y.getMax(); v++) {
			if (!y.hasValue(v)) {
				if (x.removeValue(-v) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
		}
		
		if (!x.isBound()) { //then y is also not bind
			x.callValBindWhenBind(this);
			x.callValRemoveWhenValueIsRemoved(this);
			y.callValBindWhenBind(this);
			y.callValRemoveWhenValueIsRemoved(this);
		}
		
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome valBind(CPVarInt var) {
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
	protected CPOutcome valRemove(CPVarInt var, int val) {
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
