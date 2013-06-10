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
import oscar.cp.core.CPVarBool;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.reversible.ReversibleBool;
import oscar.reversible.ReversibleInt;

/**
 * Logical Or Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Or extends Constraint {
	
	private CPVarBool [] x;
	private CPVarBool y;
	
	ReversibleInt nbBound;
	ReversibleBool ytrue;
	


    /**
     * y is true if at least one of the xi's is true, false otherwise
     * @param x
     * @param y
     */
	public Or(CPVarBool [] x, CPVarBool y) {
		super(x[0].s());
		this.x = x;
		this.y = y;
		nbBound = new ReversibleInt(s,0); // number of values assigned to false
		ytrue = new ReversibleBool(s);
		ytrue.setValue(false);
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		for (int i = 0; i < x.length; i++) {
			if (x[i].isTrue()) {
				if (y.assign(1) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				} else {
					return CPOutcome.Success;
				}
			}
		}
		// we know no variables from X are bound to 1
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) {
				x[i].callValBindIdxWhenBind(this, i);
			} else {
				assert(x[i].isFalse());
				nbBound.incr();
			}
		}
		if (!y.isBound()) {
			if (nbBound.getValue() == x.length) {
				if (y.assign(0) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			y.callValBindWhenBind(this);
		}
		else {
			if (y.getValue() == 0) {
				for (int i = 0; i < x.length; i++) {
						if (x[i].assign(0) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
				}
				return CPOutcome.Success;
			} else {
				ytrue.setValue(true);
				if (nbBound.getValue() == x.length-1) {
					for (int i = 0; i < x.length; i++) {
						if (!x[i].isBound()) {
							if (x[i].assign(1) == CPOutcome.Failure) {
								return CPOutcome.Failure;
							}
							return CPOutcome.Success;
						}
					}
				}
			}
		}
		return CPOutcome.Suspend;
	}
	
	
	@Override
	protected CPOutcome valBindIdx(CPVarInt var, int idx) {
		if (var.getValue() == 1) {
			if (y.assign(1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else {
			nbBound.incr();			
			if (nbBound.getValue() == x.length) {
				if (y.assign(0) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else if (nbBound.getValue() == x.length-1 && ytrue.getValue()){
				for (int i = 0; i < x.length; i++) {
					if (!x[i].isBound()) {
						if (x[i].assign(1) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
						return CPOutcome.Success;
					}
				}
				return CPOutcome.Suspend;
			}
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome valBind(CPVarInt yvar) {
		if (yvar.getValue() == 0) {
			for (int i = 0; i < x.length; i++) {
					if (x[i].assign(0) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				
			}
			return CPOutcome.Success;
		} else {
			ytrue.setValue(true);
			if (nbBound.getValue() == x.length-1){
				for (int i = 0; i < x.length; i++) {
					if (!x[i].isBound()) {
						if (x[i].assign(1) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
						return CPOutcome.Success;
					}
				}
			}
		}
		return CPOutcome.Suspend;
	}

}
