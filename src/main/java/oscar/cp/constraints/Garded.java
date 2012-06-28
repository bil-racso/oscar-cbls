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
import oscar.cp.core.CPVarBool;
import oscar.cp.core.Constraint;

public class Garded extends Constraint {
	
	private CPVarBool b;
	private Constraint c;
	private boolean onTrue;
	
	/**
	 * Garded constraint: c is posted only when b becomes true (if onTrue) or when b becomes false (if onFalse)
	 * @param b
	 * @param c
	 * @param onTrue 
     * @see  Constraint#when(cp.core.CPVarBool)
	 */
	public Garded(CPVarBool b, Constraint c, boolean onTrue) {
		super(b.getStore(),"Garded Constraint");
		this.b = b;
		this.c = c;
		this.onTrue = onTrue;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (!b.isBound()) {
			b.callPropagateWhenBind(this);
			return CPOutcome.Suspend;
		} else {
			if ((b.getValue() == 1 && onTrue) || (b.getValue() == 0 && !onTrue)) {
				if (s.post(c) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			return CPOutcome.Success;
		}	
	}
	
	@Override
	public CPOutcome propagate() {
		if ((b.getValue() == 1 && onTrue) || (b.getValue() == 0 && !onTrue)) {
			if (s.post(c) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Success;
	}

}