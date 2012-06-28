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
 * Greater or Equal Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class GrEq extends Constraint {

	CPVarInt x, y;

    /**
     * Constraint x >= y
     * @param x
     * @param y
     * @see GrEqCteReif
     * @see GrEqVarReif
     */
	public GrEq(CPVarInt x, CPVarInt y) {
		super(x.getStore(),"GrEq");
		this.x = x;
		this.y = y;
	}
	
	public GrEq(CPVarInt x, int v) {
		this(x, new CPVarInt(x.getStore(),v,v));
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		CPOutcome oc = propagate();
		if(oc == CPOutcome.Suspend){
			if (!y.isBound()) y.callPropagateWhenMinChanges(this);
			if (!x.isBound()) x.callPropagateWhenMaxChanges(this);
		}
		return oc;
	}
	
	@Override
	protected CPOutcome propagate() {
		if (x.getMin() >= y.getMax()) {
			return CPOutcome.Success;
		}
		if (x.updateMin(y.getMin()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (y.updateMax(x.getMax()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Suspend;
	}

}
