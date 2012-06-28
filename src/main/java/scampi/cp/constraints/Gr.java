/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.constraints;

import scampi.cp.core.CPOutcome;
import scampi.cp.core.CPPropagStrength;
import scampi.cp.core.Constraint;
import scampi.cp.core.CPVarInt;

/**
 * Strictly Greater Than Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Gr extends Constraint {

	CPVarInt x, y;

    /**
     * x > y
     * @param x
     * @param y
     */
	public Gr(CPVarInt x, CPVarInt y) {
		super(x.getStore(),"Gr");
		this.x = x;
		this.y = y;
	}
	
	public Gr(CPVarInt x, int v) {
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
		if (x.getMin() > y.getMax()) {
			return CPOutcome.Success;
		}
		if (x.updateMin(y.getMin()+1) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (y.updateMax(x.getMax()-1) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Suspend;
	
	}

}
