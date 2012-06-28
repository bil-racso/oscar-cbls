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
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.cp.core.Store;

/**
 * Reified Greater or Equal Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class LeEqCteReif extends Constraint {

	CPVarInt x;
	int v;
	CPVarBool b;

    /**
     * Constraint x less or equal to v if and only if b is true <br>
     * x <= v <=> b
     * @param x
     * @param v
     * @param b
     */
	public LeEqCteReif(CPVarInt x, int v, CPVarBool b) {
		super(x.getStore(),"GrEqCteReif");
		this.x = x;
		this.v = v;
		this.b = b;
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		CPOutcome oc = updateBounds(x);
		if(oc == CPOutcome.Suspend){
			b.callValBindWhenBind(this);
			x.callUpdateBoundsWhenBoundsChange(this);
			if (b.isBound()) {
				oc = valBind(b);
			}
		}
		return oc;
	}
	
	@Override
	protected CPOutcome updateBounds(CPVarInt x) {
		if (x.getMax() <= v) {
			if (b.assign(1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else if (x.getMin() > v) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else {
			return CPOutcome.Suspend;
		}
	}
	
	
	protected int getPriorityBindL1(){
		return Store.MAXPRIORL1;
	}
		
	@Override
	protected CPOutcome valBind(CPVarInt var) {
		if (b.getValue() == 0) {
			//x > v
			if (x.updateMin(v+1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			//x <= v
			if (x.updateMax(v) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}				
		}
		return CPOutcome.Success;
	}

}

