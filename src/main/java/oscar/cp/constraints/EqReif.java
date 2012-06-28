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
 * Reified Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class EqReif extends Constraint {

	CPVarInt x;
	int v;
	CPVarBool b;

    /**
     * x is equal to v if and only if b is true i.e.
     * links x,v and b by the relation (x == v) <=> b
     * @param x
     * @param v
     * @param b
     * @see DiffReif
     */
	public EqReif(CPVarInt x, int v, CPVarBool b) {
		super(x.getStore(),"EqReif");
		this.x = x;
		this.v = v;
		this.b = b;
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (x.isBound())
			return valBind(x);
		else if (b.isBound())
			return valBind(b);
		else {
			x.callValBindWhenBind(this);
			b.callValBindWhenBind(this);
			//x.addAC5Bounds(this);
			x.callValRemoveWhenValueIsRemoved(this);
			return CPOutcome.Suspend;
		}
	}
	
	@Override
	protected CPOutcome updateBounds(CPVarInt x) {
		if (x.getMax() < v || x.getMin() > v) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		return CPOutcome.Suspend;
	}
	
	
	protected int getPriorityBindL1(){
		return Store.MAXPRIORL1;
	}
	
	protected int getPriorityRemoveL1(){
		return Store.MAXPRIORL1;
	}
	
//	public int getPriorityAC5Bounds(){
//		return Store.MAXPRIORAC5;
//	}	
	
	@Override
	protected CPOutcome valRemove(CPVarInt x, int val) {
		if (val == v) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		return CPOutcome.Suspend;
	}
	

	@Override
	protected CPOutcome valBind(CPVarInt var) {
		if (b.isBound()) {
			if (b.isFalse()) {
				//x != v
				if (x.removeValue(v) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else {
				//x == v
				if (x.assign(v) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}				
			}
			return CPOutcome.Success;
		}
		
		if (x.isBound()) {
			if (x.getValue() == v) {
				if (b.assign(1) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			else {
				if (b.assign(0) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			return CPOutcome.Success;
		}
		
		return CPOutcome.Suspend;
	}

}

