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
 * Reified constraint.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class EqReifVar extends Constraint {

	CPVarInt x;
	CPVarInt y;
	CPVarBool b;
	

	/**
     * Ask that x and v take different values if and only if b is true. <br>
     * (x == y) <=> b
     * @param x
     * @param y
     */
	public EqReifVar(CPVarInt x, CPVarInt y, CPVarBool b) {
		super(x.getStore(),"DiffReif");
		this.x = x;
		this.y = y;
		this.b = b;
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (b.isBound()) {
			return valBind(b);
		} 
		else if (x.isBound()) {
			return valBind(x);
		} 
		else if (y.isBound()) {
			return valBind(y);
		}
		else {
			x.callPropagateWhenDomainChanges(this);
			y.callPropagateWhenDomainChanges(this);	
			b.callValBindWhenBind(this);
			x.callValBindWhenBind(this);
			y.callValBindWhenBind(this);
			return propagate();
		}
	}
	
	@Override
	protected CPOutcome valBind(CPVarInt var) {
		if (b.isBound()) {
			if (b.getValue() == 1) {
				// x == y
				if (s.post(new Eq(x,y)) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else {
				//x != y
				if (s.post(new Diff(x,y))  == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			return CPOutcome.Success;
		}	
		else if (x.isBound()) {
			if (s.post(new EqReif(y,x.getValue(),b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else if (y.isBound()) {
			if (s.post(new EqReif(x,y.getValue(),b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		return CPOutcome.Success;
	}
	
	
	
	@Override
	protected CPOutcome propagate() {
		// if the domains of x and y are disjoint we can set b to false and return success
		if (x.getMax() < x.getMin()) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else if (y.getMax() < x.getMin()) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else {
			// there is an overlap between the domain ranges
			// if no values in this overlapping range are common, set b to false
			int start = Math.max(x.getMin(), y.getMin());
			int end = Math.min(x.getMax(), y.getMax());
			boolean commonValues = false;
			for (int i = start; i <= end; i++) {
 				if (x.hasValue(i) && y.hasValue(i)) {
					commonValues = true;
					break;
				}
			}
			if (!commonValues) {
				if (b.assign(0) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				return CPOutcome.Success;
			}
			return CPOutcome.Suspend;
		}
		
	}
	


}

