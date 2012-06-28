/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.core;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class PropagEventRemoveValueIdx extends PropagEvent {
		
	private CPVarInt var;
	private int val;
	private int idx;
		
	public PropagEventRemoveValueIdx(Constraint cstr,CPVarInt var, int idx, int val) {
		super(cstr);
		this.var = var;
		this.idx = idx;
		this.val = val;
	}
	
	int getPrior() {
		return cstr.getPriorityRemoveL1();
	}
	
	@Override
	public CPOutcome notifyConstraint() {
		return cstr.valRemoveIdx(var,idx,val);
	}

}
