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
public class PropagEventBindToValueIdx extends PropagEvent {
	
	private CPVarInt var;
	private int idx;
	
	
	public PropagEventBindToValueIdx(Constraint cstr,CPVarInt var, int idx) {
		super(cstr);
		this.var = var;
		this.idx = idx;
	}

	@Override
	public CPOutcome notifyConstraint() {
		return cstr.valBindIdx(var,idx);
	}

}
