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
 * AtLeastNValue Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AtLeastNValue extends Constraint {
	
	CPVarInt [] x;
	CPVarInt  nval;

    /**
     * This is a generalization of the AllDifferent constraint where we can specify
     * with a variable the number of different values. <br>
     * Available propagation strengths  are Weak and Strong
     * @param x
     * @param nval the number of different values in x
     * @see CPPropagStrength
     * @see AllDifferent
     */
	public AtLeastNValue(CPVarInt [] x, CPVarInt nval) {
		super(x[0].getStore(),"AtLeastNValue");
		this.x = x;
		this.nval = nval;
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		
		if(l == CPPropagStrength.Weak) {
			if (s.post(new AtLeastNValueFWC(x,nval)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			if (s.post(new AtLeastNValueAC(x,nval)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		
		return CPOutcome.Success;
	}

}
