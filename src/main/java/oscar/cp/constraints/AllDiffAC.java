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
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AllDiffAC extends Constraint {

	private CPVarInt[] x;

	public AllDiffAC(CPVarInt[] x) {
		super(x[0].getStore(),"Alldifferent AC");
		this.x = x;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		CPVarInt nvalues = new CPVarInt(s,x.length,x.length);
		CPOutcome ok = s.post(new AtLeastNValueAC(x,nvalues));
		if (ok == CPOutcome.Failure) {
			return CPOutcome.Failure;
		} else {
			return CPOutcome.Success;
		}
	}

}
