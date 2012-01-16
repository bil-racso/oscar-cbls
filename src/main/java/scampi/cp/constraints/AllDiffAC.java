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
