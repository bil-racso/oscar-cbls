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
 * Alldifferent constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AllDifferent extends Constraint {
	
	private CPVarInt [] x;

    /**
     * Post the constraint that for every pair of variables in x[i], x[j], we have x[i] != x[j] <br>
     * Available propagation strength are Weak (default) and Strong
     * @see CPPropagStrength
     * @param x
     */
	public AllDifferent(CPVarInt ...x) {
		super(x[0].getStore(),"AllDifferent");
		this.x = x;
	}


	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (l == CPPropagStrength.Weak) {
			if (s.post(new AllDiffFWC(x)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			if (s.post(new AllDiffAC(x)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		
		return CPOutcome.Success;
	}

}
