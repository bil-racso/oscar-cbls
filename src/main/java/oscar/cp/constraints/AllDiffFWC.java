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
public class AllDiffFWC extends Constraint {

	private CPVarInt[] x;
	
	public AllDiffFWC(CPVarInt[] x) {
		super(x[0].getStore(),"AllDifferent forward checking");
		this.x = x;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {	
		
		for (int i = 0; i < x.length; i++) {
			if(x[i].isBound()) {
				if(valBindIdx(x[i],i) == CPOutcome.Failure)
					return CPOutcome.Failure;
			}
		}
		for (int i = 0; i < x.length; i++) {
			if(!x[i].isBound()) {
				x[i].callValBindIdxWhenBind(this, i);
			}
		}
		return CPOutcome.Suspend;
	}
	
	public CPOutcome valBindIdx(CPVarInt x, int idx) {
		return filterBind(idx);
	}
	
	CPOutcome filterBind(int i) {
		assert(x[i].isBound());

		for (int j = 0; j < x.length; j++) {
			if (j != i) {
				if(x[j].removeValue(x[i].getValue()) == CPOutcome.Failure){
					return CPOutcome.Failure;
				}
			}
		}
		
		return CPOutcome.Suspend;
	}	

}
