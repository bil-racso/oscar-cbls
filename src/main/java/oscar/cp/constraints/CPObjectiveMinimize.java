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
public class CPObjectiveMinimize extends CPObjective {
	
	
	private CPVarInt objVar;
	
	public CPObjectiveMinimize(CPVarInt objVar) {
		super(objVar);
		this.objVar = objVar;
		optimum = objVar.getMin();
	}	
	
	@Override
	public void tighten() throws RuntimeException {
		if (!objVar.isBound()) {
			System.out.println(objVar);
			throw new RuntimeException("objective not bound, not possible to tighten");
		}
		setNewBound(Math.min(objVal-1, objVar.getValue()-1));
		System.out.println("objective tighten to "+(objVal+1));
	}
	
	@Override
	public void relax() {
		objVal = Integer.MAX_VALUE;
	}

	@Override
    public boolean isOptimum() {
        return getBound() < getOptimumBound();
    }

	@Override
	protected CPObjectiveConstraint createObjectiveConstraint(CPVarInt objVar) {
		return new CPObjectiveConstraintMinimize(this, objVar);
	}
	
}
