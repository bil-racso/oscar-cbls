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
import oscar.search.Objective;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public abstract class CPObjective extends Objective {

    protected int optimum;
    
    protected CPObjectiveConstraint objConstr;
    
	public CPObjective(CPVarInt objVar) {
		relax();
		objConstr = createObjectiveConstraint(objVar);
	}
	
	protected abstract CPObjectiveConstraint createObjectiveConstraint(CPVarInt objVar);

    public int getOptimumBound() {
        return optimum;
    }
    
    public CPObjectiveConstraint getConstraint() {
    	assert objConstr != null;
    	return objConstr;
    }
    
    @Override
    public boolean isOK() {
    	assert objConstr != null;
    	return objConstr.isOK();
    }


	

}
