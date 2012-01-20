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
import scampi.reversible.Objective;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public abstract class CPObjective extends Constraint implements Objective{
	
	protected CPVarInt objVar;
	
	protected int objVal;

    protected int optimum;
	
	public CPObjective(CPVarInt objVar) {
		super(objVar.getStore(),"Objective");
		this.objVar = objVar;
		relax();
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		objVar.callPropagateWhenBoundsChange(this);
		return propagate();
	}
	
	
	public void setNewBound(int val) {
		objVal = val;
	}
	
	public int getBound() {
		return objVal;
	}

    public int getOptimumBound() {
        return optimum;
    }
    
    @Override
    public boolean isOK() {
    	return propagate() != CPOutcome.Failure;
    }


	

}
