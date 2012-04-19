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


import scampi.cp.core.CPVarInt;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class CPObjectiveMaximize extends CPObjective{
	
	private CPVarInt objVar;
	
	public CPObjectiveMaximize(CPVarInt objVar) {
		super(objVar);
		this.objVar = objVar;
		optimum = objVar.getMax();
	}

	@Override
	public void tighten() throws RuntimeException{
		if (!objVar.isBound()) {
			throw new RuntimeException("objective not bound, not possible to tighten");
		}
		setNewBound(Math.max(objVal+1, objVar.getValue()+1));
		System.out.println("objective tighten to "+(objVal-1));
	}
	
	@Override
	public void relax() {
		objVal = Integer.MIN_VALUE;
	}

	@Override
    public boolean isOptimum() {
        return getBound() > getOptimumBound();
    }

	@Override
	protected CPObjectiveConstraint createObjectiveConstraint(CPVarInt objVar) {
		return new CPObjectiveConstraintMaximize(this, objVar);
	}



}
