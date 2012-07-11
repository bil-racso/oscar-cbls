/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *  
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
package oscar.cp.constraints;


import oscar.cp.core.CPVarInt;

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
