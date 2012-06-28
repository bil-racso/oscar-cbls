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


public abstract class CPObjectiveConstraint extends Constraint {


	protected CPVarInt objVar;
	
	protected CPObjective obj;
	
	public CPObjectiveConstraint(CPObjective obj, CPVarInt objVar) {
		super(objVar.getStore(),"Objective");
		this.objVar = objVar;
		this.obj = obj;
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		objVar.callPropagateWhenBoundsChange(this);
		return propagate();
	}
	
	public boolean isOK() {
		return propagate() != CPOutcome.Failure;
	}
	
}
