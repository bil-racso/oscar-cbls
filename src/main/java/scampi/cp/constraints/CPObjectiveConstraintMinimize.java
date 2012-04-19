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
import scampi.cp.core.CPVarInt;
import scampi.cp.core.Constraint;


public class CPObjectiveConstraintMinimize extends CPObjectiveConstraint {


	public CPObjectiveConstraintMinimize(CPObjective obj, CPVarInt objVar) {
		super(obj, objVar);
	}

	@Override
	protected CPOutcome propagate() {
		if (objVar.updateMax(obj.getBound()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Suspend;
	}

	
}
