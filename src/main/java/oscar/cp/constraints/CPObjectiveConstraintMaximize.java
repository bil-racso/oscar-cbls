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


public class CPObjectiveConstraintMaximize extends CPObjectiveConstraint {


	public CPObjectiveConstraintMaximize(CPObjective obj, CPVarInt objVar) {
		super(obj, objVar);
	}

	@Override
	protected CPOutcome propagate() {
		if (objVar.updateMin(obj.getBound()) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Suspend;
	}

	
}
