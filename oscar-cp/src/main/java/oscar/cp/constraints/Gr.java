/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPStore;
import oscar.cp.core.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * Strictly Greater Than Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Gr extends Constraint {

	CPIntVar x, y;

    /**
     * x > y
     * @param x
     * @param y
     */
	public Gr(CPIntVar x, CPIntVar y) {
		super(x.s(),">");
		this.x = x;
		this.y = y;
	}
	
	public Gr(CPIntVar x, int v) {
		this(x, CPIntVar.apply(x.s(),v,v));
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		priorityL2_$eq(CPStore.MAXPRIORL2());
		CPOutcome oc = propagate();
		if(oc == CPOutcome.Suspend){
			if (!y.isBound()) y.callPropagateWhenMinChanges(this,false);
			if (!x.isBound()) x.callPropagateWhenMaxChanges(this,false);
		}
		return oc;
	}
	
	@Override
	public CPOutcome propagate() {
		if (x.getMin() > y.getMax()) {
			return CPOutcome.Success;
		}
		if (x.updateMin(y.getMin()+1) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (y.updateMax(x.getMax()-1) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Suspend;
	
	}

}
