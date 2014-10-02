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
import oscar.cp.core.CPIntervalVar;
import oscar.cp.core.Constraint;

/**
 * Less or Equal Constraint ( x <= y )
 * @author Pierre Schaus pschaus@gmail.com
 */
public class LeEq extends Constraint {

	private CPIntervalVar x, y;

    /**
     * x <= u
     * @param x
     * @param y
     */
	public LeEq(CPIntervalVar x, CPIntervalVar y) {
		super(x.store(),"LeEq");
		this.x = x;
		this.y = y;
	}
	
	public LeEq(CPIntervalVar x, int v) {
		this(x, CPIntervalVar.apply(x.store(),v,v));
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if(s().post(new GrEq(y,x)) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Success;
	}	

}
