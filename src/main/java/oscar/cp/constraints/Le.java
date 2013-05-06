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
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * Less Than Constraint ( x < y)
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Le extends Constraint {

	private CPVarInt x, y;

    /**
     * x < y
     * @param x
     * @param y
     */
	public Le(CPVarInt x, CPVarInt y) {
		super(x.s()," < ");
		this.x = x;
		this.y = y;
	}
	
	public Le(CPVarInt x, int v) {
		this(x, CPVarInt.apply(x.s(),v,v));
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		// y > x
		if(s.post(new Gr(y,x)) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Success;
	}
	
}
