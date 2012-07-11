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

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarBool;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Not extends Constraint {

	CPVarBool x, y;

    /**
     * Constraint x == !y
     * @param x
     * @param y
     */
	public Not(CPVarBool x, CPVarBool y) {
		super(x.getStore(),"Not");
		this.x = x;
		this.y = y;
	}


	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
        if (y.isBound()) {
            return valBind(y);
        }
        if (x.isBound()) {
            return valBind(x);
        }
        x.callValBindWhenBind(this);
		y.callValBindWhenBind(this);
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome valBind(CPVarInt var) {
        if (y.isBound()) {
            if (x.assign(1-y.getValue()) == CPOutcome.Failure) {
                return CPOutcome.Failure;
            }
            return CPOutcome.Success;
        }
        else {
            if (y.assign(1-x.getValue()) == CPOutcome.Failure) {
                return CPOutcome.Failure;
            }
            return CPOutcome.Success;
        }
	}
	

}



