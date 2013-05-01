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
import oscar.cp.core.CPVarBool;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.cp.core.Store;

/**
 * Reified Greater or Equal Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class GrEqVarReif extends Constraint {

	CPVarInt x, y;
	CPVarBool b;

    /**
     * Constraint x greater or equal to y if and only if b is true <br>
     * x >= y <=> b
     * @param x
     * @param y
     * @param b
     */
	public GrEqVarReif(CPVarInt x, CPVarInt y, CPVarBool b) {
		super(x.s(),"GrEqVarReif");
		this.x = x;
		this.y = y;
		this.b = b;
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		CPOutcome oc = updateBounds(x);
		if(oc == CPOutcome.Suspend){
			b.callValBindWhenBind(this);
			x.callUpdateBoundsWhenBoundsChange(this);
			y.callUpdateBoundsWhenBoundsChange(this);
			if (b.isBound()) {
				oc = valBind(b);
			}
		}
		return oc;
	}
	
	@Override
	protected CPOutcome updateBounds(CPVarInt var) {
		if (x.getMin() >= y.getMax()) {
			if (b.assign(1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else if (x.getMax() < y.getMin()) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else {
			return CPOutcome.Suspend;
		}
	}
	
	
	protected int getPriorityBindL1(){
		return Store.MAXPRIORL1-1;
	}
		
	@Override
	protected CPOutcome valBind(CPVarInt var) {
		if (b.getValue() == 0) {
			//x < y
			if (s.post(new Le(x,y)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			//x >= v
			if (s.post(new GrEq(x,y)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}				
		}
		return CPOutcome.Success;
	}

}

