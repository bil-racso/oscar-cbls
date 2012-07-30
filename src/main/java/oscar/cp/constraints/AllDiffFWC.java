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
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AllDiffFWC extends Constraint {

	private CPVarInt[] x;
	
	public AllDiffFWC(CPVarInt[] x) {
		super(x[0].s(),"AllDifferent forward checking");
		this.x = x;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {	
		
		for (int i = 0; i < x.length; i++) {
			if(x[i].isBound()) {
				if(valBindIdx(x[i],i) == CPOutcome.Failure)
					return CPOutcome.Failure;
			}
		}
		for (int i = 0; i < x.length; i++) {
			if(!x[i].isBound()) {
				x[i].callValBindIdxWhenBind(this, i);
			}
		}
		return CPOutcome.Suspend;
	}
	
	public CPOutcome valBindIdx(CPVarInt x, int idx) {
		return filterBind(idx);
	}
	
	CPOutcome filterBind(int i) {
		assert(x[i].isBound());

		for (int j = 0; j < x.length; j++) {
			if (j != i) {
				if(x[j].removeValue(x[i].getValue()) == CPOutcome.Failure){
					return CPOutcome.Failure;
				}
			}
		}
		
		return CPOutcome.Suspend;
	}	

}
