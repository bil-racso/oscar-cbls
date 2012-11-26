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
import oscar.reversible.ReversibleInt;

/**
 * Circuit constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Circuit extends Constraint {
	
	private CPVarInt [] succ;
	
	private ReversibleInt [] dest; // dest[i] is the end node index of the longest assigned path (unique) from i
	private ReversibleInt [] lengthToDest; // lengthToDest[i] is the number of edges on the path from i to dest[i]
	private ReversibleInt [] orig; // orig[i] is the origin node index on the assigned path leading to i
	
    /**
     * Ensures that succ represents a valid circuit. <br>
     * succ[i] represents the city visited after city i. Each city is visited once and
     * there is only one tour.<br>
     * Available propagation strengths are Weak (default) and Strong.
     * @param succ
     * @see CPPropagStrength
     */
	public Circuit(CPVarInt [] succ) {
		super(succ[0].s(),"Circuit");
		this.succ = succ;
		
		dest  = new ReversibleInt[succ.length];
		orig  = new ReversibleInt[succ.length];
		lengthToDest = new ReversibleInt[succ.length];
		
		for (int i = 0; i < succ.length; i++) {
			dest[i]  = new ReversibleInt(s,i);
			orig[i]  = new ReversibleInt(s,i);
			lengthToDest[i] = new ReversibleInt(s,0);	
		}	
	}
	
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (s.post(new AllDifferent(succ),l) ==  CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (succ.length > 0) {
			for (int i = 0; i < succ.length; i++) {
				if (succ[i].removeValue(i) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
		}
		for (int i = 0; i < succ.length; i++) {
			if (succ[i].isBound()) {
				if (valBindIdx(succ[i], i) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else {
				succ[i].callValBindIdxWhenBind(this, i);
			}
		}
		return CPOutcome.Suspend;
	}

	@Override
	protected CPOutcome valBindIdx(CPVarInt var, int i) {
		int j = var.getValue();
		// We have a new assigned path because of new edge i->j:
		// o *-> i -> j *-> d
		int d = dest[j].getValue();
		int o = orig[i].getValue();
		// maintain the property
		dest[o].setValue(d);
		orig[d].setValue(o);
		int lengthOrigDest = lengthToDest[o].getValue() + lengthToDest[j].getValue() + 1;
		lengthToDest[o].setValue(lengthOrigDest);
		if (lengthOrigDest < succ.length-1) {
			// otherwise we would have a closed loop with less than n-1 edges
			return succ[d].removeValue(o);
		} else {
			return CPOutcome.Suspend;
		}	
	}

}
