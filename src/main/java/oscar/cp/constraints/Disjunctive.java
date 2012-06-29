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
import oscar.cp.scheduling.Activity;

/**
 * Discuntive constraint between two activities ensureing arc-B-Consistency on (end(Ai) <= start(Aj)) or (end(Aj) <= start(Ai))
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Disjunctive extends Constraint {
	
	private Activity act1;
	private Activity act2;

	public Disjunctive(Activity act1, Activity act2) {
		super(act1.getStart().getStore(),"Binary Disjunctive Activity");
		this.act1 = act1;
		this.act2 = act2;
	}
	
	private CPOutcome notOverlap(Activity act1, Activity act2) {
        CPVarBool b1 = act2.getStart().isGrEq(act1.getEnd());
        CPVarBool b2 = act1.getStart().isGrEq(act2.getEnd());
        if (s.post(new Sum(new CPVarBool [] {b1,b2}, 1)) == CPOutcome.Failure) {
                return CPOutcome.Failure;
        }
        return CPOutcome.Suspend;
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		
		if (!act1.getStart().isBound()) act1.getStart().callPropagateWhenBoundsChange(this);
		if (!act1.getDur().isBound()) act1.getDur().callPropagateWhenBoundsChange(this);
		if (!act2.getStart().isBound()) act2.getStart().callPropagateWhenBoundsChange(this);
		if (!act2.getDur().isBound()) act2.getDur().callPropagateWhenBoundsChange(this);
		
		
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}

		return CPOutcome.Suspend;
		
	}

	@Override
	protected CPOutcome propagate() {
		
		if (act1.getECT() > act2.getLST()) { // act1 cannot precede act2, so act1 must come after act2
			if (act1.getStart().updateMin(act2.getEnd().getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (act1.getStart().getMin() >= act2.getEnd().getMax()) {
				return CPOutcome.Success;
			}
		}
		if (act2.getECT() > act1.getLST()) { // act1 cannot precede act2, so act1 must come after act2
			if (act2.getStart().updateMin(act1.getEnd().getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (act2.getStart().getMin() >= act1.getEnd().getMax()) {
				return CPOutcome.Success;
			}
		}	
		
		
		/*
		if (act2.getECT() > act1.getLST()) { // act2 must come after act1
			if (s.post(new GrEq(act2.getStart(), act1.getEnd())) == CPOutcome.Failure) {
				System.out.println("failure");
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}*/
		return CPOutcome.Suspend;
	}

}
