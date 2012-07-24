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
		super(act1.start().getStore(),"Binary Disjunctive Activity");
		this.act1 = act1;
		this.act2 = act2;
	}
	
	private CPOutcome notOverlap(Activity act1, Activity act2) {
        CPVarBool b1 = act2.start().isGrEq(act1.end());
        CPVarBool b2 = act1.start().isGrEq(act2.end());
        if (s.post(new Sum(new CPVarBool [] {b1,b2}, 1)) == CPOutcome.Failure) {
                return CPOutcome.Failure;
        }
        return CPOutcome.Suspend;
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		
		if (!act1.start().isBound()) act1.start().callPropagateWhenBoundsChange(this);
		if (!act1.dur().isBound()) act1.dur().callPropagateWhenBoundsChange(this);
		if (!act2.start().isBound()) act2.start().callPropagateWhenBoundsChange(this);
		if (!act2.dur().isBound()) act2.dur().callPropagateWhenBoundsChange(this);
		
		
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}

		return CPOutcome.Suspend;
		
	}

	@Override
	protected CPOutcome propagate() {
		
		if (act1.ect() > act2.lst()) { // act1 cannot precede act2, so act1 must come after act2
			if (act1.start().updateMin(act2.end().getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (act1.start().getMin() >= act2.end().getMax()) {
				return CPOutcome.Success;
			}
		}
		if (act2.ect() > act1.lst()) { // act1 cannot precede act2, so act1 must come after act2
			if (act2.start().updateMin(act1.end().getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (act2.start().getMin() >= act1.end().getMax()) {
				return CPOutcome.Success;
			}
		}	
		
		
		/*
		if (act2.ect() > act1.lst()) { // act2 must come after act1
			if (s.post(new GrEq(act2.start(), act1.end())) == CPOutcome.Failure) {
				System.out.println("failure");
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}*/
		return CPOutcome.Suspend;
	}

}
