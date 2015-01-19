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
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * Alldifferent constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AllDifferent extends Constraint {
	
	private CPIntVar [] x;

    /**
     * Post the constraint that for every pair of variables in x[i], x[j], we have x[i] != x[j] <br>
     * Available propagation strength are Weak (default) and Strong
     * @see CPPropagStrength
     * @param x
     */
	public AllDifferent(CPIntVar ...x) {
		super(x[0].store(),"AllDifferent");
		this.x = x;
	}


	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (l == CPPropagStrength.Weak) {
			if (s().post(new AllDiffFWC(x)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} 
		else if (l == CPPropagStrength.Medium) {
			if (s().post(new AllDiffFWC(x)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (s().post(new AllDiffBC(x)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}			
		}
		else {
			/*
			if (s().post(new AllDiffFWC(x)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}*/
			if (s().post(new AllDiffAC(x)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		
		return CPOutcome.Success;
	}

}
