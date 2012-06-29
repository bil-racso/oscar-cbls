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
 ******************************************************************************/
package oscar.search;

import java.util.ArrayList;
import java.util.Vector;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class BranchingCombinator extends Branching {
	
	
	Vector<Branching> branchings;
	
	public BranchingCombinator() {
		branchings = new Vector<Branching>(2);
	}
	
	@Override
	public void initialize() {
		for (Branching b : branchings) {
			b.initialize();
		}
	}

	public Alternative[] getAlternatives() {
		for (Branching b: branchings) {
			Alternative[] alt = b.getAlternatives();
			if (b.getAlternatives() != null && alt.length != 0) {
				return alt;
			}
		}
		return noAlternative;
	}
	
	public void addBranching(Branching b) {
		branchings.add(b);
	}

}
