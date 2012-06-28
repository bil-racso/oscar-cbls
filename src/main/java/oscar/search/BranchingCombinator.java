/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
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
