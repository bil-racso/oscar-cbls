/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.search;

import java.util.ArrayList;

import oscar.cp.constraints.Diff;
import oscar.cp.constraints.Eq;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;
import oscar.cp.util.ArrayUtils;
import oscar.search.Branching;





/**
 * Branching selecting as next uninstantiated variable the first one appearing in x. 
 * It creates one branch for each value in the domain of the selected variable.
 * The branches are considered from left to right in increasing order of the values in the domain.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Nary extends Branching {
	
	protected CPVarInt [] x;
	protected Store s;
	
	public Nary(CPVarInt ...x) {
		this.x = x;
		this.s = x[0].getStore();
	}

	/**
	 * Return alternatives corresponding labeling on the variable in increasing order of the values
	 */
	@Override
	public CPAlternative[] getAlternatives() {
		ArrayList<CPAlternative> alternatives = new ArrayList<CPAlternative>(0);
		
		int j = getVar();
		if(j == -1) return alternatives.toArray(new CPAlternative[]{});
		
		alternatives.ensureCapacity(x[j].getSize());
		
		for (Integer v : x[j]) {
			CPAlternative alt = new CPAlternative(x[j].getName()+"="+v,s);
			alt.addConstraint(new Eq(x[j],v));
			alt.addConstraintOnBacktrack(new Diff(x[j],v));
			alternatives.add(alt);
		}
		return alternatives.toArray(new CPAlternative[]{});
	}
	
	/**
	 * @return The index of the next unbound variable in x to branch on, -1 if all variables are bound
	 */
	public int getVar(){
		return ArrayUtils.getFirstNotBound(x);
	}

}
