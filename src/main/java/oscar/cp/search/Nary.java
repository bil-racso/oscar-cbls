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
