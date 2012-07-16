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


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Binary extends Nary {
	
	
	public Binary(CPVarInt ...x) {
		super(x);
	}


	public CPAlternative[] getAlternatives() {
		ArrayList<CPAlternative> alternatives = new ArrayList<CPAlternative>(2);
		
		int j = getVar();
		if(j == -1) return alternatives.toArray(new CPAlternative[]{});
		
		CPAlternative left = new CPAlternative(x[j].getName()+"="+x[j].getMin(),s);
		left.addConstraint(new Eq(x[j],x[j].getMin()));
		alternatives.add(left);
		
		CPAlternative right = new CPAlternative(/*x[j].getName()+"!="+x[j].getMin()*/"",s);
		right.addConstraint(new Diff(x[j],x[j].getMin()));
		alternatives.add(right);	
		
		return alternatives.toArray(new CPAlternative[]{});
	}
	
}
