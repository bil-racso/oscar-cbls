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
