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


import oscar.cp.core.CPVarInt;
import oscar.cp.util.ArrayUtils;

/**
 * Branching selecting as next uninstantiated variable x the one with smallest domain D(x). 
 * It creates two branches.
 * On the left branch the variable is constrained to take its smallest value x = min(D(x)) while
 * on the right branch the variable is constrained to take a different value x != min(D(x)).
 * @author Pierre Schaus pschaus@gmail.com
 */
public class BinaryFirstFail extends Binary {
	

	
	public BinaryFirstFail(CPVarInt ...x) {
		super(x);
	}
	
	
	@Override
	public int getVar() {
		return ArrayUtils.getMinDomNotBound(x);
	}



}
