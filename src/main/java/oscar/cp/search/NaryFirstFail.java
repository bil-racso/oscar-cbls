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
 * Branching selecting as next uninstantiated variable the one with smallest domain. 
 * It creates one branch for each value in the domain of the selected variable.
 * The branches are considered from left to right in increasing order of the values in the domain.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class NaryFirstFail extends Nary {
	

	
	public NaryFirstFail(CPVarInt ...x) {
		super(x);
	}
	
	/**
	 * Return the index
	 */
	@Override
	public int getVar() {
		return ArrayUtils.getMinDomNotBound(x);
	}



}
