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
