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
