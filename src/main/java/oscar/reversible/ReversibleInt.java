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

package oscar.reversible;

import oscar.reversible.ReversibleSearchNode;

/**
 * Reversible integer
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ReversibleInt extends ReversiblePointer<Integer> {

    /**
     * creates a reversible integer
     * @param node
     */
	public ReversibleInt(ReversibleSearchNode node) {
		super(node,0);
	}
	
	public ReversibleInt(ReversibleSearchNode node, int val) {
		super(node,val);
	}

    /**
     * increment the reversible integer by one
     */
	public void incr(){
		assert(hasValue());
		setValue(getValue()+1);
	}

    /**
     * decrement the reversible integer by one
     */
	public void decr(){
		assert(hasValue());
		setValue(getValue()-1);
	}

}
