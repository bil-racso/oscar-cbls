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
 ******************************************************************************/
package oscar.reversible;

import oscar.reversible.ReversibleSearchNode;

/**
 * Generic Reversible inside a reversible node
 * @author Pierre Schaus pschaus@gmail.com
 */
public abstract class Reversible {
	
	private int lastMagic;
	protected ReversibleSearchNode node;
	
	public Reversible(ReversibleSearchNode node) {
		this.node = node;
		lastMagic = -1;
	}
	
	protected boolean mustBeTrailed() {
		if (lastMagic == node.getMagic()) {
			return false;
		} else{
			return true;
		}
	}
	
	protected void trail() {
		if (!mustBeTrailed()) return;
		else {
			lastMagic = node.getMagic();
			addOnTrail();
		}
	}

    /**
     * Add what encapsulates the state of the object on the trail
     */
	protected abstract void addOnTrail();

    /**
     * Restore the state of the object
     * @param val , an object representing the state necessary for a restoration
     */
	protected abstract void restore(Object val);

}
