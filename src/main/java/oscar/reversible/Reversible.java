/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
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
