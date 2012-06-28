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
