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

import oscar.cp.core.Queue;
import oscar.reversible.ReversibleSearchNode;

/**
 * Class representing a reversible queue pointer
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ReversibleQueue<T> extends ReversiblePointer<Queue<T>> {

	public ReversibleQueue(ReversibleSearchNode node) {
		super(node,null);
	}
}


