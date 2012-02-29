/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.reversible;


/**
 * Reversible boolean
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ReversibleBool extends ReversiblePointer<Boolean> {

	public ReversibleBool(ReversibleSearchNode node, boolean b) {
		super(node,b);
	}
	
	public ReversibleBool(ReversibleSearchNode node) {
		super(node, false);
	}	
	
}
