/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.search;

import java.util.ArrayList;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public abstract class Branching {
	
	public static final Alternative[] noAlternative = new Alternative[0];
	
	/**
	 * Initialize the branching
	 * For instance in some branching strategy, some statistical probing must be done. This is the place to do it.
	 */
	public void initialize() {}
	
	public abstract Alternative[] getAlternatives();
}
