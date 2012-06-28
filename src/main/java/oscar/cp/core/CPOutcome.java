/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.core;

/**
 * A CPOutcome informs us on the state of a Store or the result of a propagation method.
 * @author Pierre Schaus pschaus@gmail.com
 */
public enum CPOutcome {
	Failure, Success, Suspend
}
