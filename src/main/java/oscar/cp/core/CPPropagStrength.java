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
 * A CPPropagStrength is used to ask for strength of filtering when adding/posting a constraint
 * to a Constraint Programming Store.
 * Three levels are possible, it doesn't mean all of them are implemented for every constraint. <br>
 * Note that usually, the strongest is the filtering the slowest is the propagation algorithm. <br>
 * Carefully choosing the filtering is usually done through experimentation (trade-off time and pruning power).
 */
public enum CPPropagStrength {
	Weak, Medium, Strong
}
