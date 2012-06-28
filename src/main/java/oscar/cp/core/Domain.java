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
 * @author Pierre Schaus pschaus@gmail.com
 */
public interface Domain {
	
	/**
	 * Dom <- Dom \ {val}
	 * @param val
	 * @return CPOutcome.Failure if the domain is empty, CPOutcome.Suspend otherwise
	 */
	public CPOutcome removeValue(int val);
	
	
	public CPOutcome assign(int val);
	
	/**
	 * Dom <- Dom \ {v | v in Dom and v < val}
	 * @param val
	 * @return CPOutcome.Failure if the domain is empty, CPOutcome.Suspend otherwise
	 */
	public CPOutcome updateMin(int val);
	
	/**
	 * Dom <- Dom \ {v | v in Dom and v > val}
	 * @param val
	 * @return CPOutcome.Failure if the domain is empty, CPOutcome.Suspend otherwise
	 */
	public CPOutcome updateMax(int val);
	
	/**
	 * @param val
	 * @return |{val} inter Dom| == 1
	 */
	public boolean hasValue(int val);
	
	/**
	 * @param val 
	 * @return max{ val, min {v | v in Dom and v >=val} }
	 */
	public int getNextValue(int val);
	
	/**
	 * @param val 
	 * @return min{ val, max {v | v in Dom and v <=val} }
	 */
	public int getPrevValue(int val);
	
	/**
	 * @return |Dom|
	 */
	public int getSize();
	
	/**
	 * @return |Dom|==0
	 */
	public boolean isEmpty();
	
	/**
	 * @pre !isEmpty()
	 * @return max {v | v in Dom }
	 */
	public int getMax();

	/**
	 * @pre !isEmpty()
	 * @return min {v | v in Dom }
	 */
	public int getMin();
	
}
