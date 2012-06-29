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
