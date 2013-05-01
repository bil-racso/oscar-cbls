/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.reversible;


import java.util.Arrays;

import oscar.cp.core.Store;


public class ReversibleSetIndexedArray extends AbstractSetIndexedArray implements ReversibleSet{
	

	private ReversibleInt size;
	private ReversibleInt maxV;
	private ReversibleInt minV;

	private Store s;
	
	/**
	 * Initializes a set with all values min..max in it
	 * @param s
	 * @param min
	 * @param max >= min
	 */
	public ReversibleSetIndexedArray(Store s, int min, int max) {
		this(s,min,max,false);
	}
	
	/**
	 * Initializes a set that can potentially contain all values min..max in it
	 * @param s
	 * @param min
	 * @param max >= min
	 */
	public ReversibleSetIndexedArray(Store s, int min, int max, boolean empty) {
		this.s = s;
		initIndexes(min, max, empty);
	}

	@Override
	protected void createSizeMinMax() {
		size = new ReversibleInt(s);
		minV = new ReversibleInt(s);
		maxV = new ReversibleInt(s);
	}

	@Override
	protected void setSize(int size) {
		this.size.setValue(size);
	}

	@Override
	protected void setMin(int min) {
		minV.setValue(min);
	}

	@Override
	protected void setMax(int max) {
		maxV.setValue(max);
	}

	@Override
	public int getSize() {
		return size.getValue();
	}

	@Override
	public int getMin() {
		return minV.getValue();
	}

	@Override
	public int getMax() {
		return maxV.getValue();
	}

    public String toString() {
        return Arrays.toString(getSortedVals());
    }
	
}
