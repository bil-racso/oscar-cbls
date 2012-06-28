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
