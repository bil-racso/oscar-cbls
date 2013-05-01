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
package oscar.cp.core;

import java.util.Iterator;

import oscar.reversible.ReversibleBool;
import oscar.reversible.ReversibleInt;
import oscar.reversible.ReversibleSet;
import oscar.reversible.ReversibleSetBitVector;
import oscar.reversible.ReversibleSetIndexedArray;

/**
 * Creates a domain that can contain holes in it (by opposition to a simple range domain). <br>
 * The data structure used to represent the holes is created in a lazy way. <br>
 * It means that as long as a no hole is present, the internal representation of the domain is a range (a min and max value). <br>
 * @author Pierre Schaus pschaus@gmail.com
 */
public class DomainWithHoles implements Domain, Iterable<Integer> {
	
	private ReversibleBool withHoles;
	private ReversibleSetIndexedArray values; //instantiated and used lazily only if there are holes in the domain
	
	private ReversibleInt min,max;
	private ReversibleInt size;
	private Store s;

	
	/**
	 * Construct a Domain with IndexedArray used in case holes are created in the domain
	 * @param s
	 * @param min
	 * @param max
	 */
	public DomainWithHoles(Store s, int min, int max) {
		this.s = s;
		withHoles = new ReversibleBool(s);
		withHoles.setValue(false);
		this.min = new ReversibleInt(s);
		this.min.setValue(min);
		this.max = new ReversibleInt(s);
		this.max.setValue(max);
		this.size = new ReversibleInt(s);
		this.size.setValue(max-min+1);	
	}
	
	
	@Override
	public String toString() {
		if (!withHoles.getValue()) {
			return min+".."+max;
		} else {
			assert(values != null);
			return values.toString();
		}
	}


	private void createValueSet() {
		values = new ReversibleSetIndexedArray(s, min.getValue(), max.getValue());
	}

    /**
     * remove value from the domain
     * @param val
     * @return Failure if the domain is empty, Suspend otherwise
     */
	public CPOutcome removeValue(int val) {
		boolean minChanged = false;
		boolean maxChanged = false;
		if (val < min.getValue() || val > max.getValue()) return CPOutcome.Suspend;
		else if (min.getValue() == val) {
			min.setValue(val + 1);
			minChanged = true;
		}
		else if(max.getValue() == val) {
			max.setValue(val - 1);
			maxChanged = true;
		}
		else if (!withHoles.getValue()) {
				//create the set of values since there are holes in the domain
				withHoles.setValue(true);
				createValueSet();
		}
		
		if (withHoles.getValue()) {
			if (values.removeValue(val)) {
				size.setValue(size.getValue()-1);
			}
		} else {
			size.setValue(size.getValue()-1);
		}
		
		if (isEmpty()) {
			return CPOutcome.Failure;
		} else {
			if (withHoles.getValue()) {
				if (minChanged) min.setValue(values.setMinVal(min.getValue()));
				if (maxChanged) max.setValue(values.setMaxVal(max.getValue()));
			}
			return CPOutcome.Suspend;
		}
	}

    /**
     * Remove all values from the domain except val
     * @param val , the only value to remain in the domain
     * @return Failure if the domain is empty, Suspend otherwise
     */
	public CPOutcome assign(int val) {		
		if (!hasValue(val)) {
			size.setValue(0);
			return CPOutcome.Failure;
		} else {
			size.setValue(1);
			min.setValue(val);
			max.setValue(val);
			if (withHoles.getValue()) {
				values.removeAllBut(val);
			}
			return CPOutcome.Suspend;
		}
	}

    /**
     * Set the minimum of the domain to be >= val
     * @param val
     * @return Failure if the domain is empty, Suspend otherwise
     */
	public CPOutcome updateMin(int val) {	
		if (val <= min.getValue()) return CPOutcome.Suspend;
		else if (val > max.getValue()) return CPOutcome.Failure;
		else if (withHoles.getValue()) {
			min.setValue(values.setMinVal(val));
			size.setValue(values.getSize());	
		} else {
			size.setValue(Math.max(0,max.getValue()-val+1));
			min.setValue(val);
		}	
		if (isEmpty()) {
			return CPOutcome.Failure;
		} else {
			return CPOutcome.Suspend;
		}
	}

    /**
     * Set the maximum of the domain to be <= val
     * @param val
     * @return Failure if the domain is empty, Suspend otherwise
     */
	public CPOutcome updateMax(int val) {
		if (val >= max.getValue()) return CPOutcome.Suspend;
		else if (val < min.getValue()) return CPOutcome.Failure;
		else if (withHoles.getValue()) {
			max.setValue(values.setMaxVal(val));
			size.setValue(values.getSize());	
		} else {
			size.setValue(Math.max(0,val-min.getValue()+1));
			max.setValue(val);
		}
		if (isEmpty()) {
			return CPOutcome.Failure;
		} else {
			return CPOutcome.Suspend;
		}
	}

    /**
     * Test if a value is present in the domain
     * @param val
     * @return true if val is present in the domain, false otherwise
     */
	public boolean hasValue(int val) {
		if (val > max.getValue() || val< min.getValue() || isEmpty()) return false;
		if (!withHoles.getValue()) {
			return true;
		} else {
			return values.hasValue(val);
		}
	}

    /**
     *
     * @param val
     * @return smallest value in the domain >= val, val is returned if no such value
     */
	public int getNextValue(int val) {
		if (val >= max.getValue()) {
			return val;
		}
		else if (val <= min.getValue()) {
			return min.getValue();
		}
        // min < val < max
		else if (!withHoles.getValue()) {
			return val;
		}
		else {
			return values.getNextValue(val);
		}	
	}

    /**
     *
     * @param val
     * @return largest value in the domain < val, val is returned if no such value
     */
	public int getPrevValue(int val) {
		if (val <= min.getValue()) {
			return val;
		}
		else if (val > max.getValue()) {
			return max.getValue();
		}
		else if (!withHoles.getValue()) {
			return val;
		}
		else {
			return values.getPreValue(val);
		}	
	}

    /**
     *
     * @return the size of the domain (number of value in it).
     */
	public int getSize(){
		return Math.max(0,size.getValue());
	}

    /**
     *
     * @return true if the domain is empty, false otherwise
     */
	public boolean isEmpty(){
		return size.getValue() <= 0;
	}

    /**
     *
     * @return the largest value in the domain
     */
	public int getMax(){
		return max.getValue();
	}

    /**
     *
     * @return the smallest value in the domain
     */
	public int getMin(){
		return min.getValue();
	}

	private int curr;
	@Override
	public Iterator<Integer> iterator() {
		if (withHoles.value()) {
			return values.iterator();
		} else {
			curr = min.getValue();
			return new Iterator<Integer>() {
				@Override
				public boolean hasNext() {
					return  curr <= max.getValue();
				}
				@Override
				public Integer next() {
					return curr++;
				}
				@Override
				public void remove() {
					throw new RuntimeException("not implemented");
				}
			};
		}
	}

}
