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
package oscar.algo.reversible;

import java.util.ArrayList;
import java.util.Set;
import java.util.TreeSet;



/**
 * @author Pierre Schaus pschaus@gmail.com
 * 
 * Implementation of a TrailSet using bitvector.
 * A bitvector is conceptually very simple: sequence of 0 or 1 to indicate whether a value is present or not.
 * To avoid trailing all the bitvector as soon as one entry is flipped, the bitvector is represented as an array of TrailBitSet.
 * This allow to trail in a more fine grained fashion so that only the TrailBitSet containing the changed value is trailed.
 * 
 */

public class ReversibleSetBitVector implements ReversibleSet {
	
	private ReversibleNode s;
	
	private int minBitSetVal;
	private int bitSetSize = 32;
	private ReversibleBitSet [] bitSets;
	private ReversibleInt maxV;
	private ReversibleInt minV;
	private ReversibleInt size;
	
	private int _min;
	private int _max;
	
	public ReversibleSetBitVector(ReversibleNode s, int min, int max) {
		assert(max >= min);
		this.s = s;
		this._min = min;
		this._max = max;
		minV = new ReversibleInt(s,min);
		maxV = new ReversibleInt(s,max);
		size = new ReversibleInt(s,max-min+1);
		int nbVal = max-min+1;
		int nbBitSets = nbVal/bitSetSize + (nbVal%bitSetSize == 0 ? 0 : 1);
		bitSets = new ReversibleBitSet[nbBitSets];
		minBitSetVal = min;
		for (int i = 0; i < bitSets.length-1; i++) {
			int mi = min+i*bitSetSize;
			int ma = mi+bitSetSize-1;
			bitSets[i] = new  ReversibleBitSet(s,mi,ma);
		}
		bitSets[nbBitSets-1] = new ReversibleBitSet(s,min+(nbBitSets-1)*bitSetSize , max); // the last bitset going up to max
		
		assert(checkBounds());
	}
	
	public int getSize() {
		return size.getValue();
	}
	
	private boolean checkBounds() {
		if (isEmpty()) return true;
		return hasValue(minV.getValue()) && hasValue(maxV.getValue());
	}
	
	private int getBitSetIndex(int val) {
		return (val-minBitSetVal)/bitSetSize;
	}
	
	private ReversibleBitSet getBitSet(int val) {
		int index = getBitSetIndex(val);
		return bitSets[index];
	}

	public boolean removeValue(int val) {
		assert(checkBounds());
		if (!hasValue(val)) return false;
		size.decr();
		getBitSet(val).removeValue(val); //we are sure the value is present
		if (maxV.getValue() == minV.getValue()) { //unique value is removed
			minV.incr();
		} else {
			if (maxV.getValue() == val) { //the max has changed
				int newmax = getPreValue(val-1);
				assert(newmax < val);
				maxV.setValue(newmax); // we are sure there is a prev val
			}
			else if (minV.getValue() == val) { //the min has changed
				int newmin = getNextValue(val+1);
				assert(newmin > val);
				minV.setValue(newmin); // we are sure there is a next val
			}	
		}
		assert(checkBounds());
		return true;
	}
	
	public void removeAllBut(int v) {
		assert(checkBounds());
		assert(hasValue(v));
		//remove everything
		for(int i = 0; i < bitSets.length; i++) {
			ReversibleBitSet bits = bitSets[i];
			if (!bits.isEmpty()) {
				bits.removeAllValues();
			}
		}
		//set back the only remaining value v
		getBitSet(v).addValue(v);
		maxV.setValue(v);
		minV.setValue(v);
		size.setValue(1); //singleton
		assert(checkBounds());
	}
	
	public boolean hasValue(int val) {
		return val >= minV.getValue() && val <= maxV.getValue() && getBitSet(val).hasValue(val);
	}

	public int getNextValue(int val) {
		if (isEmpty())
			return val-1;
		else if (hasValue(val)) {
			return val;
		} 
		else if (val <= minV.getValue()) {
			return minV.getValue();
		}
		else {		
			for(int i = getBitSetIndex(val); i <= getBitSetIndex(maxV.getValue()); i++) {
				ReversibleBitSet bits = bitSets[i];
				if (!bits.isEmpty()) {
					int res = bits.nextVal(val);
					if (res > val-1 && res <= maxV.getValue()) { //a value is found
						return res;
					}
				}
			}
			return val-1;
		}
	}
	
	public int getPreValue(int val) {
		if (isEmpty()) return val+1;
		else if (hasValue(val)) {
			return val;
		} 
		else if (val >= maxV.getValue()) {
			return maxV.getValue();
		}
		else {				
			for (int i = getBitSetIndex(val); i >= getBitSetIndex(minV.getValue()); i--) {
				ReversibleBitSet bits = bitSets[i];
				if (!bits.isEmpty()) {
					int res = bits.prevVal(val);
					if (res < val+1 && res >= minV.getValue()) { //a value is found
						return res;
					}
				}
			}
			return val+1; //no value found
		}	
	}
	
	public boolean isEmpty() {
		return maxV.getValue() < minV.getValue();
	}
	
	public int setMinVal(int min) {
		assert(checkBounds());
		if (min <= minV.getValue()) return minV.getValue();
		else if (min > maxV.getValue()) {
			size.setValue(0); // the set is empty
			minV.setValue(min);
			return Integer.MIN_VALUE;
		} else {
			//iteration over the removed value to adapt the size
			for (int v = getNextValue(minV.getValue()); v < min; v = getNextValue(v+1)) {
				size.decr();
			}
			minV.setValue(getNextValue(min));
		}
		assert(checkBounds());
		return minV.getValue();
	}
	
	public int setMaxVal(int max) {
		assert(checkBounds());
		if (max >= maxV.getValue()) return maxV.getValue();
		else if (max < minV.getValue()) {
			size.setValue(0); // the set is empty
			maxV.setValue(max);
			return Integer.MAX_VALUE;
		} else {
			//iteration over the removed value to adapt the size
			for (int v = getPreValue(maxV.getValue()); v > max; v = getPreValue(v-1)) {
				size.decr();
			}
			maxV.setValue(getPreValue(max));
		}
		assert(checkBounds());
		return maxV.getValue();
	}
	
	@Override
	public String toString() {
		Set<Integer> res = new TreeSet<Integer>();
		for (int v = minV.getValue(); v <= maxV.getValue(); v++) {
			if (hasValue(v)) res.add(v);
		}
		return res.toString();
	}
	
	public Integer[] getValues() {
		ArrayList<Integer> vals = new ArrayList<Integer>();
		for (int v = minV.getValue(); v <= maxV.getValue(); v++) {
			if (hasValue(v)) {
				vals.add(v);
			}
		}
		return vals.toArray(new Integer[]{});
	}
	
}
