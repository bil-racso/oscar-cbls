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

import java.util.BitSet;
import java.util.Set;
import java.util.TreeSet;



/**
 * Class representing a reversible bitset data structure.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ReversibleBitSet extends Reversible {
	
	private int min,max;
	private BitSet bits;

	public ReversibleBitSet(ReversibleContext node,int min,int max) {
		super(node);
		this.min = min;
		this.max = max;
		bits = new BitSet(max-min+1);
		bits.set(0, max-min+1);
	}
	
	/**
	 * remove the value
	 * @param v
	 * @return true if the value was present, false otherwise
	 */
	public boolean removeValue(int v) {
		if (v<min || v>max) return false;
		trail();
		boolean ret = bits.get(v-min);
		bits.clear(v-min);
		return ret;
	}
	
	/**
	 * adds the value v to the set
	 * @param v
	 */
	public void addValue(int v) {
		assert(v >= min && v <= max);
		trail();
		bits.set(v-min);
	}
	
	public void removeValues(int from, int to) {
		assert(from <= to);
		int f = from < min ? min : from;
		int t = from > max ? max : to;
		trail();
		bits.clear(f-min,t-min);	
	}
	
	public void removeAllValues() {
		trail();
		bits.clear();
	}
	
	public boolean hasValue(int v) {
		if (v < min || v > max) return false;
		return bits.get(v-min);
	}
	

	@Override
	protected void addOnTrail() {
		node.getTrail().addEntry(this, bits.clone());
	}

	@Override
	protected void restore(Object val) {
		assert(val instanceof BitSet);
		this.bits = null;
		this.bits = (BitSet) val;
	}
	
	public Set<Integer> getValues() {
		Set<Integer> res = new TreeSet<Integer>();
		for (int i = bits.nextSetBit(0); i >= 0; i = bits.nextSetBit(i+1)) {
			res.add(min+i);
		}
		return res;
	}
	
	public boolean isEmpty() {
		return bits.isEmpty();
	}
	
	//this method should only be used if !isEmpty
	public int minVal() {
		int i=bits.nextSetBit(0);
		assert i != -1;
		return min+i;
	}
	
	//this method should only be used if !isEmpty
	public int maxVal(){
		int v = -1;
		for(int i=bits.nextSetBit(0); i>=0; i=bits.nextSetBit(i+1)){
			v = i;
		}
		assert v != -1;
		return min+v;
	}
	
	//this method should only be used if !isEmpty
	//return next val in the trail bit, val-1 is returned if no such values
	public int nextVal(int val) {
		assert(val <= max);
		int v = (val<min) ? 0 : val-min;
		int i = bits.nextSetBit(v);
		if (i >= 0)
			return i+min;
		else {
			return val-1;
		}
	}
	
	//this method should only be used if !isEmpty
	//return previous val in the trail bit, val+1 is returned if no such values
	public int prevVal(int val) {
		assert(val >= min);
		int v = val-min;
		int r = bits.nextSetBit(0);
		if (r > v || r == -1) return val+1;
		for (int i = v; i >= 0; i--) {
			if (bits.get(i)) {
				return i+min;
			}
		}
		return val+1;
	}
	
	

}
