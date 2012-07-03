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

package oscar.cp.constraints;

import java.util.Arrays;
import java.util.Comparator;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.cp.util.ArrayUtils;
import oscar.reversible.ReversibleInt;




/**
 * Element Constraint on an array of constants
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ElementCst extends Constraint {
	

	private final int [] y;
	private CPVarInt x;
	private CPVarInt z;
	private Integer [] sortedPerm; //y[sortedPerm[0]] is the min element of y... y[sortedPerm[y.length]] is the largest element of y 
	private ReversibleInt minIndSupp;
	private ReversibleInt maxIndSupp;


    /**
     * @param y
     * @param x
     * @param z linked with y and x by the relation y[x] == z
     * @see  Element#get(int[], cp.core.CPVarInt)
     */
	public ElementCst(final int [] y, CPVarInt x, CPVarInt z) {
		super(x.getStore(),"ElementCst");
		this.y = y;
		this.x = x;
		this.z = z;
		
		sortedPerm = new Integer [y.length];
		for (int i = 0; i < y.length; i++) {
			sortedPerm[i] = i;	
		}
		Arrays.sort(sortedPerm, new Comparator<Integer>(){
	
			public int compare(Integer i1, Integer i2) {
				return (y[i1]-y[i2]);
			}});
		minIndSupp = new ReversibleInt(s);
		minIndSupp.setValue(0);
		maxIndSupp = new ReversibleInt(s);
		maxIndSupp.setValue(y.length-1);
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {

		if (x.updateMin(0) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (x.updateMax(y.length-1) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (l == CPPropagStrength.Strong) {
			z.callValRemoveWhenValueIsRemoved(this);
		}
		z.callPropagateWhenBoundsChange(this);
		x.callPropagateWhenDomainChanges(this);		
		x.callValBindWhenBind(this);

		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome valRemove(CPVarInt z, int val) {
		assert( this.z == z);
		for (int i = 0; i < y.length; i++) {
			if (y[i] == val) {
				if (x.removeValue(i) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
		}
		return CPOutcome.Suspend;
	}

	@Override
	protected CPOutcome propagate() {
		// z = y[x] 
		int i = minIndSupp.getValue();
		while (i<y.length && (y[sortedPerm[i]] < z.getMin() || !x.hasValue(sortedPerm[i]))) {
			if (x.removeValue(sortedPerm[i]) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			i++;
		}
		minIndSupp.setValue(i);
		
		if (z.updateMin(y[sortedPerm[i]]) == CPOutcome.Failure){
			return CPOutcome.Failure;
		}
		
		i = maxIndSupp.getValue();
		while (i>=0 && (y[sortedPerm[i]] > z.getMax() || !x.hasValue(sortedPerm[i]))) {
			if (x.removeValue(sortedPerm[i]) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			i--;
		}
		maxIndSupp.setValue(i);
		
		if (z.updateMax(y[sortedPerm[i]]) == CPOutcome.Failure){
			return CPOutcome.Failure;
		}
		return CPOutcome.Suspend;
	}
	
	protected CPOutcome valBind(CPVarInt x) {
		// x is bound
		if (z.assign(y[x.getValue()]) == CPOutcome.Failure)
			return CPOutcome.Failure;
		return CPOutcome.Success;
	}

}
