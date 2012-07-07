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

import java.util.Hashtable;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.reversible.ReversibleInt;
import scala.util.continuations.cpsMinus;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ElementVar extends Constraint {
	
	private CPVarInt [] y;
	private CPVarInt x;
	private CPVarInt z;
	
	private Hashtable<Integer,ReversibleInt> counters;

	//y[x] = z
	public ElementVar(CPVarInt [] y, CPVarInt x, CPVarInt z) {
		super(y[0].getStore(),"ElementVar");
		this.y = y;
		this.x = x;
		this.z = z;
	}
	
	//y[x] = z
	public ElementVar(CPVarInt [] y, CPVarInt x, int z) {
		this(y,x,new CPVarInt(x.getStore(), z, z));
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
		z.callPropagateWhenBoundsChange(this);
		x.callPropagateWhenDomainChanges(this);			
		for (Integer v: x) {
			y[v].callPropagateWhenBoundsChange(this);
		}
		if (x.isBound()) {
			if (s.post(new Eq(y[x.getValue()], z)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else {
			x.callValBindWhenBind(this);
		}
		
		
		if (l == CPPropagStrength.Strong) {
			z.callValRemoveWhenValueIsRemoved(this);
			x.callValRemoveWhenValueIsRemoved(this);
			for (int i = 0; i < y.length; i++) {
				y[i].callValRemoveIdxWhenValueIsRemoved(this, i);
				if (filterX(i) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			if (filterZ() == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome valBind(CPVarInt x) {
		if (s.post(new Eq(y[x.getValue()], z)) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Success;
	}
	
	
	private void initCounters() {
		counters = new Hashtable<Integer, ReversibleInt>();
		for (int i = 0; i < y.length; i++) {
			if (x.hasValue(i)) {
				for (Integer v: y[i]) {
					ReversibleInt counter = counters.get(v);
					if (counter == null) {
						counter = new ReversibleInt(s,1);
						counters.put(v, counter);
					} else {
						counter.incr();
					}
				}
			}
		}
	}

	private CPOutcome filterZ() {
		// we remove a value from z if it is not present in any domain of y[i] where i is in Dom(x)
		for (int v = z.getMin(); v <= z.getMax(); v++) {
			if (z.hasValue(v)) {
				// check if this value is present in some Dom(y[i]) with i in Dom(x)
				boolean present = false;
				for (Integer i: x) {
					if (y[i].hasValue(v)) {
						present = true;
						break;
					}

				}
				if (!present) {
					if (z.removeValue(v) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				}
			}
		}
		return CPOutcome.Suspend;
	}
	
	// filter dom(x) because dom of y(i) has changed
	private CPOutcome filterX(int i) {
		// if the size of intersection between dom(z) and dom(y[idx]) becomes 0, we can remove idx from x
		if (z.getIntersectionSize(y[i]) == 0) {
			if (x.removeValue(i) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome valRemoveIdx(CPVarInt var, int idx, int val) {
		assert (var == y[idx]);

		 
		return filterX(idx);
	}
	
	
	@Override
	protected CPOutcome valRemove(CPVarInt var, int val) {
		if (var == x) {
			return filterZ();
		} else {
			assert(var == z);
			// if the size of intersection between dom(z) and dom(y[idx]) becomes 0, we can remove idx from x
			for (int i = 0; i < y.length; i++) {
				if (filterX(i) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome propagate() {
		   // z = y[x] 

		   int minY = Integer.MAX_VALUE;
		   int maxY = Integer.MIN_VALUE;
		   for (Integer v: x) {
			   minY = Math.min(minY, y[v].getMin());
			   maxY = Math.max(maxY, y[v].getMax());
		   }

		   // update z
		   if (z.updateMin(minY) == CPOutcome.Failure)
		      return CPOutcome.Failure;
		   if (z.updateMax(maxY) == CPOutcome.Failure)
		      return CPOutcome.Failure;
		   
		   // update x
		   for (int v = x.getMin(); v <= x.getMax(); v++) {
			   if (y[v].getMin() > z.getMax() || y[v].getMax() < z.getMin()) {
				   if (x.removeValue(v) == CPOutcome.Failure) {
					   return CPOutcome.Failure;
				   }
			   }
			   if (z.isBound()) {
				   int zval = z.getValue();
				   if (!y[v].hasValue(zval)) {
					   if (x.removeValue(v) == CPOutcome.Failure) {
						   return CPOutcome.Failure;
					   }
				   }
			   }
			   // could do more pruning taking holes of z into account
			   // if none of the values in dom(z) are in y[v], we can safely remove v from x
			   // could maintain the size of the intersection between dom(z) and y[v] for every v
			   // when the size becomes 0, we can safely remove v from x
		   }
		   
		   if (x.isBound()) { // x is bound
		      CPVarInt yx = y[x.getValue()];
		      if (yx.updateMin(z.getMin()) == CPOutcome.Failure)
		         return CPOutcome.Failure;
		      if (yx.updateMax(z.getMax()) == CPOutcome.Failure)
		         return CPOutcome.Failure;
		   }
		   
		   return CPOutcome.Suspend;
	}

}
