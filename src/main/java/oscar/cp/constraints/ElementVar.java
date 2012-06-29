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

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ElementVar extends Constraint {
	
	CPVarInt [] y;
	CPVarInt x;
	CPVarInt z;

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
			   // if note of the values in dom(z) are in y[v], we can safely remove v from x
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
