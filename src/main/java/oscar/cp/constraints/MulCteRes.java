/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.constraints;


import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.cp.util.NumberUtils;

/**
 * Multiplication Constraint x * y = c
 * @author Pierre Schaus pschaus@gmail.com
 */
public class MulCteRes extends Constraint {

	private CPVarInt x, y;
	private int c;

    /**
     * x * y == c
     * @param x
     * @param y
     * @param c
     * @see CPVarInt#mul(cp.core.CPVarInt)
     */
	public MulCteRes(CPVarInt x, CPVarInt y, int c) {
		super(x.getStore(),"MulCteRes x*y=c");
		this.x = x;
		this.y = y;
		this.c = c;
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		
		if (x == y) {
			if (s.post(new Square(x,new CPVarInt(s, c,c))) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		
		if (c == 0 && x.hasValue(0) && y.hasValue(0)) {
			x.callPropagateWhenDomainChanges(this);
			y.callPropagateWhenDomainChanges(this);
		} else {
			x.callPropagateWhenBoundsChange(this);
			y.callPropagateWhenBoundsChange(this);
		}
		// propagate must be called after attaching events because this propagator may not reach fix-point it-self.
		CPOutcome ok = propagate();
		if (ok != CPOutcome.Suspend) {
			return ok;
		}
		
		return CPOutcome.Suspend;
	}
		
	@Override
	protected CPOutcome propagate() {
		
		if (c != 0) {
			if (x.removeValue(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.removeValue(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		if (x.isBound()) {
			if (s.post(new MulCte(y,x.getValue(),new CPVarInt(s, c,c))) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else if (y.isBound()) {
			if (s.post(new MulCte(x,y.getValue(),new CPVarInt(s, c,c))) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else {
			if (c == 0) {
				boolean xZero = x.hasValue(0);
				boolean yZero = y.hasValue(0);
				if (xZero || yZero) {
					if (xZero ^ yZero) {
						if (xZero) {
							x.assign(0);
						} else {
							y.assign(0);
						}
						return CPOutcome.Success;
					}
				} else return CPOutcome.Failure;
			} else { // c != 0
				if (propagateVar(x,y) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (propagateVar(y,x) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			return CPOutcome.Suspend;
		}
	}
	
	/**
	 * Filter domain of z with w * z == c with c!=0
	 */
	private CPOutcome propagateVar(CPVarInt w , CPVarInt z) { 
		int a = w.getMin();
		int b = w.getMax();
		
		assert (c != 0);
		assert(a < b);
		
		if (a > 0 || b < 0) {
			// [a,b] > 0 or [a,b] < 0
			if (z.updateMin(NumberUtils.minCeilDiv(c,a,b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (z.updateMax(NumberUtils.maxFloorDiv(c,a,b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Suspend;
		} else if (a == 0) {
			int after0 = w.getValueAfter(0);
			// a=0 ... after0 ... b
			if (z.updateMin(NumberUtils.minCeilDiv(c,after0,b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (z.updateMax(NumberUtils.maxFloorDiv(c,after0,b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Suspend;
		} else if (b == 0) {
			int before0 = w.getValueBefore(0);
			// a ... before0 ... b=0
			if (z.updateMin(NumberUtils.minCeilDiv(c,before0,a)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (z.updateMax(NumberUtils.maxFloorDiv(c,before0,a)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Suspend;
		} else { // a ... 0 ... b
			int before0 = w.getValueBefore(0);
			int after0 = w.getValueAfter(0);
			if (z.updateMin(NumberUtils.minCeilDiv(c, a, before0, after0, b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (z.updateMax(NumberUtils.maxFloorDiv(c, a, before0, after0, b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}			
		}
		return CPOutcome.Suspend;
	}	
}



