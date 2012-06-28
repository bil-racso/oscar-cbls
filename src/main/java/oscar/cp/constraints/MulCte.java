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
 * Multiplication Constraint x * c = z
 * @author Pierre Schaus pschaus@gmail.com
 */
public class MulCte extends Constraint {

	private CPVarInt x, z;
	private int c;

    /**
     * x * c == z
     * @param x
     * @param c
     * @param z
     * @see CPVarInt#mul(int)
     */
	public MulCte(CPVarInt x, int c, CPVarInt z) {
		super(x.getStore(),"MulCte");
		this.x = x;
		this.z = z;
		this.c = c;
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		CPOutcome ok = propagate();
		if (ok == CPOutcome.Suspend) {
			x.callPropagateWhenBoundsChange(this);
			z.callPropagateWhenBoundsChange(this);
		}
		return ok;
	}
	
	@Override
	protected CPOutcome propagate() {
		if (x.isBound()) {
			if (z.assign(NumberUtils.safeMul(c , x.getValue())) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else {
			if (c == 0) {
				if (z.assign(0) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				return CPOutcome.Success;
			} else {
				if (z.updateMin(Math.min(NumberUtils.safeMul(c , x.getMin()), NumberUtils.safeMul(c , x.getMax()))) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (z.updateMax(Math.max(NumberUtils.safeMul(c , x.getMin()), NumberUtils.safeMul(c , x.getMax()))) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (x.updateMin(Math.min(NumberUtils.ceilDiv(z.getMin(), c), 
										 NumberUtils.ceilDiv(z.getMax(), c))) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (x.updateMax(Math.max(NumberUtils.floorDiv(z.getMin(), c), 
										 NumberUtils.floorDiv(z.getMax(), c))) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
				if (x.getSize() <= 200) { // remove all numbers not multiples of c if dom size to too big
					for (int v = z.getMin(); v <= z.getMax(); v++) {
						if (z.hasValue(v) && (v%c != 0)) {
							if (z.removeValue(v) == CPOutcome.Failure) {
								return CPOutcome.Failure;
							}
						}
					}
				}
				return CPOutcome.Suspend;
			}
		}
	}
}
