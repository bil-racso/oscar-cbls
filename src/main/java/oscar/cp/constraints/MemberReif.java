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

import java.util.Set;

import oscar.cp.core.*;
import oscar.reversible.ReversibleInt;
import oscar.reversible.SetIndexedArray;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class MemberReif extends Constraint {

	private CPVarInt x;
	private SetIndexedArray set;
	private CPVarBool b;
	private ReversibleInt inter;  // size of the intersection of D(x) and set
    private ReversibleInt xsize; // size of x

	/**
	 * Reified constraint: b is true iff x is a value from the set
	 * @param x
	 * @param set, should not be modified externally after posting the constraint
	 * @param b
	 */
	public MemberReif(CPVarInt x, SetIndexedArray set, CPVarBool b) {
		super(x.getStore());
		this.x = x;
		this.set = set;
		this.b = b;
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (b.isBound()) {
            return valBind(b);
        }
        if (x.isBound()) {
            return valBind(x);
        }

        int interSize = 0;
		for (int v : set) {
			if (x.hasValue(v)) interSize++;
		}

		if (interSize == 0) { // intersection between the domain of x and set is empty
			return emptyIntersection();
		}
        if (interSize >= x.getSize()) { // D(x) is included in set
           return fullIntersection();
        }
        inter = new ReversibleInt(s,interSize);
        xsize = new ReversibleInt(s,x.getSize());

		x.callValBindWhenBind(this);
        b.callValBindWhenBind(this);
        x.callValRemoveWhenValueIsRemoved(this);

		return CPOutcome.Suspend;

	}

    @Override
    protected CPOutcome valRemove(CPVarInt var, int val) {
        xsize.decr();
        if (set.hasValue(val)) {
            inter.decr();
        }
        if (inter.getValue() == 0) {
            return emptyIntersection();
        }
        if (inter.getValue() == xsize.getValue()) { // D(x) is included in set
           return fullIntersection();
        }
        return  CPOutcome.Suspend;
    }

    @Override
    protected CPOutcome valBind(CPVarInt var) {
        assert(var.isBound());
		if (var == x) {
             if (set.hasValue(x.getValue())) {
                 if (b.assign(1) == CPOutcome.Failure) {
                     return CPOutcome.Failure;
                 }
             } else {
                 if (b.assign(0) == CPOutcome.Failure) {
                     return CPOutcome.Failure;
                 }
             }
             return CPOutcome.Success;
        } else {
            assert(var == b);
            if (b.getValue() == 1) { // x must be a member of the set
                return removeValues(false); // remove all non member values of x and return success if no failure
            } else { // x cannot be a member of the set
                return removeValues(true); // remove all member values of x and return success if no failure
            }
        }
	}

    private CPOutcome emptyIntersection() {
		if (b.assign(0) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Success;
	}

    private CPOutcome fullIntersection() {
		if (b.assign(1) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Success;
	}

    /**
     * @param memberValue = true then remove all values from x that are also member of set
     *        memberValue = false then remove all values from x that are not member of set
     * @return Failure if the domain of x becomes empty, Success otherwise
     */
    private CPOutcome removeValues(boolean memberValue) {
        assert(b.isBound());
        for (int v: x) {
            if ((memberValue && set.hasValue(v)) || (!memberValue && !set.hasValue(v))) {
                if (x.removeValue(v) == CPOutcome.Failure) {
                    return CPOutcome.Failure;
                }
            }
        }
		return CPOutcome.Success;
	}
}

