/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.constraints;

import scampi.cp.core.CPOutcome;
import scampi.cp.core.CPPropagStrength;
import scampi.cp.core.Constraint;
import scampi.cp.core.Store;
import scampi.cp.core.CPVarBool;
import scampi.cp.core.CPVarInt;

/**
 * Reified Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Implication extends Constraint {

	CPVarInt A;
	CPVarInt B;
	CPVarInt V;


    /**
     * Logical Implication: A => B <=> V
     * @param A
     * @param B
     * @param V
     */
	public Implication(CPVarBool A, CPVarBool B, CPVarBool V) {
		super(A.getStore(),"Implication");
		this.A = A; 
		this.B = B;
		this.V = V;
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (A.isBound()) return valBind(A);
		else A.callValBindWhenBind(this);
		
		if (B.isBound()) return valBind(B);
		else B.callValBindWhenBind(this);
		
		if (V.isBound()) return valBind(V);
		else V.callValBindWhenBind(this);
		
		if (B.isBound()) return valBind(B);
		else B.callValBindWhenBind(this);
		
		return CPOutcome.Suspend;
	}

	
	
	protected int getPriorityBindL1(){
		return Store.MAXPRIORL1;
	}


	@Override
	protected CPOutcome valBind(CPVarInt var) {
		if (A.isBound()) {
			if (A.isBoundTo(0)) {
				// F => X is always true
				if (V.assign(1) == CPOutcome.Failure) return CPOutcome.Failure;
				return CPOutcome.Success;
			} else {
				// T => B <-> V
				if (B.isBoundTo(0)) { // T => F it means V must be F
					if (V.assign(0) == CPOutcome.Failure) return CPOutcome.Failure;
					return CPOutcome.Success;
				}
				if (B.isBoundTo(1)) { // T => T it means V must be T
					if (V.assign(1) == CPOutcome.Failure) return CPOutcome.Failure;
					return CPOutcome.Success;
				}
				// the case of whether V is bound is treated below
			}
		} 
		if (B.isBound()) {
			if (B.isBoundTo(1)) { // V is always true in this case
				if (V.assign(1) == CPOutcome.Failure) return CPOutcome.Failure;
				return CPOutcome.Success;
			} else {
				// A => F <-> V
				// case A is bound is treated above and V is bound is treated below
			}
		}	
		if (V.isBound()) {
			if (V.getValue() == 0) {
				// only way to get A => B <-> F is to have T => F
				if (A.assign(1) == CPOutcome.Failure) return CPOutcome.Failure;
				if (B.assign(0) == CPOutcome.Failure) return CPOutcome.Failure;
				return CPOutcome.Success;
			} else {
				// V is True
				if (B.isBoundTo(0)) {  // A => F <-> T it means A must be F
					if (A.assign(0) == CPOutcome.Failure) return CPOutcome.Failure;
					return CPOutcome.Success;
				}
				if (B.isBoundTo(1)) { // A => T <-> T it means A can be true of false, doesn't matter
					return CPOutcome.Success;
				}
			}
		}
		return CPOutcome.Suspend;
	}

}

