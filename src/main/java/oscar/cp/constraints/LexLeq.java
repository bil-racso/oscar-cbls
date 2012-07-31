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
import oscar.reversible.ReversibleInt;

/**
 * Lexicographic Less or Equal (LexLeq) Constraint
 * DX TX refer to states name from the paper of M. Carlson & N Beldiceanu
 * and variable names as well.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class LexLeq extends Constraint {
	   
	private CPVarInt [] x;
	private CPVarInt [] y;
	
	private ReversibleInt q;
	private ReversibleInt r;
	private ReversibleInt s;
	private ReversibleInt u; // current state of the automaton
	
	private int i;
	
	private boolean posted;

    /**
     * Constraints the vector x to be lexicographically less or equal to y (lexleq). <br>
     * x lexleq y iff x[i] < y[i] or x[i] == y[i] and x[i+1,...,n] lexleq y[i+1,...,n] <br>
     * example: [0,0,1] lexleq [0,1,0]
     * @param x
     * @param y a vector of same length as x
     */
	public LexLeq(CPVarInt [] x, CPVarInt [] y) {
		super(x[0].s(),"LexLeq");
			
		if (x.length != y.length) {
			throw new RuntimeException("LexLeq: x and y must have the same length");
		}
		
		this.x = x;
		this.y = y;
		q = new ReversibleInt(super.s);
		r = new ReversibleInt(super.s);		
		s = new ReversibleInt(super.s);
		u = new ReversibleInt(super.s);
		u.setValue(0);
		
		posted = false;
	}

	protected CPOutcome setup(CPPropagStrength l) {
		  CPOutcome ok = mySetup(l);
		  posted = true;
		  if (ok == CPOutcome.Failure) {
			  return CPOutcome.Failure;
		  }
		  return ok;
	}
	
	
	
	
	private CPOutcome mySetup(CPPropagStrength l) {
	   i = 0;
	   q.setValue(0);
	   r.setValue(0);
	   s.setValue(0);
	   return state1();
	}
	
	private void setupFrom(int p) {
		if (posted) return;
		for (int i = p ;i < x.length; i++) {
			if (!x[i].isBound()) {
				x[i].callUpdateBoundsIdxWhenBoundsChange(this, i);
			}
			if (!y[i].isBound()) {
				y[i].callUpdateBoundsIdxWhenBoundsChange(this, i);
			}
		}
	}
	
	
	protected CPOutcome updateBoundsIdx(CPVarInt var, int idx) {
		i = idx;
		if (i == q.getValue()) return state1();
		else if (i == r.getValue()) return state2();
		else if (u.getValue() == 3 && (i == s.getValue() || (i < s.getValue() && x[i].getMax() != y[i].getMin()))) return state3();
		else if (u.getValue() == 4 && (i == s.getValue() || (i < s.getValue() && x[i].getMin() != y[i].getMax()))) return state4();
		else return CPOutcome.Suspend;
	}
	
  
	private CPOutcome state1() {
		while(i < x.length && x[i].getMin() == y[i].getMax()) {
			int val = x[i].getMin();
			if (x[i].assign(val) == CPOutcome.Failure || y[i].assign(val) == CPOutcome.Failure)
				return CPOutcome.Failure;
			q.setValue(i = i + 1);
		}
		if (i >= x.length || x[i].getMax() < y[i].getMin()) {
			if (posted) deactivate();
			return CPOutcome.Success; 
		}
		if (x[i].updateMax(y[i].getMax()) == CPOutcome.Failure  || y[i].updateMin(x[i].getMin()) ==  CPOutcome.Failure)
			return CPOutcome.Failure;

		i = i+1 > r.getValue() ? i+1 : r.getValue();
		r.setValue(i);

		return state2();
	}
	   
	private CPOutcome state2() {
		while(i < x.length && x[i].isBound() && y[i].isBound() && x[i].getMin() == y[i].getMin())  //STATE 2
			r.setValue(i = i + 1);
		if (i >= x.length || x[i].getMax() < y[i].getMin()) {
			if (posted) deactivate(); // deactivate the constraint since it is now replaced by a new one
			CPOutcome ok = super.s.post(new LeEq(x[q.getValue()],y[q.getValue()])); // T3
			return ok;
		} else if (x[i].getMin() > y[i].getMax()) {
			if (posted) deactivate();
			CPOutcome ok = super.s.post(new Le(x[q.getValue()],y[q.getValue()])); // T2
			return ok;
		} else if (x[i].getMax() == y[i].getMin() && x[i].getMin() < y[i].getMax()) {
			i = i+1 > s.getValue() ? i+1 : s.getValue();
			s.setValue(i);
			return state3();
		} if (x[i].getMin() == y[i].getMax() && x[i].getMax() > y[i].getMin()) {
			i = i+1 > s.getValue() ? i+1 : s.getValue();
			s.setValue(i);
			return state4();
		}
		setupFrom(q.getValue());
		u.setValue(2);
		return CPOutcome.Suspend; // D1
	}
	
	private CPOutcome state3() {
		while(i < x.length && x[i].getMax() == y[i].getMin())
			i = i+1;
		s.setValue(i);
		if (i>= x.length || x[i].getMax() < y[i].getMin()) {
			if (posted) super.deactivate();
			CPOutcome ok = super.s.post(new LeEq(x[q.getValue()], y[q.getValue()])); // T3
			return ok;
		}
		setupFrom(q.getValue());
		u.setValue(3);
		return CPOutcome.Suspend; // D3
	}
	
	private CPOutcome state4() {
		while (i < x.length && x[i].getMin() == y[i].getMax())
			i = i + 1;
		s.setValue(i);
		if (i < x.length && x[i].getMin() > y[i].getMax()) {
			if (posted) super.deactivate();
			CPOutcome ok = super.s.post(new Le(x[q.getValue()], y[q.getValue()]));
			return ok;
		}
		setupFrom(q.getValue());
		u.setValue(4);
		return CPOutcome.Suspend; // D2
	}
	

}
