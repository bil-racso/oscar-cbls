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
package oscar.cp.constraints;


import oscar.algo.reversible.SetIndexedArray;
import oscar.cp.core.*;


/**
 * Sequence constraint specifying that in any sequence of length q in x, there 
 * are at least min and most max occurrences from a value in set values. 
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Sequence extends Constraint {
	

	private CPVarInt[] xinit;
    private CPVarBool[] x; // x[i] <-> (xinit[i] memberOf values)
	private int min, max, len;
    private SetIndexedArray values;
    private CPVarInt[] cumulatedCounters; // cumulatedCounters[i] = x[0]+x[1]+...+x[i]
	private CPVarInt[][] P; // partial sums Pij = x[i]+...+x[j]
	
	/**
	 * Sequence constraint specifying that in any sequence of length q in x, there 
	 * are at least min and most max occurrences from a value in set values. 
	 * @param x the vector of variables constrained by the Sequence constraint. 
	 * @param values set of values which occurrence is counted within each sequence.
	 * @param l the length of the sequence
	 * @param min the minimal occurrences of values from set within a sequence.
	 * @param max the maximal occurrences of values from set within a sequence.
	 */
	public Sequence(CPVarInt [] x, SetIndexedArray values, int l, int min, int max) {
		super(x[0].s(),"Sequence");
		assert (!(values.getSize() == 0));
		assert(l < x.length);
        assert(l > 0);
		assert(min <= max);
		assert(min > 0);
		assert(max <= l);
        this.xinit = x;
		this.values = values;
		this.len = l;
		this.min = min;
		this.max = max;
	}

	@Override
	public CPOutcome setup(CPPropagStrength cl) {
        // creates the bool vars and create the channeling constraints
        x = new CPVarBool[xinit.length];
        for (int i = 0; i < x.length; i++) {
        	x[i] = CPVarBool.apply(s());
        }
        for (int i = 0; i < x.length; i++) {
            if (s().post(new MemberReif(xinit[i],values,x[i])) == CPOutcome.Failure) {
                return CPOutcome.Failure;
            }
        }
        cumulatedCounters = new CPVarInt[x.length]; // cumulatedCounters[i] = x[0]+x[1]+...+x[i]
        cumulatedCounters[0] = x[0];
        for (int i = 1; i < x.length; i++) {
            cumulatedCounters[i] = cumulatedCounters[i-1].plus(x[i]);
        }
        P = new CPVarInt[x.length][x.length];
        for (int i = 0; i < x.length; i++) {
            P[i][i] = x[i];
            for (int j = i+1; j < Math.min(x.length, i+len); j++) {
                if (i > 0)
                    P[i][j] = cumulatedCounters[j].minus(cumulatedCounters[i-1]);
                else
                    P[i][j] = cumulatedCounters[j];
            }
        }
        for (int i = 0; i < x.length; i++) {
            for (int j = i+1; j < Math.min(x.length, i+len); j++) {
                for (int m = i; m < j; m++) {
                    if (s().post(new Sum(new CPVarInt[]{P[i][m],P[m+1][j]},P[i][j])) == CPOutcome.Failure) {
                        return CPOutcome.Failure;
                    }
                }
            }

            if (i <= x.length-len) {
               if (s().post(new GrEq(P[i][i+len-1],min)) == CPOutcome.Failure) {
                   return CPOutcome.Failure;
               }
               if (s().post(new LeEq(P[i][i+len-1],max)) == CPOutcome.Failure) {
                   return CPOutcome.Failure;
               }
            }

        }
		return CPOutcome.Success;
	}
}
