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

import scampi.cp.core.*;

/**
 * Global Cardinality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class GCC extends Constraint {
	
	private CPVarInt [] x;
	private int minval;
	private int [] low;
	private int [] up;

    /**
     * Constraint the values minval+i to appear between low[i] and up[i] times in x
     * @param x
     * @param minval
     * @param low
     * @param up
     * @see SoftGCC
     * @see GCCVar
     */
	public GCC(CPVarInt [] x,int minval, int [] low, int [] up) {
		super(x[0].getStore());
		this.x = x;
		this.minval = minval;
		this.low = low;
		this.up = up;
        // super(x,minval,low,up,new CPVarInt(x[0].getStore(),0,0));
    }
	
	protected CPOutcome setup(CPPropagStrength l) {
		CPOutcome ok = CPOutcome.Success;
		if (l == CPPropagStrength.Strong) {
			ok = s.post(new SoftGCC(x, minval, low, up, new CPVarInt(s,0,0)));
		} else {
			ok = s.post(new GCCFWC(x, minval, low, up));
		}
		if (ok == CPOutcome.Failure) return CPOutcome.Failure;
		else return ok;
	}
	

}//end of class GCCVar
