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
import oscar.cp.core.CPVarBool;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * Bin-Packing constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class BinPacking extends Constraint {
	
	private CPVarInt [] x;
	private int [] w;
	private CPVarInt [] l;
	
	private CPVarBool [][] b;

    /**
     * Links a number of weighted/sized items to be placed into a set of bins.
     * Available propagation strength are Weak and Strong (the default)
     * @param x, x[i] is the bin id in which item i is placed
     * @param w, w[i] is the size/weight of item i
     * @param l, l[j] is the load of bin j (sum of the weights of all items placed into bin j)
     * @see CPPropagStrength
     * @see BinaryKnapsack
     */
	public BinPacking(CPVarInt [] x, int [] w, CPVarInt [] l) {
		super(x[0].getStore(),"BinPacking");
		this.x = x;
		this.w = w;
		this.l = l;
	}

	@Override
	protected CPOutcome setup(CPPropagStrength cl) {
		for (int i = 0; i < x.length; i++) {
			if (x[i].updateMin(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x[i].updateMax(l.length-1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		
		b = new CPVarBool[l.length][];
		//b[i][j] == true iff item x[j] is placed into bin i
		int totW = 0;
		for (int j = 0; j < x.length; j++) {
			totW += w[j];
		}
		for (int i = 0; i < b.length; i++) {
			b[i] = new CPVarBool[x.length];
			for (int j = 0; j < x.length; j++) {
				b[i][j] = x[j].isEq(i);
			}
			if (s.post(new BinaryKnapsack(b[i],w,l[i]),cl) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		for (int j = 0; j < x.length; j++) {
			CPVarBool [] itemj = new CPVarBool[l.length];
			for (int i = 0; i < itemj.length; i++) {
				itemj[i] = b[i][j];
			}
		}
		//redundant constraint
		if (s.post(new Sum(l,new CPVarInt(s,totW,totW))) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Success;
	}
}
