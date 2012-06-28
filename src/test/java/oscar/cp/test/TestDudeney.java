/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.test;


import junit.framework.TestCase;
import java.util.Arrays;

import oscar.cp.constraints.*;
import oscar.cp.core.*;
import oscar.cp.search.*;
import oscar.cp.util.*;
import oscar.reversible.*;
import oscar.search.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestDudeney extends TestCase {

    public TestDudeney(String name) {
        super(name);
        
    }


    public void testDudeney(){
        final Counter nbSol = new Counter();
    	Store cp = new Store();
        int n = 5;

	    CPVarInt [] x = CPVarInt.getArray(cp,n,0,9,"x");

		final CPVarInt nb = new CPVarInt(cp,1,((int) Math.pow(10,n))-1);
		CPVarInt s = new CPVarInt(cp,1,9*n,"s");

		int [] w = new int[n];
		for (int i = 0; i < w.length; i++) {
			w[i] = (int) Math.pow(10,(n-i-1));
		}

		cp.post(new MulVar(s.mul(s),s,nb));

		cp.post(new WeightedSum(w,x,nb));

		cp.post(new Sum(x,s));

        Branching b2 = new Binary(new CPVarInt[]{s});
		Branching b1 = new Binary(x);
		BranchingCombinator b = new BranchingCombinator();
		b.addBranching(b2); b.addBranching(b1);
		Search search = new Search(cp,b,"");
		search.addSolutionObserver(new SolutionObserver() {
            public void solutionFound() {
                nbSol.incr();
                switch (nbSol.getValue()) {
                    case 1:
                        assertEquals(nb.getValue(),1);
                        break;
                    case 2:
                        assertEquals(nb.getValue(),512);
                        break;
                    case 3:
                        assertEquals(nb.getValue(),4913);
                        break;
                    case 4:
                        assertEquals(nb.getValue(),5832);
                        break;
                    case 5:
                        assertEquals(nb.getValue(),17576);
                        break;
                    case 6:
                        assertEquals(nb.getValue(),19683);
                        break;
                    default:
                        assertTrue(false);
                }
            }
        });
		search.solveAll();
        assertEquals(nbSol.getValue(),6);



    }
    

}

