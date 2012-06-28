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
import oscar.cp.constraints.BinaryKnapsack;
import oscar.cp.constraints.Diff;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarBool;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;
import oscar.cp.util.ArrayUtils;

import junit.framework.TestCase;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestBinaryKnapsack extends TestCase {
	

	

    public TestBinaryKnapsack(String name) {
        super(name);
        
    }
    	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();
    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();
    }
    
    public void testa() {

        Store s = new Store();
        CPVarBool [] b = new CPVarBool[111];
        for (int i = 0; i < b.length; i++) {
			b[i] = new CPVarBool(s);
		}

        CPVarInt l = new CPVarInt(s,12,44);
        int [] w = new int[]{2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 12, 12, 12, 13, 13, 13, 13, 13, 14, 14, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 17, 17, 18, 18, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 25, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 30, 30, 30, 30};

    	//propagate binary knapsack
    	BinaryKnapsack bkp = new BinaryKnapsack(b,w,l);
        s.add(bkp, CPPropagStrength.Weak);
        s.add(bkp,CPPropagStrength.Strong);

        s.add(new Diff(b[0],0));
        assertFalse(s.isFailed());
    }



    public void testb() {
        Store cp = new Store();
        int n = 20;
        CPVarBool [] x = CPVarBool.getArray(cp,n);
        int [] values = new int[n];
        int [] values2 = new int[n];
        for (int i = 0; i < n; i++) {
            values[i] = i+1;
            values2[i] = values[i]*values[i];
        }
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Strong);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Strong);
        boolean [] sol = new boolean[]{true, false, false, true, false, false, true, true, true, true, false, true, false, false, false, true, false, true, false, true};
        for (int i = 0; i < sol.length; i++) {
            if (i == sol.length/2) {
                cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Weak);
                cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Weak);
                cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Strong);
                cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Strong);
            }

            if (sol[i]) {
                cp.add(x[i].constraintTrue());
            } else {
                cp.add(x[i].constraintFalse());
            }
        }
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Strong);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Strong);

        assertFalse(cp.isFailed());
    }

    public void testc() {
        Store cp = new Store();

        CPVarBool [] x = CPVarBool.getArray(cp,3);
        int [] values = new int[]{43,23,23};
        CPVarInt c = new CPVarInt(cp,1,82);

        cp.add(new BinaryKnapsack(x,values, c), CPPropagStrength.Strong);

        assertFalse(cp.isFailed());
        assertEquals(c.getMin(),23);
        assertEquals(c.getMax(),66);
    }




    
   
}

