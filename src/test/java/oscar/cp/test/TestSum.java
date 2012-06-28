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
import java.util.Random;

import oscar.cp.constraints.*;
import oscar.cp.core.*;
import oscar.cp.search.*;
import oscar.cp.util.*;
import oscar.reversible.*;
import oscar.search.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestSum extends TestCase {
	
	private Store s;	
	
    public TestSum(String name) {
        super(name);
    }
    	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();
        s = new Store();
    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        s = null;
    }
    
    public void testSum1() {  	
    	CPVarInt x0 = new CPVarInt(s,0,1);
    	CPVarInt x1 = new CPVarInt(s,0,2);
    	CPVarInt x2 = new CPVarInt(s,0,2);
    	CPVarInt x3 = new CPVarInt(s,0,3);
    	
    	CPVarInt [] cumulatedCounters = new CPVarInt[]{x0,x1,x2,x3};
    	int n = cumulatedCounters.length;
    	int len = 2;
    	int min = 1;
    	int max = 2;
    	
    	// 0-1 , 0-2 , 0-2 , 0-3
    	
    	CPVarInt [] nb = new CPVarInt[3];
    	
        for (int i = 0; i <= n-len; i++) {
            CPVarInt nbVal = cumulatedCounters[i+len-1];
            if (i > 0) {
            	nbVal = nbVal.minus(cumulatedCounters[i-1]);
            }
            nb[i] = nbVal;
            
            if (s.post(new LeEq(nbVal,max)) == CPOutcome.Failure) {
                assertFalse(true);
            }
            if (s.post(new GrEq(nbVal,min)) == CPOutcome.Failure) {
                assertFalse(true);
            }
            assertFalse(s.isFailed());
            
        }
        System.out.println(Arrays.toString(cumulatedCounters));
        System.out.println(Arrays.toString(nb));
    }
    

}
