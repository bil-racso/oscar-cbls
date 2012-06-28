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
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;
import oscar.cp.util.ArrayUtils;

import junit.framework.TestCase;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestArrayUtils extends TestCase {



    public TestArrayUtils(String name) {
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
    
    public void testSort() {
    	Integer [] vals1 = new Integer[]{1,3,6,1,3};
        int [] vals2 = new int[]{1,3,6,1,3};
        ArrayUtils.sort(vals1,vals2);
        for (int i = 0; i < vals2.length-1; i++) {
            assertTrue( vals1[i] <= vals1[i+1]);
        }
    }
    
    public void testGetRandomVar1() {
    	Store cp = new Store();
    	int [] freq = new int[5]; 
        CPVarInt [] x = new CPVarInt[]{new CPVarInt(cp,0,1,2,3),new CPVarInt(cp,2),new CPVarInt(cp,0,1,2,3),new CPVarInt(cp,3),new CPVarInt(cp,1,3,9)};
        for (int i = 0; i < 600; i++) {
        	freq[ArrayUtils.getRandomNotBound(x)]++;
        }
        assertTrue(freq[0] > 100);
        assertTrue(freq[1] == 0);
        assertTrue(freq[2] > 100);
        assertTrue(freq[3] == 0);
        assertTrue(freq[4] > 100);      
    }
    
    public void testGetRandomVar2() {
    	Store cp = new Store();
    	int [] freq = new int[5]; 
        CPVarInt [] x = new CPVarInt[]{new CPVarInt(cp,3),new CPVarInt(cp,2),new CPVarInt(cp,2),new CPVarInt(cp,3),new CPVarInt(cp,9)};
        
        assertTrue(ArrayUtils.getRandomNotBound(x) ==  -1);     
    }

}
