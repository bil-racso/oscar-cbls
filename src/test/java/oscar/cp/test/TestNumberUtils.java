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
public class TestNumberUtils extends TestCase {



    public TestNumberUtils(String name) {
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
    
    private boolean isPerfectSquare(int v) {
    	int r = (int) Math.sqrt(v);
    	return r*r == v;
    }
    
    public void testPerfectSquare() {
    	assertTrue(NumberUtils.isPerfectSquare(8*8));
    	assertFalse(NumberUtils.isPerfectSquare(8*9));
    }
    
    public void testCeilDiv() {
    	assertEquals(NumberUtils.ceilDiv(7, 2),4);
    	assertEquals(NumberUtils.ceilDiv(-7, -2),4);
    	assertEquals(NumberUtils.ceilDiv(-7, 2),-3);
    	assertEquals(NumberUtils.ceilDiv(7, -2),-3);
    }
    
    
}
