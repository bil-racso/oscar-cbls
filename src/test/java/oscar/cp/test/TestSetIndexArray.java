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


import java.util.Arrays;
import java.util.Vector;

import oscar.cp.constraints.*;
import oscar.cp.core.*;
import oscar.cp.search.*;
import oscar.cp.util.*;
import oscar.reversible.*;
import oscar.search.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestSetIndexArray extends TestCase {

	private Store s;


    /**
     * Constructor for TestTrailInt.
     * @param name
     */
    public TestSetIndexArray(String name) {
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


    
    public void test1(){
    	SetIndexedArray set = new SetIndexedArray(0,10,true);
        assertTrue(set.getSize() == 0);
        for (int v: set) {
            assertTrue(false);
        }
        set.insert(1);
        int [] valArray = set.getSortedVals();
        assertTrue(valArray.length == 1);
        assertTrue(valArray[0] == 1);

        set.insert(2);
        set.insert(4);
        valArray = set.getSortedVals();
        System.out.println(Arrays.toString(valArray));
        assertTrue(valArray[0] == 1);
        assertTrue(valArray[1] == 2);
        assertTrue(valArray[2] == 4);
        assertFalse(set.hasValue(0));
        assertFalse(set.hasValue(3));
        assertFalse(set.hasValue(5));


    }

}
