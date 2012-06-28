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
public class TestTrailBoundedSet extends TestCase {
	
	private Store s;
	
	
    /**
     * Constructor for TestTrailInt.
     * @param name
     */
    public TestTrailBoundedSet(String name) {
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
    
    public void testEquals(){
    	
    	//a = null, b = null
    	s.pushState();
    	
    	ReversibleBoundedSet set = new ReversibleBoundedSet(s,10);
    	
    	s.pushState();
    	
    	set.insert(5);
    	set.insert(10);
    	set.insert(3);
    	set.remove(10);
    	
    	s.pushState();
    	
    	set.insert(6);
    	
    	assertTrue(set.getSize() == 3);
    	
    	set.remove(5);
    	
    	s.pop();
    	
    	
    	assertTrue(set.contains(3));
    	assertTrue(set.contains(5));
    	assertTrue(set.getSize()==2);

    }

}
