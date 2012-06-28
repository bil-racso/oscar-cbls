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
public class TestTrailInt extends TestCase {
	
	private Store s;
	private ReversibleInt a;
	private ReversibleInt b;
	
    /**
     * Constructor for TestTrailInt.
     * @param name
     */
    public TestTrailInt(String name) {
        super(name);
        
    }
    	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();
        s = new Store();
        a = new ReversibleInt(s);
        b = new ReversibleInt(s);
    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        s = null;
        a = null;
        b = null;
    }
    
    public void testEquals(){
    	
    	
    	assertTrue(a.getValue() == 0);
    	assertTrue(b.getValue() == 0);
    	
    	//a = null, b = null
    	s.pushState();
    	
    	a.setValue(1);
    	a.setValue(2);
    	b.setValue(3);
    	b.setValue(2);
    	
    	//a = 2, b = 2
    	s.pushState();
    	
    	a.setValue(4);
    	b.setValue(6);
    	a.setValue(1);
    	b.setValue(1);

    	//a = 1, b = 1
    	s.pushState();
    	
       	a.setValue(9);
    	b.setValue(8);
    	a.setValue(2);
    	b.setValue(6);
    	
    	s.pop();
    	assertTrue(a.getValue() == 1);
    	assertTrue(b.getValue() == 1);
    	
    	s.pop();
    	assertTrue(a.getValue() == 2);
    	assertTrue(b.getValue() == 2);
    	
    	s.pop();
    	assertTrue(a.getValue() == 0);
    	assertTrue(b.getValue() == 0);

    }

}
