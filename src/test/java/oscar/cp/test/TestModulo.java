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
public class TestModulo extends TestCase {
	
	
	
    public TestModulo(String name) {
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
    
    public void test1(){  	
    	Store cp = new Store();
    	CPVarInt x = new CPVarInt(cp,0,3,9,12);
    	CPVarInt y = new CPVarInt(cp, 0,5);
    	cp.post(new Modulo(x, 3, y));
    	assertTrue(y.isBound());
    	assertTrue(y.getValue() == 0);
    }
    
    public void test2(){  	
    	Store cp = new Store();
    	CPVarInt x = new CPVarInt(cp,0,1,6,9,12);
    	CPVarInt y = new CPVarInt(cp, 0,5);
    	cp.post(new Modulo(x, 3, y));
    	assertTrue(y.getSize() == 2);
    	cp.post(new Diff(x, 1));
    	assertTrue(y.isBound());
    	assertTrue(y.getValue() == 0);
    }
    
    public void test3(){  	
    	Store cp = new Store();
    	CPVarInt x = new CPVarInt(cp,0,1,6,9,12);
    	CPVarInt y = new CPVarInt(cp, 0,5);
    	cp.post(new Modulo(x, 3, y));
    	cp.post(new Diff(y, 0));
    	assertTrue(x.isBound());
    	assertTrue(x.getValue() == 1);
    }
    
    public void test4(){  	
    	Store cp = new Store();
    	CPVarInt x = new CPVarInt(cp,0,1,6,2,9,12);
    	CPVarInt y = new CPVarInt(cp, 0,5);
    	cp.post(new Modulo(x, 3, y));
    	cp.post(new Diff(y, 0));
    	cp.post(new Diff(y, 2));
    	assertTrue(x.isBound());
    	assertTrue(x.getValue() == 1);
    	assertTrue(y.getValue() == 1);
    }
    
    public void test5(){  	
    	Store cp = new Store();
    	CPVarInt x = new CPVarInt(cp,0,-1,-6,-2,-9,-12);
    	CPVarInt y = new CPVarInt(cp, -5,5);
    	cp.post(new Modulo(x, 3, y));
    	cp.post(new Diff(y, 0));
    	cp.post(new Diff(y, -2));
    	assertTrue(x.isBound());
    	assertTrue(x.getValue() == -1);
    	assertTrue(y.getValue() == -1);
    }
    
    public void test6(){  	
    	Store cp = new Store();
    	CPVarInt x = new CPVarInt(cp,-6,-3,-9,-12,3,6,9,12);
    	CPVarInt y = new CPVarInt(cp, -5,5);
    	cp.post(new Modulo(x, 3, y));
    	assertTrue(y.getValue() == 0);
    }

}
