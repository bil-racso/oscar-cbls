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
public class TestLexLeq extends TestCase {
	
	private Store s;	
	
    public TestLexLeq(String name) {
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
    
    public void testLexLeq(){ 
    	CPVarInt [] x = CPVarInt.getArray(s, 5, 0, 1);
    	CPVarInt [] y = CPVarInt.getArray(s, 5, 0, 1);
    	
    	
    	
    	s.post(new LexLeq(x,y));
    	
    	
    	
    	s.post(new Eq(y[0],0));  	
    	s.post(new Eq(y[1],0));
    	s.post(new Eq(x[2],1));
    	
    	
    	assertTrue(!s.isFailed());
    	assertTrue(x[0].getValue()==0);
    	assertTrue(x[1].getValue()==0);
    	assertTrue(y[2].getValue()==1);
    }
    
    
}
