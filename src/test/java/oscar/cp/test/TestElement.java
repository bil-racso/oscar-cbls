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
public class TestElement extends TestCase {
	
	private Store s;	
	
    public TestElement(String name) {
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
    
    public void testElem1() {  	    	
    	
    	CPVarInt x1 = new CPVarInt(s,5,9);
    	CPVarInt x2 = new CPVarInt(s,17,18);
    	CPVarInt [] x = new CPVarInt[]{x1,x2};
    	
    	CPVarInt ind = new CPVarInt(s,0,1);
    	
    	CPVarInt res = Element.get(x, ind);
    	
    	s.post(new LeEq(res, 10));
    	
    	assertTrue(ind.isBound() && ind.getValue()==0);
    	
    	
    }
    
    

}
