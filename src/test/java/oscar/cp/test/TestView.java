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
public class TestView extends TestCase {
	
	private Store s;	
	
    public TestView(String name) {
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
    
    public void testView(){  	
    	CPVarInt x = new CPVarInt(s,1,5,"x");
    	CPVarInt y = new CPVarInt(s,1,5);
        System.out.println(x);

        CPVarBool b = new CPVarBool(s);


    	CPVarInt x1 = x.plus(0);
    	CPVarInt x2 = x1.plus(y);
    	
    	CPVarInt x3 = x.plus(4);
    	
//    	for(Integer v: x) {
//    		System.out.println(v);
//    	}
    	
    	assertTrue(s.getStatus()!= CPOutcome.Failure);


    }

}
