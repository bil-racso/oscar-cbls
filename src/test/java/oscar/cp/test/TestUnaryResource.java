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
import oscar.cp.scheduling.Activity;
import oscar.cp.search.*;
import oscar.cp.util.*;
import oscar.reversible.*;
import oscar.search.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestUnaryResource extends TestCase {
	
	private Store s;	
	
    public TestUnaryResource(String name) {
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
    
    public void testRank() {  	
//    	CPVarInt start1 = new CPVarInt(s,0,5);
//    	CPVarInt start2 = new CPVarInt(s,2,10);
//    	
//    	Activity act1 = new Activity(start1, 5);
//    	Activity act2 = new Activity(start2, 5);
//    	
//    	s.post(new LeEq(act1.getEnd(), act2.getStart()));  
//    	UnaryResource r = new UnaryResource(new Activity[]{act1,act2});
//    	s.post(r);
//    	
//    	assertTrue(r.getRanks()[0].isBound());
//    	assertTrue(r.getRanks()[0].getValue()==0);
//    	assertTrue(r.getRanks()[1].isBound());
//    	assertTrue(r.getRanks()[1].getValue()==1);
    	
    }
    
    

}
