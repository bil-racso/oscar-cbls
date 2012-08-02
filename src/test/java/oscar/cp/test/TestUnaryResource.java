/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *  
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
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
//    	CPVarInt start1 = CPVarInt.apply(s,0,5);
//    	CPVarInt start2 = CPVarInt.apply(s,2,10);
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
