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
import oscar.cp.search.*;
import oscar.cp.util.*;
import oscar.reversible.*;
import oscar.search.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestStretch extends TestCase {
	
	private Store s;	
	
    public TestStretch(String name) {
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
    
    public void testStretch(){ 
    	CPVarInt [] x = new CPVarInt[8];
    	for (int i = 0; i < x.length; i++) {
			x[i] = new CPVarInt(s,0,2);
		}
    	int [] shortest = new int[] {2,2,2};
    	int [] longest = new int[] {4,3,2};
    	
    	Automaton automaton = Stretch.getStretchAutomaton(x, shortest, longest);
    	s.post(new Regular(x,automaton));
    	s.post(new Eq(x[0],0));
    	assertTrue(x[1].isBound() && x[1].getValue()==0);
    	s.post(new Eq(x[1],0));
    	s.post(new Eq(x[2],0));
    	s.post(new Eq(x[3],0));
    	assertTrue(!x[4].hasValue(0));
    }
    
    
}
