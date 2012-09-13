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
    	CPVarInt x = CPVarInt.apply(s,1,5);
    	CPVarInt y = CPVarInt.apply(s,1,5);
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
