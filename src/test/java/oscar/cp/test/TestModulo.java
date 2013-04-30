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

import oscar.cp.constraints.*;
import oscar.cp.core.*;
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
    	CPVarInt x = CPVarInt.apply(cp,new int[]{0,3,9,12});
    	CPVarInt y = CPVarInt.apply(cp, 0,5);
    	cp.post(new Modulo(x, 3, y));
    	assertTrue(y.isBound());
    	assertTrue(y.getValue() == 0);
    }
    
    public void test2(){  	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,new int[]{0,1,6,9,12});
    	CPVarInt y = CPVarInt.apply(cp, 0,5);
    	cp.post(new Modulo(x, 3, y));
    	assertTrue(y.getSize() == 2);
    	cp.post(new DiffVal(x, 1));
    	assertTrue(y.isBound());
    	assertTrue(y.getValue() == 0);
    }
    
    public void test3(){  	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,new int[]{0,1,6,9,12});
    	CPVarInt y = CPVarInt.apply(cp, 0,5);
    	cp.post(new Modulo(x, 3, y));
    	cp.post(new DiffVal(y, 0));
    	assertTrue(x.isBound());
    	assertTrue(x.getValue() == 1);
    }
    
    public void test4(){  	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,new int[]{0,1,6,2,9,12});
    	CPVarInt y = CPVarInt.apply(cp, 0,5);
    	cp.post(new Modulo(x, 3, y));
    	cp.post(new DiffVal(y, 0));
    	cp.post(new DiffVal(y, 2));
    	assertTrue(x.isBound());
    	assertTrue(x.getValue() == 1);
    	assertTrue(y.getValue() == 1);
    }
    
    public void test5(){  	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,new int[]{0,-1,-6,-2,-9,-12});
    	CPVarInt y = CPVarInt.apply(cp, -5,5);
    	cp.post(new Modulo(x, 3, y));
    	cp.post(new DiffVal(y, 0));
    	cp.post(new DiffVal(y, -2));
    	assertTrue(x.isBound());
    	assertTrue(x.getValue() == -1);
    	assertTrue(y.getValue() == -1);
    }
    
    public void test6(){  	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,new int[]{-6,-3,-9,-12,3,6,9,12});
    	CPVarInt y = CPVarInt.apply(cp, -5,5);
    	cp.post(new Modulo(x, 3, y));
    	assertTrue(y.getValue() == 0);
    }

}
