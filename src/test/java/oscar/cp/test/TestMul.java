/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
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
public class TestMul extends TestCase {
	

	
    public TestMul(String name) {
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
    
    public void testMul1() {  
    	Store s = new Store();
    	CPVarInt x = CPVarInt.apply(s,0,5);
    	CPVarInt y = x.mul(-2);
    	s.post(new Gr(y,-10));
    	assertTrue(x.getMax()==4);
    }
    
    public void testMul2() { 
    	Store s = new Store();
    	CPVarInt x = CPVarInt.apply(s,2,5);
    	CPVarInt y = CPVarInt.apply(s,10,10);
    	s.post(new MulCte(x,3,y));
    	
    	assertTrue(s.isFailed());
    }
    
    public void testMul3() {  	
    	Store s = new Store();
    	CPVarInt x = CPVarInt.apply(s,2,5);
    	CPVarInt z = CPVarInt.apply(s,10,12);
    	s.post(new MulCte(x,3,z));
    	assertTrue(!s.isFailed());
    	assertTrue(x.isBound() && x.getValue()==4);
    }
    
    public void testMul4() { 
    	Store s = new Store();
    	CPVarInt x = CPVarInt.apply(s,2,5);
    	CPVarInt z = CPVarInt.apply(s,9,12);
    	s.post(new MulCte(x,3,z));
    	s.post(new LeEq(z,11));
    	assertTrue(!s.isFailed());
    	assertTrue(x.isBound() && x.getValue()==3);
    }
    
    public void testMul5() {  
    	Store s = new Store();
    	CPVarInt x = CPVarInt.apply(s,2,5);
    	CPVarInt y = CPVarInt.apply(s,10,10);
    	//s.post(new MulCte(x,3,y));
    	s.post(new WeightedSum(new int[]{3},new CPVarInt[]{x},y));
    	assertTrue(s.isFailed());
    	
   
    	assertTrue(true);
    }
    
    public void testMul6() {  
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,-1,4);
    	CPVarInt y = CPVarInt.apply(cp,-1,1);
    	CPVarInt z = CPVarInt.apply(cp,0,0);
    	
    	cp.post(new MulVar(x,y,z));
    	cp.post(new DiffVal(y,0));

    	assertTrue(!cp.isFailed());
    	assertTrue(x.isBound() && x.getValue()==0);
    }
    
    public void testMul7() {  
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,-1,4);
    	CPVarInt y = CPVarInt.apply(cp,-1,1);
    	CPVarInt z = CPVarInt.apply(cp,0,1);
    	
    	cp.post(new MulVar(x,y,z));
    	cp.post(new DiffVal(z,0));

    	assertTrue(!cp.isFailed());
    	assertTrue(!x.hasValue(0));
    	assertTrue(!y.hasValue(0));
    	assertTrue(x.getMin()>=-1 && x.getMax()<=1);
    	assertTrue(y.getMin()>=-1 && y.getMax()<=1);
    }
    
    public void testMul8() {  
    	
    	//could prune better x1

    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,-4,4);
    	CPVarInt y = CPVarInt.apply(cp,-1,1);
    	CPVarInt z = CPVarInt.apply(cp,-1,1);
    	
    	cp.post(new MulVar(x,y,z));
    	cp.post(new DiffVal(y,0));

    	//System.out.println(x+" "+y+" "+z);
    	//should prune better x since y!=0.
    	
    	//System.out.println(x.hasValue(0));

    }
    
    public void testMul9() {
    	//could prune better x1 and y1
    	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,-4,4);
    	CPVarInt y = CPVarInt.apply(cp,-1,1);
    	CPVarInt z = CPVarInt.apply(cp,-1,1);

    	cp.post(new MulVar(x,y,z));
    	cp.post(new DiffVal(z,0));
    }
    
    public void testMul10() {
    	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,0,4);
    	CPVarInt y = CPVarInt.apply(cp,-3,1);
    	CPVarInt z = CPVarInt.apply(cp,-1,1);
    	
    	cp.post(new MulVar(x,y,z));
    	cp.post(new LeEq(y,0));
    	
    	assertTrue(!z.hasValue(1));
    }
    
    public void testMul11() {
    	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,0,4);
    	CPVarInt y = CPVarInt.apply(cp,-3,0);
    	CPVarInt z = CPVarInt.apply(cp,0,2);
    	
    	cp.post(new MulVar(x,y,z));
    	
    	
    	assertTrue(x.getMax()==4);
    	assertTrue(x.getMin()==0);
    	assertTrue(z.getValue()==0);

    }
    
    public void testMul12() {
    	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,0,4);
    	CPVarInt y = CPVarInt.apply(cp,-3,0);
    	CPVarInt z = CPVarInt.apply(cp,0,2);
    	
    	cp.post(new MulVar(x,y,z));
    	cp.post(new LeEq(y,-1));

    	
    	assertTrue(x.getValue()==0);
    	assertTrue(z.getValue()==0);

    }
    
    public void testMul13() {
    	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,1,4);
    	CPVarInt y = CPVarInt.apply(cp,-3,0);
    	CPVarInt z = CPVarInt.apply(cp,-2,2);
    	
    	cp.post(new MulVar(x,y,z));
    	cp.post(new LeEq(y,-1));
    	
    	assertTrue(x.getMax()==2);
    	assertTrue(y.getMin()==-2);
    	assertTrue(z.getMax()==-1);

    }
    
    public void testMul14() {
    	
    	Store cp = new Store();
    	CPVarInt s = CPVarInt.apply(cp,1,30);
    	CPVarInt nb = CPVarInt.apply(cp,10506,19596);
    	CPVarInt tmp = CPVarInt.apply(cp,351,900);
    	
    	cp.post(new MulVar(tmp,s,nb));
    	cp.post(new MulVar(s,s,tmp));
    }
    
    public void testMul15() {
    	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,1, 10);
    	CPVarInt y = CPVarInt.apply(cp,new int[]{50,70});
    	CPVarInt z = CPVarInt.apply(cp,100,100);
    	
    	cp.post(new MulVar(x,y,z));
    	
    	
    	assertTrue(x.isBoundTo(2));
    	assertTrue(y.isBoundTo(50));
    	
    }
    
    public void testMul16() {
    	
    	Store cp = new Store();
    	CPVarInt x = CPVarInt.apply(cp,0, 10);
    	CPVarInt y = CPVarInt.apply(cp,new int[]{50,70});
    	CPVarInt z = CPVarInt.apply(cp,100,100);
    	
    	cp.post(new MulVar(x,y,z));
    	
    	
    	assertTrue(x.isBoundTo(2));
    	assertTrue(y.isBoundTo(50));
    	
    }  
    
 
    
    public void testMul18() {  
    	Store s = new Store();
    	CPVarInt x = CPVarInt.apply(s,-5,5);
    	CPVarInt y = CPVarInt.apply(s,-5,16);
    	s.post(new Eq(x.mul(x), y)); // should detect it is a square constraint
    	assertTrue(!s.isFailed());
    	assertTrue(x.getMin() == -4);
    	assertTrue(x.getMax() == 4);
    	assertTrue(y.getMax() == 16);
    	assertTrue(y.getMin() == 0);
    }
    
    public void testMul19() {  
    	Store s = new Store();
    	

    	CPVarInt x = CPVarInt.apply(s,6,43986624);
    	CPVarInt y = CPVarInt.apply(s,4,355);
    	CPVarInt z = CPVarInt.apply(s,711,711);
    	//s.post(new MulVar(x, y, z));
    	System.out.println("hello");
    	z = x.mul(y);
    	System.out.println("z:"+z);
    	s.post(new Eq(z, CPVarInt.apply(s,711))); // should detect it is a square constraint
    	assertTrue(!s.isFailed());

    }
    



}
