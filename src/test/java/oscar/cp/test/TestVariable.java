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
public class TestVariable extends TestCase {

	private Store s;
	private CPVarInt a;
	private CPVarInt b;
	private CPVarInt c;


	public TestVariable(String name) {
		super(name);

	}

	/**
	 * setUp() method that initializes common objects
	 */
	protected void setUp() throws Exception {
		super.setUp();
		s = new Store();
		a = CPVarInt.apply(s,1,6);
		b = CPVarInt.apply(s,0,31);
		c = CPVarInt.apply(s,1,10);
	}

	/**
	 * tearDown() method that cleanup the common objects
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
		s = null;
		a = null;
		b = null;
		c = null;
	}




	public void testa(){


		assertTrue(true);

		a.removeValue(2);
		a.removeValue(4);

		s.pushState();

		a.removeValue(2);
		a.removeValue(10);
		a.removeValue(6);

		s.pushState();

		a.removeValue(-2);
		a.removeValue(4);
		a.removeValue(1);
		a.removeValue(4);

		s.pop();

		a.removeValue(1);
		a.removeValue(0);

		s.pop();

		//[1, 3, 5, 6] card:4 min:1 max:6
		assertTrue(a.getMin()==1);
		assertTrue(a.getMax()==6);
		assertTrue(a.getSize()==4);
		assertTrue(a.hasValue(1));
		assertTrue(a.hasValue(3));
		assertTrue(a.hasValue(5));
		assertTrue(a.hasValue(6));
		assertTrue(!a.hasValue(2));

	}

	public void testb(){

		b.assign(0);
		assertTrue(b.isBound() && b.getValue()==0);
	}

	public void testc(){

		c.removeValue(5);
		c.updateMax(7);
		//1..4,6..7
		int i = 0;
		for (int v = c.min(); v < c.max(); v++) {
			if (c.hasValue(v)) {
				switch (i) {
				case 0:
					assertTrue(v == 1);
					break;
				case 1:
					assertTrue(v == 2);
					break;
				case 2:
					assertTrue(v == 3);
					break;
				case 3:
					assertTrue(v == 4);
					break;
				case 4:
					assertTrue(v == 6);
					break;
				case 5:
					assertTrue(v == 7);
					break;
				default:
					break;
				}
				i++;
			}
		}
	}


	public void testd(){
		Store cp = new Store();
		CPVarInt x = CPVarInt.apply(cp,1,6);
		assertTrue(x.valueAfter(5) == 6);
		assertTrue(x.valueAfter(-10) == 1);
		
		int v  = x.valueAfter(6);
		assertTrue(v == 6);


		CPVarInt y = CPVarInt.apply(cp,-100,100);
		y.removeValue(0);


		assertTrue(y.valueAfter(-1) == 1);
		assertTrue(y.valueAfter(0) == 1);
		assertTrue(y.valueBefore(-1) == -2);
		assertTrue(y.valueBefore(1) == -1);
		assertTrue(y.valueBefore(0) == -1);
		assertTrue(y.valueBefore(1000) == 100);

		y.removeValue(30);
		y.removeValue(31);
		y.removeValue(32);
		y.removeValue(33);

		assertTrue(y.valueBefore(31) == 29);
		assertTrue(y.valueBefore(34) == 29);
		assertTrue(y.valueAfter(31) == 34);
		assertTrue(y.valueAfter(34) == 35);     

	}


	public void teste(){
		Store cp = new Store();
		int [] freq = new int[4]; 
		CPVarInt x = CPVarInt.apply(cp,new int[]{0,1,2,3});
		for (int i = 0; i < 200; i++) {
			freq[x.randomValue()]++;
		}
		for (int i = 0; i < 4; i++) {
			assertTrue(freq[i]>0);
		}
		System.out.println(Arrays.toString(freq));
	}

	public void testf(){
		Store cp = new Store();
		CPVarInt x = CPVarInt.apply(cp,new int[]{1,5,9,10});
		CPVarInt y = CPVarInt.apply(cp,new int[]{5,9,11});
		CPVarInt z = CPVarInt.apply(cp,new int[]{6,7,11});
		CPVarInt w = CPVarInt.apply(cp, 14);
		
		assertTrue(x.intersectionSize(y) == 2);
		
		assertTrue(y.intersectionSize(x) == 2);

		assertTrue(z.intersectionSize(y) == 1);
		assertTrue(y.intersectionSize(z) == 1);

		assertTrue(w.intersectionSize(x) == 0);
		assertTrue(x.intersectionSize(w) == 0);
	}


}

