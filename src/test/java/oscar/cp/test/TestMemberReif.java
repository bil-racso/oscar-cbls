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
public class TestMemberReif extends TestCase {
	
	private Store s;	
	
    public TestMemberReif(String name) {
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

    public void test0(){
        SetIndexedArray set = new SetIndexedArray(0,10,true);
        set.insert(1);
        set.insert(2);
        set.insert(4);
        //set1 = {1,2,4}
        CPVarInt x = new CPVarInt(s,0,5);
        //x = {0,1,2,3,4,5}
        CPVarBool b = new CPVarBool(s);
        assertFalse(b.isBound());
        s.post(b.constraintFalse()); // forbid x to be a member os set
        s.post(new MemberReif(x,set,b));
        assertFalse(s.isFailed());
        //check that x lost its values 1,2 and 4
        assertTrue(!x.hasValue(1) && !x.hasValue(2) && !x.hasValue(4));
    }

    public void test1(){
        SetIndexedArray set = new SetIndexedArray(0,10,true);
        set.insert(1);
        set.insert(2);
        set.insert(4);
        //set1 = {1,2,4}
        CPVarInt x = new CPVarInt(s,0,5);
        //x = {0,1,2,3,4,5}
        CPVarBool b = new CPVarBool(s);
        s.post(new MemberReif(x,set,b));
        assertFalse(b.isBound());
        s.post(b.constraintFalse());  // forbid x to be a member os set
        assertFalse(s.isFailed());
        //check that x lost its values 1,2 and 4
        assertTrue(!x.hasValue(1) && !x.hasValue(2) && !x.hasValue(4));
    }

    public void test2(){
        SetIndexedArray set = new SetIndexedArray(0,10,true);
        set.insert(1);
        set.insert(2);
        set.insert(4);
        //set1 = {1,2,4}
        CPVarInt x = new CPVarInt(s,0,5);
        //x = {0,1,2,3,4,5}
        CPVarBool b = new CPVarBool(s);
        assertFalse(b.isBound());
        s.post(b.constraintTrue());  //force x to be a member of s
        s.post(new MemberReif(x,set,b));
        assertFalse(s.isFailed());
        //check that x has only values 1,2 and 4
        assertTrue(x.hasValue(1) && x.hasValue(2) && x.hasValue(4) && x.getSize()==3);
    }

    public void test3(){
        SetIndexedArray set = new SetIndexedArray(0,10,true);
        set.insert(1);
        set.insert(2);
        set.insert(4);
        //set1 = {1,2,4}
        CPVarInt x = new CPVarInt(s,0,5);
        //x = {0,1,2,3,4,5}
        CPVarBool b = new CPVarBool(s);
        assertFalse(b.isBound());
        s.post(new MemberReif(x,set,b));
        s.post(b.constraintTrue());  //force x to be a member of s
        assertFalse(s.isFailed());
        //check that x has only values 1,2 and 4
        assertTrue(x.hasValue(1) && x.hasValue(2) && x.hasValue(4) && x.getSize()==3);
    }

    public void test4(){
        SetIndexedArray set = new SetIndexedArray(0,10,true);
        set.insert(1);
        set.insert(2);
        set.insert(3);
        //set1 = {1,2,3}
        CPVarInt x = new CPVarInt(s,1,5);
        //x = {1,2,3,4,5}
        CPVarBool b = new CPVarBool(s);
        assertFalse(b.isBound());
        s.post(new MemberReif(x,set,b));
        s.post(new LeEq(x,3));  //force D(x) = 1,2,3 so that x is always a member
        assertFalse(s.isFailed());
        assertTrue(b.isTrue());
    }

    public void test5(){
        SetIndexedArray set = new SetIndexedArray(0,10,true);
        set.insert(1);
        set.insert(2);
        set.insert(3);
        //set1 = {1,2,3}
        CPVarInt x = new CPVarInt(s,1,5);
        //x = {1,2,3,4,5}
        CPVarBool b = new CPVarBool(s);
        assertFalse(b.isBound());
        s.post(new LeEq(x,3));  //force D(x) = 1,2,3 so that x is always a member
        s.post(new MemberReif(x,set,b));
        assertFalse(s.isFailed());
        assertTrue(b.isTrue());
    }

    public void test6(){
        SetIndexedArray set = new SetIndexedArray(0,10,true);
        set.insert(1);
        set.insert(2);
        set.insert(3);
        //set1 = {1,2,3}
        CPVarInt x = new CPVarInt(s,1,5);
        //x = {1,2,3,4,5}
        CPVarBool b = new CPVarBool(s);
        assertFalse(b.isBound());
        s.post(new GrEq(x,4));  //force D(x) = 4,5 so that x is not a member
        s.post(new MemberReif(x,set,b));
        assertFalse(s.isFailed());
        assertTrue(b.isFalse());
    }

    public void test7(){
        SetIndexedArray set = new SetIndexedArray(0,10,true);
        set.insert(1);
        set.insert(2);
        set.insert(3);
        //set1 = {1,2,3}
        CPVarInt x = new CPVarInt(s,1,5);
        //x = {1,2,3,4,5}
        CPVarBool b = new CPVarBool(s);
        assertFalse(b.isBound());
        s.post(new MemberReif(x,set,b));
        s.post(new GrEq(x,4));  //force D(x) = 4,5 so that x is not a member
        assertFalse(s.isFailed());
        assertTrue(b.isFalse());
    }

}
