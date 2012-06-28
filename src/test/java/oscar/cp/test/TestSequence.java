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
public class TestSequence extends TestCase {

	private Store s;

    public TestSequence(String name) {
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
    
    public void testSequence1(){
    	CPVarInt [] x = new CPVarInt[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = new CPVarInt(s,0,1);
		}
        SetIndexedArray set = new SetIndexedArray(1,1);
    
        s.add(new Eq(x[2], 0));
        s.add(new Sequence(x, set, 2, 1, 2));

        assertTrue(x[3].isBound());
        assertTrue(x[1].isBound());
        assertEquals(x[1].getValue(),1);
        assertEquals(x[3].getValue(),1);
        assertFalse(s.isFailed());
    }

    public void testSequence2(){
    	CPVarInt [] x = new CPVarInt[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = new CPVarInt(s,1,5);
		}
        SetIndexedArray set = new SetIndexedArray(3,3);

        s.add(new Eq(x[2], 2));
        s.add(new Sequence(x, set, 2, 1, 2));

        assertTrue(x[3].isBound());
        assertTrue(x[1].isBound());
        assertEquals(x[1].getValue(),3);
        assertEquals(x[3].getValue(),3);
        assertFalse(s.isFailed());
    }

    public void testSequence3(){
    	CPVarInt [] x = new CPVarInt[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = new CPVarInt(s,1,5);
		}
        SetIndexedArray set = new SetIndexedArray(2,3);

        s.add(new Eq(x[2], 1));
        s.add(new Sequence(x, set, 2, 1, 2));

        assertTrue(x[1].getSize() == 2 && x[1].hasValue(2) && x[1].hasValue(3));
        assertTrue(x[3].getSize() == 2 && x[3].hasValue(2) && x[3].hasValue(3));
        assertFalse(s.isFailed());
    }

    public void testSequence4(){
    	CPVarInt [] x = new CPVarInt[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = new CPVarInt(s,1,5);
		}
        SetIndexedArray set = new SetIndexedArray(4,4);

        s.add(new Eq(x[2], 1));
        s.post(new Sequence(x, set, 1, 1, 1));

        assertTrue(s.isFailed());
    }
    
    
}
