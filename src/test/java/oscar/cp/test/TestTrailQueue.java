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
public class TestTrailQueue extends TestCase {
	
	private Store s;
	private ReversibleQueue<Integer> a;
	private ReversibleQueue<String>  b;
	

    public TestTrailQueue(String name) {
        super(name);
    }
    
	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();
        s = new Store();
        a = new ReversibleQueue<Integer>(s);
        b = new ReversibleQueue<String>(s);
    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        s = null;
        a = null;
        b = null;
    }
    
    public void testEquals(){
    	
    	
    	assertTrue(a.getValue()==null);
    	assertTrue(b.getValue()==null);
    	
    	//a = null, b = null
    	s.pushState();
    	a.setValue(new Queue<Integer>(a.getValue(),1));
    	a.setValue(new Queue<Integer>(a.getValue(),2));
    	a.setValue(new Queue<Integer>(a.getValue(),3));
    	b.setValue(new Queue<String>(b.getValue(),"a"));
    	b.setValue(new Queue<String>(b.getValue(),"b"));
    	b.setValue(new Queue<String>(b.getValue(),"c"));
    	
    	    	
    	//a = 3->2->1    b = c->b->a
    	s.pushState();
    	b.setValue(new Queue<String>(b.getValue(),"d"));
    	  	
    	//a = 3->2->1    b = d->c->b->a
    	s.pushState();
    	a.setValue(new Queue<Integer>(a.getValue(),4));
    	a.setValue(new Queue<Integer>(a.getValue(),5));
    	
    	//a = 5->4->3->2->1    b= d->c->b->a
    	s.pushState();
    	  	
    	s.pop();
    	assertTrue(a.getValue().toString().equals("5->4->3->2->1"));
    	assertTrue(b.getValue().toString().equals("d->c->b->a"));
    	
 	
    	s.pop();  
    	assertTrue(a.getValue().toString().equals("3->2->1"));
    	assertTrue(b.getValue().toString().equals("d->c->b->a"));
    	
    	s.pop();  
    	assertTrue(a.getValue().toString().equals("3->2->1"));
    	assertTrue(b.getValue().toString().equals("c->b->a"));
    	
    	s.pop();  
    	assertTrue(a.getValue()==null);
    	assertTrue(b.getValue()==null); 	
    	
    }


}
