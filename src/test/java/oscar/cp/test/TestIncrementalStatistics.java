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

import oscar.cp.constraints.*;
import oscar.cp.core.*;
import oscar.cp.search.*;
import oscar.cp.util.*;
import oscar.reversible.*;
import oscar.search.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestIncrementalStatistics extends TestCase {
	
	
	
    public TestIncrementalStatistics(String name) {
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
    
    public static double getAverage(double [] vals) {
    	double sum = ArrayUtils.sum(vals);
    	return sum/vals.length;
    }
    
    public static double getVariance(double [] vals) {
    	double res = 0;
    	double avg = getAverage(vals);
    	for (double v : vals) {
    		res += (v-avg)*(v-avg);
    	}
    	return res/vals.length;
    }
    
    public static int round(double a) {
    	return (int)(1000*a);
    }

    public void test0(){  	
    	double [] vals = new double[]{-3.5};
    	IncrementalStatistics stat = new IncrementalStatistics();
    	for (double v : vals) {
    		stat.addPoint(v);
    	}
    	assertEquals(round(stat.getAverage()),round(getAverage(vals)));
    	assertEquals(round(stat.getVariance()),round(getVariance(vals)));
    }
    
    public void test2(){  	
    	double [] vals = new double[]{-3,-2,2,3,9,10};
    	IncrementalStatistics stat = new IncrementalStatistics();
    	for (double v : vals) {
    		stat.addPoint(v);
    	}
    	assertEquals(round(stat.getAverage()),round(getAverage(vals)));
    	assertEquals(round(stat.getVariance()),round(getVariance(vals)));
    }
    
    
}
