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

import java.util.ArrayList;
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
public class TestGolomb extends TestCase {

    public TestGolomb(String name) {
        super(name);
        
    }

    /**
     * Test the optimal sol of golomb ruler
     */
    public int testGolomb(int n) {
	
    	
		//   -----------------  model -----------------------
    	Store cp = new Store();
    	final CPVarInt [] marks = CPVarInt.getArray(cp, n, 0, n*n);
		
		// we break symmetries to put the marks increasing
		cp.add(new Eq(marks[0], 0));
		for (int i = 0; i < n-1; i++) {
			cp.add(new Le(marks[i], marks[i+1]));
			
		}
		
		ArrayList<CPVarInt> differences = new ArrayList<CPVarInt>();
		for (int i = 0; i < n; i++) {
			for (int j = i+1; j < n; j++) {
				differences.add(marks[j].minus(marks[i]));
			}
		}
		cp.add(new AllDifferent(differences.toArray(new CPVarInt[0])),CPPropagStrength.Strong);
		
		
		// break the symmetries between differences
		// marks[1] - marks[0] < marks[n-1] - marks[n-2]
		cp.add(new Le(marks[1].minus(marks[0]), marks[n-1].minus(marks[n-2])));
		
		final CPVarInt obj = marks[n-1];
		cp.minimization(obj);
    	
		//   -----------------  search -----------------------
		final Int best = new Int(Integer.MAX_VALUE);
		Search search = new Search(cp,new NaryFirstFail(marks));
		search.addSolutionObserver(new SolutionObserver() {			
			public void solutionFound() {
				best.setValue(obj.getValue());
			}
		});

		search.solveAll();
		return best.getValue();
    }
    
    public void test6() {
    	assertEquals(testGolomb(6), 17);
    }
    
    public void test7() {
    	assertEquals(testGolomb(7), 25);
    }
    
    
    

}

