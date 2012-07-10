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
import oscar.cp.search.*;
import oscar.cp.util.*;
import oscar.reversible.*;
import oscar.search.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestMagicSquare extends TestCase {

    public TestMagicSquare(String name) {
        super(name);
        
    }


    public void testMagicSquare() {
    	
    	Store cp = new Store();
    	
    	int n = 3;
    	
    	// declare variables
		
    	CPVarInt [][] x = CPVarInt.getArray(cp,n,n,1,n*n);
		
		
		//add constraints
		
		cp.add(new AllDifferent(ArrayUtils.flattenvars(x)),CPPropagStrength.Weak);
		
		int s = ( n * (n*n + 1)) / 2;
		
		CPVarInt [] diag1 = new CPVarInt[n];
		CPVarInt [] diag2 = new CPVarInt[n];
		for (int i = 0; i < n; i++) {
			diag1[i] =  x[i][i];
			diag2[i] =  x[i][n-i-1];
		}
		// same sum for diagonals
		cp.add(new Sum(diag1, s));
		cp.add(new Sum(diag2, s));
		// same sum for lines and columns
		for (int i = 0; i < n; i++) {
			cp.add(new Sum(x[i],s));
			cp.add(new Sum(ArrayUtils.getSlice(x, i),s));
		}
	
		//   -----------------  search -----------------------
		final Counter nbSol = new Counter();
		Search search = new Search(cp,new NaryFirstFail(ArrayUtils.flattenvars(x)));
		search.addSolutionObserver(new SolutionObserver() {			
			public void solutionFound() {
				nbSol.incr();
			}
		});

		search.solveAll();
		
		// -------------- test the number of sol is 8 for the 3x3 magic square ---------
		assertEquals(nbSol.getValue(), 8);
    }
    
  
    
    
    

}

