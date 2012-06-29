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
public class TestQueens extends TestCase {

    public TestQueens(String name) {
        super(name);
        
    }


    public void test1() {
        assertEquals(testQueens(7,CPPropagStrength.Weak),40); // test if there's well 40 sols to the 7-queens problem
    }

    public void test2() {
        assertEquals(testQueens(7,CPPropagStrength.Strong),40); // test if there's well 40 sols to the 7-queens problem
    }

    /**
     *
     * @param n the number of queens
     * @param strength the propagation strength to use in alldiff constraints
     * @return the number of solutions to the n-queens problem
     */
    public int testQueens(int n,CPPropagStrength strength){
        final Counter nbSol = new Counter();
    	Store cp = new Store();
	    CPVarInt [] x = CPVarInt.getArray(cp,n,0,n-1);
		CPVarInt [] dia1 = new CPVarInt[n];
		CPVarInt [] dia2 = new CPVarInt[n];
		for (int i = 0; i < n; i++) {
			dia1[i] = x[i].minus(i);
			dia2[i] = x[i].plus(i);
		}

        cp.post(new AllDifferent(x),strength);
		cp.post(new AllDifferent(dia1),strength);
		cp.post(new AllDifferent(dia2),strength);

        Search search = new Search(cp,new Binary(x));

		search.addSolutionObserver(new SolutionObserver() {
            public void solutionFound() {
                nbSol.incr();
            }
        });
		search.solveAll();
        return nbSol.getValue();
    }
    

}

