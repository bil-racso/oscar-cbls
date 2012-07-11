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
import junit.framework.TestCase;
import java.util.Arrays;

import oscar.cp.constraints.AllDifferent;
import oscar.cp.constraints.BinaryKnapsack;
import oscar.cp.constraints.Diff;
import oscar.cp.constraints.GCC;
import oscar.cp.constraints.MulVar;
import oscar.cp.constraints.Sequence;
import oscar.cp.constraints.Sum;
import oscar.cp.constraints.WeightedSum;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarBool;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;
import oscar.cp.search.*;
import oscar.cp.util.ArrayUtils;
import oscar.cp.util.Counter;
import oscar.reversible.SetIndexedArray;
import oscar.search.Search;
import oscar.search.SolutionObserver;



/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestCarSequencing extends TestCase {

    public TestCarSequencing(String name) {
        super(name);
        
    }

    /**
     * Test the number of solutions in the car sequencing problem
     */
    public void testCarSequencing(){
	
    	//   -----------------  data -----------------------
		int nbCars = 10;
		int nbConfigs = 6;
		int nbOptions = 5	;
		int [] lb = new int [] {1,2,1,2,1};
		int [] ub = new int [] {2,3,3,5,5};
		int [] demand = new int [] {1, 1, 2, 2, 2, 2};// demand for each config
		int [][] requires = new int [][]	{{1, 0, 1, 1, 0}, // nbConfigs x nbOptions
											{0, 0, 0, 1, 0},
											{0, 1, 0, 0, 1},
											{0, 1, 0, 1, 0},
											{1, 0, 1, 0, 0},
											{1, 1, 0, 0, 0}};	
		SetIndexedArray [] options = new SetIndexedArray [nbOptions];
		for (int o = 0; o < nbOptions; o++) {
			options[o] = new SetIndexedArray(0, nbConfigs,true);
			for (int c = 0; c < nbConfigs; c++) {
				if (requires[c][o] == 1) {
					options[o].insert(c);
				}
			}
		}
		//   -----------------  model -----------------------
        final Counter nbSol = new Counter();
    	Store cp = new Store();
    	

		CPVarInt [] line = CPVarInt.getArray(cp, nbCars, 0, nbConfigs-1);
		for (int o = 1; o < nbOptions; o++) {
            System.out.println(Arrays.toString(options[o].getSortedVals()));
			cp.add(new Sequence(line, options[o], ub[o], 0, lb[o]));
		}
		cp.add(new GCC(line, 0, new int[nbConfigs], demand));
    	
		//   -----------------  search -----------------------
		Search search = new Search(cp,new NaryFirstFail(line));
		search.addSolutionObserver(new SolutionObserver() {
			public void solutionFound() {
				nbSol.incr();	
			}
		});
		search.solveAll();
        assertEquals(nbSol.getValue(),860);
    }
    

}

