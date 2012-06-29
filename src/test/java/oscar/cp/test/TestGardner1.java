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
public class TestGardner1 extends TestCase {

    int bestSol = Integer.MAX_VALUE;

    public TestGardner1(String name) {
        super(name);
    }


    public void testGardner(){
        // What is the minimum possible product of three different numbers of the set {-8,-6,-4,0,3,5,7}

        final Counter nbSol = new Counter();
    	Store cp = new Store();

        int [] values = new int[]{-8,-6,-4,0,3,5,7};
		CPVarInt [] x = new CPVarInt[]{new CPVarInt(cp,values),
                           new CPVarInt(cp,values),
                           new CPVarInt(cp,values)};
		cp.add(new AllDifferent(x));
        cp.add(new Le(x[0],x[1]));
        cp.add(new Le(x[1],x[2]));
        final CPVarInt obj = x[0].mul(x[1]).mul(x[2]);
        cp.minimization(x[0].mul(x[1]).mul(x[2]));

        Search search = new Search(cp,new Binary(x));
		search.addSolutionObserver(new SolutionObserver() {
            public void solutionFound() {
                bestSol = obj.getValue();
            }
        });
        search.solveAll();
        assertEquals(bestSol,-280);
    }
    

}

