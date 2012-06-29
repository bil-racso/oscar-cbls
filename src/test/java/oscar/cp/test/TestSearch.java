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
public class TestSearch extends TestCase {

    int depth;


    public TestSearch(String name) {
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
    
    public void testSearch1(){
    	depth = 0;
        final Counter nbSol = new Counter();

    	ReversibleSearchNode node = new ReversibleSearchNode();

        Search search = new Search(node,new Branching() {
               public Alternative[] getAlternatives() {
                      if (depth == 3) return null;
                      Alternative left = new Alternative("left") {
                          public boolean execute() {
                              depth++;
                              return true;
                          }
                          @Override
                          public boolean executeOnBacktrack() {
                              depth--;
                              return true;
                          }
                      };
                      Alternative right = new Alternative("right") {
                          public boolean execute() {
                              depth++;
                              return true;
                          }
                          @Override
                          public boolean executeOnBacktrack() {
                              depth--;
                              return true;
                          }
                      };
                      return new Alternative[]{left,right};
               }
        });

        search.addSolutionObserver(new SolutionObserver() {
            public void solutionFound() {
                nbSol.incr();
            }
        });

        search.solveAll(); // explores all the leaf nodes a binary search of depth 3 => 8 solutions
        assertEquals(nbSol.getValue(),8);

        depth = 0;
        nbSol.reset();

        search.solveOne(); // explores all the leaf nodes a binary search of depth 3 => 8 solutions
        assertEquals(nbSol.getValue(),1);

        System.out.println(nbSol.getValue()); // explores only the left branch
    	

    }
    
    

}
