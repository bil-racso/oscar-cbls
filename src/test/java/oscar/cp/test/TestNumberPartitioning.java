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
 * Problem 49 of CSPLib <br>
 * find a partition of numbers 1..N into two sets A and B such that: <br>
 * a) A and B have the same cardinality  <br>
 * b) sum of numbers in A = sum of numbers in B  <br>
 * c) sum of squares of numbers in A = sum of squares of numbers in B
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestNumberPartitioning extends TestCase {



    public TestNumberPartitioning(String name) {
        super(name);
    }

    /**
     *
     * @param n
     * @return  the number of solutions
     */
    public int testNumberPartitioning(int n) {
        Store cp = new Store();
        CPVarBool [] x = CPVarBool.getArray(cp, n);
        int [] values = new int[n];
        int [] values2 = new int[n];
        for (int i = 0; i < n; i++) {
            values[i] = i+1;
            values2[i] = values[i]*values[i];
        }

        cp.add(x[0].constraintTrue()); // break symmetries between the two partitions
        cp.add(new Sum(x,n/2)); // same cardinality

        // we add different versions of the BinaryKnapsack to test it well
        cp.add(new WeightedSum(values, x, ArrayUtils.sum(values)/2)); // sum of numbers in A = sum of numbers in B
        cp.add(new WeightedSum(values2, x, ArrayUtils.sum(values2)/2)); // sum of squares of numbers in A = sum of squares of numbers in B
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Weak);
        cp.add(new BinaryKnapsack(x,values, ArrayUtils.sum(values)/2), CPPropagStrength.Strong);
        cp.add(new BinaryKnapsack(x,values2,ArrayUtils.sum(values2)/2), CPPropagStrength.Strong);

        CPVarBool [] y = new CPVarBool[x.length];
        for (int i = 0; i < x.length; i++) {
            y[i] = x[x.length-i-1];
        }
        final Counter nbSol = new Counter();
        Search search = new Search(cp,new Binary(y),"");
		search.addSolutionObserver(new SolutionObserver() {
            public void solutionFound() {
                 nbSol.incr();
            }
        });
		search.solveAll();
        return nbSol.getValue();
    }

    public void test1() {
        assertEquals(testNumberPartitioning(8),1);
    }

    public void test2() {
        assertEquals(testNumberPartitioning(10),0);
    }

    public void test3() {
        assertEquals(testNumberPartitioning(12),1);
    }

    public void test4() {
        assertEquals(testNumberPartitioning(20),24);
    }
    

}

