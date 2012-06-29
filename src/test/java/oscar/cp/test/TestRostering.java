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
public class TestRostering extends TestCase {

    public TestRostering(String name) {
        super(name);
        
    }


    public void testRostering() {
    	
    	int nbPersons = 5;
		int nbSlots = 6;
		int nbActivities = 10;

		int [][] possibleActivities = new int [][]  {{0,1,2,3,4,5,7},
													{0,2,3,6,8,9},
													{1,3,4,7,8,9},
													{0,2,4,5,6,7,8,9},
													{0,3,4,6,7,9}};	
		
		int [][] demand = new int [][]  {{1,0,2,1,0,0,0,0,1,0},
										{2,0,0,0,0,1,0,0,0,1},
										{1,0,0,1,0,0,1,0,0,2},
										{0,1,0,1,0,0,0,0,1,0},
										{1,0,1,0,0,1,0,1,0,1},
										{0,0,1,0,0,1,1,0,1,0}};
	
    	
		//   -----------------  model -----------------------
    	Store cp = new Store();
    	
    	
    	CPVarInt [][] activities = new CPVarInt [nbPersons][nbSlots];
		for (int p = 0; p < nbPersons; p++) {
			for (int t = 0; t < nbSlots; t++) {
				activities[p][t] = new CPVarInt(cp, possibleActivities[p]);
			}
		}
		
		CPVarInt [] underDemand = CPVarInt.getArray(cp, nbSlots, 0, nbPersons);
		
		final CPVarInt totUnderDemand = new CPVarInt(cp, 0,nbPersons*nbSlots);
		cp.add(new Sum(underDemand,totUnderDemand));
		
		// each person must do a different activity every day
		for (int p = 0; p < nbPersons; p++) {
			cp.add(new AllDifferent(activities[p]),CPPropagStrength.Strong);
		}
		
		int [] maxCap = new int[nbActivities];
		for (int a = 0; a < nbActivities; a++) {
			maxCap[a] = nbPersons;
		}
		
		for (int t = 0; t < nbSlots; t++) {
			CPVarInt [] act_t = ArrayUtils.getSlice(activities, t);
			// computes the shortage of demand at each time slot
			cp.add(new SoftGCC(act_t,0,demand[t],maxCap,totUnderDemand));
		}
		
		cp.minimization(totUnderDemand);    	
    	
		//   -----------------  search -----------------------
		final Int best = new Int(Integer.MAX_VALUE);
		Search search = new Search(cp,new NaryFirstFail(ArrayUtils.flattenvars(activities)));
		search.addSolutionObserver(new SolutionObserver() {			
			public void solutionFound() {
				best.setValue(totUnderDemand.getValue());
			}
		});

		search.solveAll();
		assertEquals(best.getValue(), 1);
    }

}

