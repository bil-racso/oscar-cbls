/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._
import collection.immutable.SortedSet



/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestRostering extends FunSuite with ShouldMatchers  {


  test("Test Rostering") {
     	val  nbPersons = 5;
		val nbSlots = 6;
		val nbActivities = 10;

		val possibleActivities = Array(Set(0,1,2,3,4,5,7),
									   Set(0,2,3,6,8,9),
									   Set(1,3,4,7,8,9),
									   Set(0,2,4,5,6,7,8,9),
									   Set(0,3,4,6,7,9));	
		
		val demand = Array(Array(1,0,2,1,0,0,0,0,1,0),
						   Array(2,0,0,0,0,1,0,0,0,1),
						   Array(1,0,0,1,0,0,1,0,0,2),
						   Array(0,1,0,1,0,0,0,0,1,0),
						   Array(1,0,1,0,0,1,0,1,0,1),
						   Array(0,0,1,0,0,1,1,0,1,0));
		
		
		val cp = CPSolver()
    	
    	
    	val activities = Array.tabulate(nbPersons, nbSlots)((p,t) => CPVarInt(cp,possibleActivities(p)))
	
    	val underDemand = Array.tabulate(nbSlots)(t => CPVarInt(cp,0 to nbPersons))
    	
    	val totUnderDemand: CPVarInt = sum(underDemand)
    	var best = Int.MaxValue
    	cp.minimize(totUnderDemand) subjectTo {
    	  	// each person must do a different activity every day
		    for (p <- 0 until nbPersons) {
			  cp.add(allDifferent(activities(p)),Strong);
		    }
		    
		    val maxCap = Array.fill(nbActivities)(nbPersons)
		    for (t <- 0 until nbSlots) {
		      val act_t = Array.tabulate(nbPersons)(p => activities(p)(t))
		      cp.add(new oscar.cp.constraints.SoftGCC(act_t,0,demand(t),maxCap,totUnderDemand))
		    }
		
    	  
    	} exploration {
    	  println("exploration"+activities.flatten.mkString(","))
    	  val x = activities.flatten
    	  cp.binary(x,y => y.size)
    	  //println("solution======"+cp.isFailed()+" "+totUnderDemand)
    	  best = totUnderDemand.value
    	} run()
    	best should be(1)
		
		
    
  }  
  

  


}
