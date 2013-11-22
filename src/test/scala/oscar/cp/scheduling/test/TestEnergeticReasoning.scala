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
package oscar.cp.scheduling.test


import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.modeling._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class TestEnergeticReasoning extends FunSuite with ShouldMatchers  {


  //example from Baptiste's book
  test("ER : Detect infeasibility") {
    //a task : (startmin, endmax, dur, demand)	
  
	val activitiesDescription = Array((1,8,4,1), (1,8,4,1), (0,10,4,1),(0,10,4,1),(0,10,4,1))
	
	val horizon = 3000
	
	val cp = CPScheduler(horizon)

	val starts = activitiesDescription.map(aD => CPVarInt(cp, aD._1 to aD._2 - aD._3))
	val durs = activitiesDescription.map(aD => CPVarInt(cp, aD._3, aD._3))
	val ends = activitiesDescription.map(aD => CPVarInt(cp, aD._1 + aD._3 to aD._2))
	val demands = activitiesDescription.map(aD => CPVarInt(cp, aD._4 to aD._4))
	
	val resId = 1
	
	val resources = Array.fill(activitiesDescription.length)(CPVarInt(cp, resId to resId)) 

	val capa = CPVarInt(cp, 2 to 2)
	
	var nb = 0
	cp.solve subjectTo
	{
		cp.add(new EnergeticReasoning(starts,durs,ends,demands,resources,capa,resId))
	} exploration {
			cp.binaryFirstFail(starts)
    	    nb += 1
    	} run()
    	
    nb should be(0)

  }  
  
  test("ER : Pruning") {
    //a task : (startmin, endmax, dur, demand)
  
    val activitiesDescription = Array((1,8,4,1), (0,10,4,1),(0,10,4,1))
	
	val horizon = 3000
	
	val cp = CPScheduler(horizon)

	val starts = activitiesDescription.map(aD => CPVarInt(cp, aD._1 to aD._2 - aD._3))
	val durs = activitiesDescription.map(aD => CPVarInt(cp, aD._3, aD._3))
	val ends = activitiesDescription.map(aD => CPVarInt(cp, aD._1 + aD._3 to aD._2))
	val demands = activitiesDescription.map(aD => CPVarInt(cp, aD._4 to aD._4))
	
	val resId = 1
	
	val resources = Array.fill(activitiesDescription.length)(CPVarInt(cp, resId to resId)) 

	val capa = CPVarInt(cp, 2 to 2)

	
	cp.add(starts(0) == 1)
	cp.add(starts(1) == 0)
	
	for(i <- 0 until starts.length) {
	  cp.add(starts(i) + durs(i) == ends(i))
	}
	
	cp.add(new EnergeticReasoning(starts,durs,ends,demands,resources,capa,resId))
    	
	starts(2).getMin should be(4)
	
	

  }  
  
}
